/* Dense block Cholesky sequential baseline in C.
 *
 * Same algorithm as the parallel variants: process ops from dag.bin in
 * topological order, each op being one of POTRF / TRSM / SYRK / GEMM.
 * Block storage: row-major lower-triangular, block (i, j) for i >= j at
 * block_idx(i, j) = i*(i+1)/2 + j.
 *
 * Build:  gcc -O3 -march=native -o sc_seq_c sc_seq.c -lm
 * Run:    ./sc_seq_c DATA_DIR
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <time.h>

static int NB, B, SEED;

static double *A;       /* block storage [n_blocks * B * B] */
static int   *ops;      /* DAG: each op is 9 ints + deps */

static void *xmalloc(size_t n) { void *p = malloc(n); if (!p) { fprintf(stderr, "OOM %zu\n", n); exit(1); } return p; }

static inline int block_idx(int i, int j) { return i * (i + 1) / 2 + j; }
static inline double *block_ptr(int i, int j) { return A + (size_t)block_idx(i, j) * B * B; }

static void load_config(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/config.txt", dir);
    FILE *f = fopen(path, "r"); if (!f) { perror(path); exit(1); }
    char k[64]; int v;
    while (fscanf(f, "%63s %d", k, &v) == 2) {
        if      (!strcmp(k, "NB"))  NB  = v;
        else if (!strcmp(k, "B"))   B   = v;
        else if (!strcmp(k, "SEED")) SEED = v;
    }
    fclose(f);
}

static void load_A(const char *dir) {
    int n_blocks = NB * (NB + 1) / 2;
    size_t nbytes = (size_t)n_blocks * B * B * sizeof(double);
    A = xmalloc(nbytes);
    char path[1024]; snprintf(path, sizeof path, "%s/A.bin", dir);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    if (fread(A, 1, nbytes, f) != nbytes) { fprintf(stderr, "short read A.bin\n"); exit(1); }
    fclose(f);
}

/* POTRF: A[k,k] = chol_lower(A[k,k]).  In-place, lower-triangular only. */
static void potrf_block(double *D, int n) {
    for (int j = 0; j < n; j++) {
        double s = D[j*n + j];
        for (int kk = 0; kk < j; kk++) s -= D[j*n + kk] * D[j*n + kk];
        if (s <= 0.0) { fprintf(stderr, "POTRF: non-SPD diag %g at j=%d\n", s, j); exit(1); }
        D[j*n + j] = sqrt(s);
        double inv = 1.0 / D[j*n + j];
        for (int i = j + 1; i < n; i++) {
            double t = D[i*n + j];
            for (int kk = 0; kk < j; kk++) t -= D[i*n + kk] * D[j*n + kk];
            D[i*n + j] = t * inv;
        }
    }
    /* Zero upper triangle for cleanliness (checksum stability) */
    for (int i = 0; i < n; i++)
        for (int j = i + 1; j < n; j++)
            D[i*n + j] = 0.0;
}

/* TRSM: X = X * L^-T  where L is lower-triangular (X is B×B, L is B×B).
 *   For each row i of X, solve L^T * y = x_i, write y back to x_i.
 *   Equivalent to: for each i, for j = n-1 down to 0:
 *     X[i,j] = (X[i,j] - sum_{k>j} X[i,k] * L[k,j]) / L[j,j]   (using L lower-tri)
 */
static void trsm_right_lower_trans(double *X, const double *L, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            double s = X[i*n + j];
            for (int kk = 0; kk < j; kk++) s -= X[i*n + kk] * L[j*n + kk];
            X[i*n + j] = s / L[j*n + j];
        }
    }
}

/* SYRK: C -= A * A^T   (C is B×B symmetric, only lower-tri actually used).
 *   For lower-tri only (i >= j): C[i,j] -= sum_k A[i,k] * A[j,k]
 */
static void syrk_block(double *C, const double *A_, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j <= i; j++) {
            double s = 0.0;
            for (int kk = 0; kk < n; kk++) s += A_[i*n + kk] * A_[j*n + kk];
            C[i*n + j] -= s;
        }
    }
}

/* GEMM: C -= A * B^T   (all B×B). */
static void gemm_block(double *C, const double *A_, const double *B_, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            double s = 0.0;
            for (int kk = 0; kk < n; kk++) s += A_[i*n + kk] * B_[j*n + kk];
            C[i*n + j] -= s;
        }
    }
}

/* Process DAG ops in order. Each op is encoded as:
 *   kind (int), ti, tj, s1i, s1j, s2i, s2j, level, n_deps, [dep_op_ids...]
 * We can ignore deps in sequential — process in order. */
static void process_dag(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/dag.bin", dir);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    int n_ops;
    if (fread(&n_ops, sizeof(int), 1, f) != 1) { fprintf(stderr, "short read dag.bin hdr\n"); exit(1); }

    for (int op = 0; op < n_ops; op++) {
        int hdr[9];
        if (fread(hdr, sizeof(int), 9, f) != 9) { fprintf(stderr, "short read op %d\n", op); exit(1); }
        int kind = hdr[0], ti = hdr[1], tj = hdr[2];
        int s1i = hdr[3], s1j = hdr[4], s2i = hdr[5], s2j = hdr[6];
        int n_deps = hdr[8];
        if (n_deps > 0) {
            int dep_buf[64];
            if (n_deps > 64) { fprintf(stderr, "deps overflow %d\n", n_deps); exit(1); }
            if (fread(dep_buf, sizeof(int), n_deps, f) != (size_t)n_deps) { fprintf(stderr, "short read deps op %d\n", op); exit(1); }
        }

        double *T = block_ptr(ti, tj);
        switch (kind) {
            case 0: /* POTRF */
                potrf_block(T, B);
                break;
            case 1: /* TRSM: X = X · L^-T where L is L[s1i,s1j] = L[k,k] */
                trsm_right_lower_trans(T, block_ptr(s1i, s1j), B);
                break;
            case 2: /* SYRK: A[k,k] -= A[k,j] · A[k,j]^T  ;  s1 = (k, j) */
                syrk_block(T, block_ptr(s1i, s1j), B);
                break;
            case 3: /* GEMM: A[i,k] -= A[i,j] · A[k,j]^T ; s1=(i,j), s2=(k,j) */
                gemm_block(T, block_ptr(s1i, s1j), block_ptr(s2i, s2j), B);
                break;
            default:
                fprintf(stderr, "unknown op kind %d at op=%d\n", kind, op); exit(1);
        }
    }
    fclose(f);
}

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s DATA_DIR\n", argv[0]); return 2; }
    const char *dir = argv[1];
    load_config(dir);
    load_A(dir);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    process_dag(dir);
    clock_gettime(CLOCK_MONOTONIC, &t1);
    double secs = (double)(t1.tv_sec - t0.tv_sec) + (double)(t1.tv_nsec - t0.tv_nsec) / 1e9;

    /* Compute checksum: sum of all elements in lower-tri blocks, as int(v * 1e6) mod 2^32 */
    int n_blocks = NB * (NB + 1) / 2;
    uint64_t cs = 0;
    for (size_t k = 0; k < (size_t)n_blocks * B * B; k++) {
        int64_t fixed = (int64_t)(A[k] * 1e6);
        cs = (cs + (uint64_t)((uint32_t)fixed)) & 0xFFFFFFFFu;
    }
    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    printf("RUNTIME_SEC=%f\n", secs);
    free(A);
    return 0;
}
