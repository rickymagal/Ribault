/* Attention sequential baseline in C — monolithic single-thread end-to-end
 * transformer block. Same algorithm as the parallel ribault_c super body,
 * but run as one flat full-N kernel (no row-block decomposition). Reads
 * weights/input from DATA_DIR; prints CHECKSUM= and RUNTIME_SEC= for the
 * runner CSV. Cross-validates byte-for-byte against the numpy reference
 * checksum and against the parallel variants.
 *
 * Build:  gcc -O3 -march=native -o attn_seq_c attn_seq.c -lm
 * Run:    ./attn_seq_c DATA_DIR
 *
 * This is the C-tier denominator: every C parallel variant (ribault_c) is
 * measured against this baseline so the reported speedup isolates the
 * dataflow scheduler from the language compute backend.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <time.h>

static int N, D, N_HEADS, HEAD_DIM, D_FF, VOCAB, SEED;

static double *E, *PE, *W_Q, *W_K, *W_V, *W_O, *W_1, *W_2, *W_U;
static double *LN_1_w, *LN_1_b, *LN_2_w, *LN_2_b;
static uint8_t *input_tokens, *output_tokens;
static double *x_buf, *xa_buf, *Q, *K, *V, *attn, *xb_buf, *ffn_h, *logits;

static void *xmalloc(size_t n) {
    void *p = malloc(n);
    if (!p) { fprintf(stderr, "OOM %zu\n", n); exit(1); }
    return p;
}

static void read_bin_d(const char *dir, const char *name, double *dst, size_t count) {
    char path[1024]; snprintf(path, sizeof path, "%s/%s", dir, name);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    if (fread(dst, sizeof(double), count, f) != count) {
        fprintf(stderr, "short read %s\n", path); exit(1);
    }
    fclose(f);
}

static void read_bin_u8(const char *dir, const char *name, uint8_t *dst, size_t count) {
    char path[1024]; snprintf(path, sizeof path, "%s/%s", dir, name);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    if (fread(dst, 1, count, f) != count) {
        fprintf(stderr, "short read %s\n", path); exit(1);
    }
    fclose(f);
}

static void load_config(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/config.txt", dir);
    FILE *f = fopen(path, "r"); if (!f) { perror(path); exit(1); }
    char k[64]; int v;
    while (fscanf(f, "%63s %d", k, &v) == 2) {
        if      (!strcmp(k, "N"))        N        = v;
        else if (!strcmp(k, "D"))        D        = v;
        else if (!strcmp(k, "N_HEADS"))  N_HEADS  = v;
        else if (!strcmp(k, "HEAD_DIM")) HEAD_DIM = v;
        else if (!strcmp(k, "D_FF"))     D_FF     = v;
        else if (!strcmp(k, "VOCAB"))    VOCAB    = v;
        else if (!strcmp(k, "SEED"))     SEED     = v;
    }
    fclose(f);
}

static void matmul(const double *A, const double *B, double *C,
                   int M, int K_dim, int N_dim) {
    /* C[M,N_dim] = A[M,K_dim] @ B[K_dim,N_dim] */
    for (int m = 0; m < M; m++) {
        const double *Arow = A + (size_t)m * K_dim;
        double *Crow = C + (size_t)m * N_dim;
        memset(Crow, 0, (size_t)N_dim * sizeof(double));
        for (int k = 0; k < K_dim; k++) {
            double a = Arow[k];
            const double *Brow = B + (size_t)k * N_dim;
            for (int n = 0; n < N_dim; n++) Crow[n] += a * Brow[n];
        }
    }
}

static void layer_norm(const double *x, const double *w, const double *b,
                       double *out, int rows, int dim) {
    const double eps = 1e-5;
    for (int i = 0; i < rows; i++) {
        const double *xr = x + (size_t)i * dim;
        double *or_ = out + (size_t)i * dim;
        double mean = 0.0;
        for (int j = 0; j < dim; j++) mean += xr[j];
        mean /= (double)dim;
        double var = 0.0;
        for (int j = 0; j < dim; j++) { double d = xr[j] - mean; var += d * d; }
        var /= (double)dim;
        double inv = 1.0 / sqrt(var + eps);
        for (int j = 0; j < dim; j++) or_[j] = (xr[j] - mean) * inv * w[j] + b[j];
    }
}

static void multihead_attention(void) {
    /* per-row scratch for scores (size N) */
    double *scores = xmalloc((size_t)N * sizeof(double));
    double inv = 1.0 / sqrt((double)HEAD_DIM);
    memset(attn, 0, (size_t)N * D * sizeof(double));
    for (int i = 0; i < N; i++) {
        for (int h = 0; h < N_HEADS; h++) {
            const double *Qhi = Q + (size_t)i * D + h * HEAD_DIM;
            for (int j = 0; j < N; j++) {
                const double *Khj = K + (size_t)j * D + h * HEAD_DIM;
                double s = 0.0;
                for (int k = 0; k < HEAD_DIM; k++) s += Qhi[k] * Khj[k];
                scores[j] = s * inv;
            }
            double m = scores[0];
            for (int j = 1; j < N; j++) if (scores[j] > m) m = scores[j];
            double sum = 0.0;
            for (int j = 0; j < N; j++) { scores[j] = exp(scores[j] - m); sum += scores[j]; }
            for (int j = 0; j < N; j++) scores[j] /= sum;
            double *ahi = attn + (size_t)i * D + h * HEAD_DIM;
            for (int j = 0; j < N; j++) {
                double a = scores[j];
                const double *Vhj = V + (size_t)j * D + h * HEAD_DIM;
                for (int k = 0; k < HEAD_DIM; k++) ahi[k] += a * Vhj[k];
            }
        }
    }
    free(scores);
}

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s DATA_DIR\n", argv[0]); return 2; }
    const char *dir = argv[1];
    load_config(dir);

    E      = xmalloc((size_t)VOCAB * D * sizeof(double)); read_bin_d(dir, "E.bin", E, (size_t)VOCAB * D);
    PE     = xmalloc((size_t)N * D * sizeof(double));     read_bin_d(dir, "PE.bin", PE, (size_t)N * D);
    W_Q    = xmalloc((size_t)D * D * sizeof(double));     read_bin_d(dir, "W_Q.bin", W_Q, (size_t)D * D);
    W_K    = xmalloc((size_t)D * D * sizeof(double));     read_bin_d(dir, "W_K.bin", W_K, (size_t)D * D);
    W_V    = xmalloc((size_t)D * D * sizeof(double));     read_bin_d(dir, "W_V.bin", W_V, (size_t)D * D);
    W_O    = xmalloc((size_t)D * D * sizeof(double));     read_bin_d(dir, "W_O.bin", W_O, (size_t)D * D);
    W_1    = xmalloc((size_t)D * D_FF * sizeof(double));  read_bin_d(dir, "W_1.bin", W_1, (size_t)D * D_FF);
    W_2    = xmalloc((size_t)D_FF * D * sizeof(double));  read_bin_d(dir, "W_2.bin", W_2, (size_t)D_FF * D);
    W_U    = xmalloc((size_t)D * VOCAB * sizeof(double)); read_bin_d(dir, "W_U.bin", W_U, (size_t)D * VOCAB);
    LN_1_w = xmalloc((size_t)D * sizeof(double));         read_bin_d(dir, "LN_1_w.bin", LN_1_w, D);
    LN_1_b = xmalloc((size_t)D * sizeof(double));         read_bin_d(dir, "LN_1_b.bin", LN_1_b, D);
    LN_2_w = xmalloc((size_t)D * sizeof(double));         read_bin_d(dir, "LN_2_w.bin", LN_2_w, D);
    LN_2_b = xmalloc((size_t)D * sizeof(double));         read_bin_d(dir, "LN_2_b.bin", LN_2_b, D);
    input_tokens = xmalloc(N); read_bin_u8(dir, "input_tokens.bin", input_tokens, N);

    x_buf  = xmalloc((size_t)N * D * sizeof(double));
    xa_buf = xmalloc((size_t)N * D * sizeof(double));
    Q      = xmalloc((size_t)N * D * sizeof(double));
    K      = xmalloc((size_t)N * D * sizeof(double));
    V      = xmalloc((size_t)N * D * sizeof(double));
    attn   = xmalloc((size_t)N * D * sizeof(double));
    xb_buf = xmalloc((size_t)N * D * sizeof(double));
    ffn_h  = xmalloc((size_t)N * D_FF * sizeof(double));
    logits = xmalloc((size_t)N * VOCAB * sizeof(double));
    output_tokens = xmalloc(N);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    /* embed + sinusoidal pos */
    for (int i = 0; i < N; i++) {
        const double *Ei = E + (size_t)input_tokens[i] * D;
        const double *Pi = PE + (size_t)i * D;
        double *xi = x_buf + (size_t)i * D;
        for (int j = 0; j < D; j++) xi[j] = Ei[j] + Pi[j];
    }

    /* LN_1 */
    layer_norm(x_buf, LN_1_w, LN_1_b, xa_buf, N, D);

    /* Q/K/V projections */
    matmul(xa_buf, W_Q, Q, N, D, D);
    matmul(xa_buf, W_K, K, N, D, D);
    matmul(xa_buf, W_V, V, N, D, D);

    /* multi-head attention -> attn[N, D] */
    multihead_attention();

    /* attn @ W_O + residual */
    matmul(attn, W_O, xa_buf, N, D, D);
    for (size_t i = 0; i < (size_t)N * D; i++) x_buf[i] += xa_buf[i];

    /* LN_2 */
    layer_norm(x_buf, LN_2_w, LN_2_b, xb_buf, N, D);

    /* FFN: x_b @ W_1 -> ReLU -> @ W_2 + residual */
    matmul(xb_buf, W_1, ffn_h, N, D, D_FF);
    for (size_t i = 0; i < (size_t)N * D_FF; i++) if (ffn_h[i] < 0.0) ffn_h[i] = 0.0;
    matmul(ffn_h, W_2, xa_buf, N, D_FF, D);
    for (size_t i = 0; i < (size_t)N * D; i++) x_buf[i] += xa_buf[i];

    /* unembed + argmax */
    matmul(x_buf, W_U, logits, N, D, VOCAB);
    for (int i = 0; i < N; i++) {
        const double *l = logits + (size_t)i * VOCAB;
        int best = 0;
        double bv = l[0];
        for (int v = 1; v < VOCAB; v++) if (l[v] > bv) { bv = l[v]; best = v; }
        output_tokens[i] = (uint8_t)best;
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double secs = (double)(t1.tv_sec - t0.tv_sec) + (double)(t1.tv_nsec - t0.tv_nsec) / 1e9;

    /* checksum */
    uint64_t cs = 0;
    for (int i = 0; i < N; i++) cs = (cs + (uint64_t)(i + 1) * (uint64_t)output_tokens[i]) & 0xFFFFFFFFu;
    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    printf("RUNTIME_SEC=%f\n", secs);

    /* persist output_tokens.bin (for cross-validation) */
    char out[1024]; snprintf(out, sizeof out, "%s/output_tokens.bin", dir);
    FILE *of = fopen(out, "wb"); if (of) { fwrite(output_tokens, 1, N, of); fclose(of); }

    return 0;
}
