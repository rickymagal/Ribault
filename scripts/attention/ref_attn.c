/* Reference single-threaded C implementation of one transformer block
 * end-to-end. Reads weights/input from data dir, writes output_tokens.bin,
 * and prints CHECKSUM= on stdout. Used as ground truth — every parallel
 * variant must match its output exactly (or at least its checksum).
 *
 * Algorithm: see gen_attn_data.py docstring. Same math, single-threaded.
 * Build:   gcc -O3 -march=native -o ref_attn ref_attn.c -lm
 * Run:     ./ref_attn DATA_DIR
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

static int N, D, N_HEADS, HEAD_DIM, D_FF, VOCAB, SEED;

static double *E, *PE, *W_Q, *W_K, *W_V, *W_O, *W_1, *W_2, *W_U;
static double *LN_1_w, *LN_1_b, *LN_2_w, *LN_2_b;
static uint8_t *input_tokens, *output_tokens;
/* activations */
static double *x_buf, *xa_buf, *Q, *K, *V, *attn, *xb_buf, *ffn_h, *logits;

static void *xmalloc(size_t n) { void *p = malloc(n); if (!p) { fprintf(stderr, "OOM %zu\n", n); exit(1); } return p; }

static void read_bin_d(const char *dir, const char *name, double *dst, size_t count) {
    char path[1024]; snprintf(path, sizeof path, "%s/%s", dir, name);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    size_t r = fread(dst, sizeof(double), count, f);
    if (r != count) { fprintf(stderr, "short read %s: %zu/%zu\n", path, r, count); exit(1); }
    fclose(f);
}
static void read_bin_u8(const char *dir, const char *name, uint8_t *dst, size_t count) {
    char path[1024]; snprintf(path, sizeof path, "%s/%s", dir, name);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    size_t r = fread(dst, 1, count, f);
    if (r != count) { fprintf(stderr, "short read %s: %zu/%zu\n", path, r, count); exit(1); }
    fclose(f);
}

static void load_config(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/config.txt", dir);
    FILE *f = fopen(path, "r"); if (!f) { perror(path); exit(1); }
    char k[64]; int v;
    while (fscanf(f, "%63s %d", k, &v) == 2) {
        if (!strcmp(k, "N")) N = v;
        else if (!strcmp(k, "D")) D = v;
        else if (!strcmp(k, "N_HEADS")) N_HEADS = v;
        else if (!strcmp(k, "HEAD_DIM")) HEAD_DIM = v;
        else if (!strcmp(k, "D_FF")) D_FF = v;
        else if (!strcmp(k, "VOCAB")) VOCAB = v;
        else if (!strcmp(k, "SEED")) SEED = v;
    }
    fclose(f);
}

static void matmul(const double *A, const double *B, double *C, int M, int K_dim, int N_dim) {
    /* C[M,N] = A[M,K] @ B[K,N] */
    for (int m = 0; m < M; m++) {
        const double *Arow = A + m * K_dim;
        double *Crow = C + m * N_dim;
        memset(Crow, 0, N_dim * sizeof(double));
        for (int k = 0; k < K_dim; k++) {
            double a = Arow[k];
            const double *Brow = B + k * N_dim;
            for (int n = 0; n < N_dim; n++) Crow[n] += a * Brow[n];
        }
    }
}

static void layer_norm(const double *x, const double *w, const double *b, double *out, int rows, int dim) {
    const double eps = 1e-5;
    for (int i = 0; i < rows; i++) {
        const double *xr = x + i * dim;
        double *or_ = out + i * dim;
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
    /* Q/K/V are (N, D). Treat as (n_heads, N, head_dim) per-head. */
    /* For each head h: */
    /*   scores[i,j] = Q[h][i] . K[h][j] / sqrt(head_dim)   (N x N) */
    /*   A[i,j] = softmax_j(scores[i,j])                    (per row) */
    /*   attn[h][i] = sum_j A[i,j] * V[h][j]                (head_dim) */
    /* Then concat heads back to (N, D) and store into `attn`. */
    double inv = 1.0 / sqrt((double)HEAD_DIM);
    double *scores = xmalloc((size_t)N * N * sizeof(double));
    memset(attn, 0, (size_t)N * D * sizeof(double));
    for (int h = 0; h < N_HEADS; h++) {
        /* For head h, slice Q/K/V columns [h*HEAD_DIM, (h+1)*HEAD_DIM). */
        for (int i = 0; i < N; i++) {
            const double *Qhi = Q + i * D + h * HEAD_DIM;
            double *srow = scores + i * N;
            for (int j = 0; j < N; j++) {
                const double *Khj = K + j * D + h * HEAD_DIM;
                double s = 0.0;
                for (int k = 0; k < HEAD_DIM; k++) s += Qhi[k] * Khj[k];
                srow[j] = s * inv;
            }
            /* softmax row i */
            double m = srow[0];
            for (int j = 1; j < N; j++) if (srow[j] > m) m = srow[j];
            double sum = 0.0;
            for (int j = 0; j < N; j++) { srow[j] = exp(srow[j] - m); sum += srow[j]; }
            for (int j = 0; j < N; j++) srow[j] /= sum;
            /* weighted sum of V_h */
            double *ahi = attn + i * D + h * HEAD_DIM;
            for (int j = 0; j < N; j++) {
                double a = srow[j];
                const double *Vhj = V + j * D + h * HEAD_DIM;
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
    fprintf(stderr, "N=%d D=%d HEADS=%d HEAD_DIM=%d D_FF=%d VOCAB=%d\n",
            N, D, N_HEADS, HEAD_DIM, D_FF, VOCAB);

    E       = xmalloc((size_t)VOCAB * D * sizeof(double)); read_bin_d(dir, "E.bin", E, (size_t)VOCAB * D);
    PE      = xmalloc((size_t)N * D * sizeof(double));     read_bin_d(dir, "PE.bin", PE, (size_t)N * D);
    W_Q     = xmalloc((size_t)D * D * sizeof(double));     read_bin_d(dir, "W_Q.bin", W_Q, (size_t)D * D);
    W_K     = xmalloc((size_t)D * D * sizeof(double));     read_bin_d(dir, "W_K.bin", W_K, (size_t)D * D);
    W_V     = xmalloc((size_t)D * D * sizeof(double));     read_bin_d(dir, "W_V.bin", W_V, (size_t)D * D);
    W_O     = xmalloc((size_t)D * D * sizeof(double));     read_bin_d(dir, "W_O.bin", W_O, (size_t)D * D);
    W_1     = xmalloc((size_t)D * D_FF * sizeof(double));  read_bin_d(dir, "W_1.bin", W_1, (size_t)D * D_FF);
    W_2     = xmalloc((size_t)D_FF * D * sizeof(double));  read_bin_d(dir, "W_2.bin", W_2, (size_t)D_FF * D);
    W_U     = xmalloc((size_t)D * VOCAB * sizeof(double)); read_bin_d(dir, "W_U.bin", W_U, (size_t)D * VOCAB);
    LN_1_w  = xmalloc((size_t)D * sizeof(double));         read_bin_d(dir, "LN_1_w.bin", LN_1_w, D);
    LN_1_b  = xmalloc((size_t)D * sizeof(double));         read_bin_d(dir, "LN_1_b.bin", LN_1_b, D);
    LN_2_w  = xmalloc((size_t)D * sizeof(double));         read_bin_d(dir, "LN_2_w.bin", LN_2_w, D);
    LN_2_b  = xmalloc((size_t)D * sizeof(double));         read_bin_d(dir, "LN_2_b.bin", LN_2_b, D);
    input_tokens  = xmalloc(N); read_bin_u8(dir, "input_tokens.bin", input_tokens, N);

    /* activations */
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

    /* embed + pos */
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

    /* Multi-head attention -> writes into `attn` */
    multihead_attention();

    /* attn @ W_O, then add to x (residual) */
    matmul(attn, W_O, xa_buf, N, D, D);   /* reuse xa_buf */
    for (int i = 0; i < N * D; i++) x_buf[i] += xa_buf[i];

    /* LN_2 */
    layer_norm(x_buf, LN_2_w, LN_2_b, xb_buf, N, D);

    /* FFN: x_b @ W_1 -> ReLU -> @ W_2 -> add to x (residual) */
    matmul(xb_buf, W_1, ffn_h, N, D, D_FF);
    for (int i = 0; i < N * D_FF; i++) if (ffn_h[i] < 0.0) ffn_h[i] = 0.0;
    matmul(ffn_h, W_2, xa_buf, N, D_FF, D);  /* reuse xa_buf */
    for (int i = 0; i < N * D; i++) x_buf[i] += xa_buf[i];

    /* Unembed + argmax */
    matmul(x_buf, W_U, logits, N, D, VOCAB);
    for (int i = 0; i < N; i++) {
        const double *l = logits + (size_t)i * VOCAB;
        int best = 0;
        double bv = l[0];
        for (int v = 1; v < VOCAB; v++) if (l[v] > bv) { bv = l[v]; best = v; }
        output_tokens[i] = (uint8_t)best;
    }

    /* checksum */
    uint64_t cs = 0;
    for (int i = 0; i < N; i++) cs = (cs + (uint64_t)(i + 1) * (uint64_t)output_tokens[i]) & 0xFFFFFFFFu;
    printf("CHECKSUM=%lu\n", (unsigned long)cs);

    /* write output_tokens.bin for cross-validation */
    char out[1024]; snprintf(out, sizeof out, "%s/output_tokens.bin", dir);
    FILE *of = fopen(out, "wb"); fwrite(output_tokens, 1, N, of); fclose(of);

    return 0;
}
