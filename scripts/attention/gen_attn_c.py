#!/usr/bin/env python3
"""Generate Ribault attention files with C-implemented supers (no Haskell leaf).

Emits:
  1. attn.fl        — dataflow graph with 5 super types:
                       init (load weights), phaseA (per-block Q/K/V),
                       barrier (K-way fan-in), phaseB (per-block attn+FFN+argmax),
                       result (final checksum)
  2. attn_c_supers.c — C implementation of all five with constants baked in
                       (N, D, N_HEADS, etc.). Reads data_dir at runtime.

Same data files as gen_attn_data.py: input_tokens.bin, E.bin, PE.bin,
W_Q.bin, W_K.bin, W_V.bin, W_O.bin, W_1.bin, W_2.bin, W_U.bin,
LN_{1,2}_{w,b}.bin. Writes output_tokens.bin and prints CHECKSUM=.

Block scheme: N rows split into K = n_blocks blocks of N/K rows each
(last block absorbs remainder). Phase A independent per block, phase B
needs ALL Q/K/V (gated by barrier super). Final result fans in all K
phaseB done-signals.
"""

import argparse, os


SUPER_INIT    = 13
SUPER_PHASE_A = 12
SUPER_BARRIER = 14
SUPER_PHASE_B = 11
SUPER_RESULT  = 10


C_TEMPLATE = r"""/* Auto-generated: attention C supers. Constants baked in. */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define N         __N__
#define D         __D__
#define N_HEADS   __N_HEADS__
#define HEAD_DIM  __HEAD_DIM__
#define D_FF      __D_FF__
#define VOCAB     __VOCAB__
#define N_BLOCKS  __N_BLOCKS__

static const char *DATA_DIR = "__DATA_DIR__";

static double *E, *PE, *W_Q, *W_K, *W_V, *W_O, *W_1, *W_2, *W_U;
static double *LN_1_w, *LN_1_b, *LN_2_w, *LN_2_b;
static double *x_buf, *xa_buf, *Q, *K, *V, *attn_buf, *xb_buf, *ffn_h, *logits_buf;
static uint8_t *input_tokens, *output_tokens;

static void *xmalloc(size_t n) { void *p = malloc(n); if (!p) { fprintf(stderr, "OOM %zu\n", n); exit(1); } return p; }
static void read_bin_d(const char *name, double *dst, size_t count) {
    char path[1024]; snprintf(path, sizeof path, "%s/%s", DATA_DIR, name);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    if (fread(dst, sizeof(double), count, f) != count) { fprintf(stderr, "short read %s\n", path); exit(1); }
    fclose(f);
}
static void read_bin_u8(const char *name, uint8_t *dst, size_t count) {
    char path[1024]; snprintf(path, sizeof path, "%s/%s", DATA_DIR, name);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    if (fread(dst, 1, count, f) != count) { fprintf(stderr, "short read %s\n", path); exit(1); }
    fclose(f);
}

static int row_lo(int b) { return b * N / N_BLOCKS; }
static int row_hi(int b) { return (b == N_BLOCKS - 1) ? N : (b + 1) * N / N_BLOCKS; }

static void matmul_block(int row_lo_, int row_hi_, const double *A, const double *B,
                         double *C, int K_dim, int N_dim) {
    for (int m = row_lo_; m < row_hi_; m++) {
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

static void layer_norm_block(int row_lo_, int row_hi_, const double *x, const double *w,
                              const double *b, double *out, int dim) {
    const double eps = 1e-5;
    for (int i = row_lo_; i < row_hi_; i++) {
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

static int64_t s_init(void) {
    E       = xmalloc((size_t)VOCAB * D * sizeof(double)); read_bin_d("E.bin", E, (size_t)VOCAB * D);
    PE      = xmalloc((size_t)N * D * sizeof(double));     read_bin_d("PE.bin", PE, (size_t)N * D);
    W_Q     = xmalloc((size_t)D * D * sizeof(double));     read_bin_d("W_Q.bin", W_Q, (size_t)D * D);
    W_K     = xmalloc((size_t)D * D * sizeof(double));     read_bin_d("W_K.bin", W_K, (size_t)D * D);
    W_V     = xmalloc((size_t)D * D * sizeof(double));     read_bin_d("W_V.bin", W_V, (size_t)D * D);
    W_O     = xmalloc((size_t)D * D * sizeof(double));     read_bin_d("W_O.bin", W_O, (size_t)D * D);
    W_1     = xmalloc((size_t)D * D_FF * sizeof(double));  read_bin_d("W_1.bin", W_1, (size_t)D * D_FF);
    W_2     = xmalloc((size_t)D_FF * D * sizeof(double));  read_bin_d("W_2.bin", W_2, (size_t)D_FF * D);
    W_U     = xmalloc((size_t)D * VOCAB * sizeof(double)); read_bin_d("W_U.bin", W_U, (size_t)D * VOCAB);
    LN_1_w  = xmalloc((size_t)D * sizeof(double));         read_bin_d("LN_1_w.bin", LN_1_w, D);
    LN_1_b  = xmalloc((size_t)D * sizeof(double));         read_bin_d("LN_1_b.bin", LN_1_b, D);
    LN_2_w  = xmalloc((size_t)D * sizeof(double));         read_bin_d("LN_2_w.bin", LN_2_w, D);
    LN_2_b  = xmalloc((size_t)D * sizeof(double));         read_bin_d("LN_2_b.bin", LN_2_b, D);
    input_tokens  = xmalloc(N); read_bin_u8("input_tokens.bin", input_tokens, N);
    output_tokens = xmalloc(N);

    x_buf   = xmalloc((size_t)N * D * sizeof(double));
    xa_buf  = xmalloc((size_t)N * D * sizeof(double));
    Q       = xmalloc((size_t)N * D * sizeof(double));
    K       = xmalloc((size_t)N * D * sizeof(double));
    V       = xmalloc((size_t)N * D * sizeof(double));
    attn_buf= xmalloc((size_t)N * D * sizeof(double));
    xb_buf  = xmalloc((size_t)N * D * sizeof(double));
    ffn_h   = xmalloc((size_t)N * D_FF * sizeof(double));
    logits_buf = xmalloc((size_t)N * VOCAB * sizeof(double));
    return 0;
}

/* Phase A: per-block compute embed + LN_1 + Q/K/V projections for its rows. */
static int64_t s_phaseA(int64_t block_idx) {
    int b = (int)block_idx;
    int lo = row_lo(b), hi = row_hi(b);
    /* embed + pos */
    for (int i = lo; i < hi; i++) {
        const double *Ei = E + (size_t)input_tokens[i] * D;
        const double *Pi = PE + (size_t)i * D;
        double *xi = x_buf + (size_t)i * D;
        for (int j = 0; j < D; j++) xi[j] = Ei[j] + Pi[j];
    }
    /* LN_1 -> xa_buf */
    layer_norm_block(lo, hi, x_buf, LN_1_w, LN_1_b, xa_buf, D);
    /* Q/K/V projections */
    matmul_block(lo, hi, xa_buf, W_Q, Q, D, D);
    matmul_block(lo, hi, xa_buf, W_K, K, D, D);
    matmul_block(lo, hi, xa_buf, W_V, V, D, D);
    return 0;
}

/* Phase B: per-block multi-head attention + residual + LN_2 + FFN + residual + unembed + argmax. */
static int64_t s_phaseB(int64_t block_idx) {
    int b = (int)block_idx;
    int lo = row_lo(b), hi = row_hi(b);
    double inv = 1.0 / sqrt((double)HEAD_DIM);
    /* per-row scratch for scores (size N) */
    double *scores = xmalloc((size_t)N * sizeof(double));
    /* For each row i in [lo, hi): for each head, compute scores then weighted V. */
    /* attn_buf[i, :] = concat heads. Zero it first. */
    memset(attn_buf + (size_t)lo * D, 0, (size_t)(hi - lo) * D * sizeof(double));
    for (int i = lo; i < hi; i++) {
        for (int h = 0; h < N_HEADS; h++) {
            const double *Qhi = Q + (size_t)i * D + h * HEAD_DIM;
            /* scores[j] = (Q_h[i] . K_h[j]) * inv */
            for (int j = 0; j < N; j++) {
                const double *Khj = K + (size_t)j * D + h * HEAD_DIM;
                double s = 0.0;
                for (int k = 0; k < HEAD_DIM; k++) s += Qhi[k] * Khj[k];
                scores[j] = s * inv;
            }
            /* softmax */
            double m = scores[0];
            for (int j = 1; j < N; j++) if (scores[j] > m) m = scores[j];
            double sum = 0.0;
            for (int j = 0; j < N; j++) { scores[j] = exp(scores[j] - m); sum += scores[j]; }
            for (int j = 0; j < N; j++) scores[j] /= sum;
            /* attn_h[i, :] = sum_j scores[j] * V_h[j, :] */
            double *ahi = attn_buf + (size_t)i * D + h * HEAD_DIM;
            for (int j = 0; j < N; j++) {
                double a = scores[j];
                const double *Vhj = V + (size_t)j * D + h * HEAD_DIM;
                for (int k = 0; k < HEAD_DIM; k++) ahi[k] += a * Vhj[k];
            }
        }
    }
    free(scores);
    /* Output projection: attn @ W_O -> xa_buf (we can clobber xa_buf now). */
    matmul_block(lo, hi, attn_buf, W_O, xa_buf, D, D);
    /* x += xa_buf (residual) */
    for (int i = lo; i < hi; i++) {
        double *xi = x_buf + (size_t)i * D;
        const double *yi = xa_buf + (size_t)i * D;
        for (int j = 0; j < D; j++) xi[j] += yi[j];
    }
    /* LN_2 -> xb_buf */
    layer_norm_block(lo, hi, x_buf, LN_2_w, LN_2_b, xb_buf, D);
    /* FFN: xb @ W_1 -> ffn_h, ReLU, @ W_2 -> xa_buf, then x += xa_buf */
    matmul_block(lo, hi, xb_buf, W_1, ffn_h, D, D_FF);
    for (int i = lo; i < hi; i++) {
        double *hi_ = ffn_h + (size_t)i * D_FF;
        for (int j = 0; j < D_FF; j++) if (hi_[j] < 0.0) hi_[j] = 0.0;
    }
    matmul_block(lo, hi, ffn_h, W_2, xa_buf, D_FF, D);
    for (int i = lo; i < hi; i++) {
        double *xi = x_buf + (size_t)i * D;
        const double *yi = xa_buf + (size_t)i * D;
        for (int j = 0; j < D; j++) xi[j] += yi[j];
    }
    /* Unembed: x @ W_U -> separate logits buffer (per-row stride VOCAB). */
    matmul_block(lo, hi, x_buf, W_U, logits_buf, D, VOCAB);
    for (int i = lo; i < hi; i++) {
        const double *li = logits_buf + (size_t)i * VOCAB;
        int best = 0; double bv = li[0];
        for (int v = 1; v < VOCAB; v++) if (li[v] > bv) { bv = li[v]; best = v; }
        output_tokens[i] = (uint8_t)best;
    }
    return 0;
}

/* Barrier: pure sync, ignores inputs, returns 0. */
static int64_t s_barrier(void) { return 0; }

/* Result: compute checksum over output_tokens, print, return. */
static int64_t s_result(void) {
    uint64_t cs = 0;
    for (int i = 0; i < N; i++) cs = (cs + (uint64_t)(i + 1) * (uint64_t)output_tokens[i]) & 0xFFFFFFFFu;
    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    fflush(stdout);
    /* persist output_tokens.bin so other tools can validate. */
    char p[1024]; snprintf(p, sizeof p, "%s/output_tokens.bin", DATA_DIR);
    FILE *f = fopen(p, "wb"); if (f) { fwrite(output_tokens, 1, N, f); fclose(f); }
    return (int64_t)cs;
}

/* Trebuchet bindings (strong overrides of WEAK sN slots in wrappers.c). */
void s__SUPER_INIT__(int64_t *in, int64_t *out) { (void)in; out[0] = s_init(); }
void s__SUPER_PHASE_A__(int64_t *in, int64_t *out) {
    /* in[0] = trigger from init (unused), in[1] = block_idx */
    out[0] = s_phaseA(in[1]);
}
void s__SUPER_BARRIER__(int64_t *in, int64_t *out) { (void)in; out[0] = s_barrier(); }
void s__SUPER_PHASE_B__(int64_t *in, int64_t *out) {
    /* in[0] = trigger from barrier (unused), in[1] = block_idx */
    out[0] = s_phaseB(in[1]);
}
void s__SUPER_RESULT__(int64_t *in, int64_t *out) { (void)in; out[0] = s_result(); }

/* Stubs for per-thread mode (no per-thread state needed in C). */
__attribute__((visibility("default"))) void supers_hs_init(void) { }
__attribute__((visibility("default"))) void supers_hs_exit(void) { }
__attribute__((visibility("default"))) void supers_hs_init_thread(void) { }
__attribute__((visibility("default"))) void supers_hs_thread_done(void) { }
"""


def emit_chunked_fan_in(final_kind, leaf_names, final_name, intermediate_kind="barrier", branch=16):
    """Logarithmic-depth fan-in. The TALM assembler encodes source count in
    5 bits so each super accepts at most 31 inputs; we chunk into groups of
    `branch` (default 16). Intermediate levels use `intermediate_kind`
    (default "barrier" — no side effects); only the final level uses
    `final_kind`. Returns the list of .fl lines and the final output name.
    """
    if len(leaf_names) <= branch:
        return [f"{final_kind} {final_name}, " + ", ".join(leaf_names)], final_name
    lines = []
    intermediates = []
    for ci in range(0, len(leaf_names), branch):
        chunk = leaf_names[ci:ci + branch]
        name = f"{final_name}_lvl0_{ci // branch}"
        lines.append(f"{intermediate_kind} {name}, " + ", ".join(chunk))
        intermediates.append(name)
    if len(intermediates) <= branch:
        lines.append(f"{final_kind} {final_name}, " + ", ".join(intermediates))
        return lines, final_name
    raise ValueError(f"n_blocks={len(leaf_names)} exceeds branch^2={branch*branch}; extend tree")


def emit(out_dir, data_dir, N, D, n_heads, head_dim, d_ff, vocab, n_blocks):
    os.makedirs(out_dir, exist_ok=True)

    # ---- 1. .fl dataflow graph ----
    fl_lines = [
        f"superinst('init',    {SUPER_INIT},    1, False, False)",
        f"superinst('phaseA',  {SUPER_PHASE_A}, 1, False, False)",
        f"superinst('barrier', {SUPER_BARRIER}, 1, False, False)",
        f"superinst('phaseB',  {SUPER_PHASE_B}, 1, False, False)",
        f"superinst('output',  {SUPER_RESULT},  1, False, False)",
        "avgtime('phaseA', 1000)",
        "avgtime('phaseB', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    # Phase A supers
    for b in range(n_blocks):
        fl_lines.append(f"const k_A_{b}, {b}")
        fl_lines.append(f"phaseA a{b}, ini, k_A_{b}")
    # Chunked barrier (handles n_blocks > 31). Intermediate AND final levels use "barrier".
    barrier_lines, barrier_name = emit_chunked_fan_in(
        "barrier", [f"a{b}" for b in range(n_blocks)], "br",
        intermediate_kind="barrier")
    fl_lines.extend(barrier_lines)
    # Phase B supers
    for b in range(n_blocks):
        fl_lines.append(f"const k_B_{b}, {b}")
        fl_lines.append(f"phaseB b{b}, {barrier_name}, k_B_{b}")
    # Chunked output super (final fan-in over all phaseB done signals).
    # Intermediate levels use "barrier" (pure sync, no side effects);
    # only the top-level uses "output" which calls s_result() to print CHECKSUM.
    output_lines, _ = emit_chunked_fan_in(
        "output", [f"b{b}" for b in range(n_blocks)], "out",
        intermediate_kind="barrier")
    fl_lines.extend(output_lines)

    fl_path = os.path.join(out_dir, "attn.fl")
    with open(fl_path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    print(f"[gen_attn_c] wrote {fl_path}  (n_blocks={n_blocks}, 2*K+barrier+result = {2*n_blocks + 3} supers)")

    # ---- 2. C source ----
    c_src = (C_TEMPLATE
             .replace("__N__", str(N))
             .replace("__D__", str(D))
             .replace("__N_HEADS__", str(n_heads))
             .replace("__HEAD_DIM__", str(head_dim))
             .replace("__D_FF__", str(d_ff))
             .replace("__VOCAB__", str(vocab))
             .replace("__N_BLOCKS__", str(n_blocks))
             .replace("__DATA_DIR__", data_dir)
             .replace("__SUPER_INIT__", str(SUPER_INIT))
             .replace("__SUPER_PHASE_A__", str(SUPER_PHASE_A))
             .replace("__SUPER_BARRIER__", str(SUPER_BARRIER))
             .replace("__SUPER_PHASE_B__", str(SUPER_PHASE_B))
             .replace("__SUPER_RESULT__", str(SUPER_RESULT)))
    c_path = os.path.join(out_dir, "attn_c_supers.c")
    with open(c_path, "w") as f:
        f.write(c_src)
    print(f"[gen_attn_c] wrote {c_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    ap.add_argument("--n-blocks", type=int, required=True)
    args = ap.parse_args()

    # Read config from data dir
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.out_dir, os.path.abspath(args.data_dir),
         cfg["N"], cfg["D"], cfg["N_HEADS"], cfg["HEAD_DIM"], cfg["D_FF"], cfg["VOCAB"],
         args.n_blocks)


if __name__ == "__main__":
    main()
