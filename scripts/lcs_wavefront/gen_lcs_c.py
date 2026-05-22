#!/usr/bin/env python3
"""Generate Ribault LCS files with C-implemented supers (no Haskell leaf).

Produces:
  1. lcs_wf.fl       — same dataflow graph as gen_talm_input.py
  2. lcs_c_supers.c  — C implementation of init/block/result with constants
                       (LCS_SEQ_LEN, LCS_DIM_ROWS, LCS_DIM_COLS, LCS_SEED,
                       LCS_ALPHA) baked at compile time. Mirrors the Haskell
                       lcsBlock byte-for-byte: same boundary arrays, same
                       inner DP loop, same LCG seed.

The C file exports s10, s11, s12, s13 (strong symbols) which override the
WEAK_FN Haskell exports declared in supers_wrappers.c. Linked into
libsupers.so without any Haskell module.
"""

import argparse, os


SUPER_INIT    = 13
SUPER_BLOCK1  = 12  # 2 inputs: dep + idx
SUPER_BLOCK2  = 11  # 3 inputs: top + left + idx
SUPER_RESULT  = 10


def emit(out_dir, seq_len, alphabet, seed, dim_rows, dim_cols):
    os.makedirs(out_dir, exist_ok=True)

    # ---- 1. .fl ----
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('block1', {SUPER_BLOCK1}, 1, False, False)",
        f"superinst('block2', {SUPER_BLOCK2}, 1, False, False)",
        f"superinst('output', {SUPER_RESULT}, 1, False, False)",
        f"avgtime('block1', 10000)",
        f"avgtime('block2', 10000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]

    def bname(i, j):
        return f"blck{i * dim_cols + j}"

    def kname(i, j):
        return f"k_{i}_{j}"

    fl_lines.append(f"const {kname(0, 0)}, 0")
    fl_lines.append(f"block1 {bname(0, 0)}, ini, {kname(0, 0)}")

    for j in range(1, dim_cols):
        idx = j
        fl_lines.append(f"const {kname(0, j)}, {idx}")
        fl_lines.append(f"block1 {bname(0, j)}, {bname(0, j-1)}, {kname(0, j)}")

    for i in range(1, dim_rows):
        idx = i * dim_cols
        fl_lines.append(f"const {kname(i, 0)}, {idx}")
        fl_lines.append(f"block1 {bname(i, 0)}, {bname(i-1, 0)}, {kname(i, 0)}")

    for i in range(1, dim_rows):
        for j in range(1, dim_cols):
            idx = i * dim_cols + j
            fl_lines.append(f"const {kname(i, j)}, {idx}")
            fl_lines.append(
                f"block2 {bname(i, j)}, {bname(i-1, j)}, {bname(i, j-1)}, {kname(i, j)}"
            )

    fl_lines.append(f"output out, {bname(dim_rows-1, dim_cols-1)}")

    fl_path = os.path.join(out_dir, "lcs_wf.fl")
    with open(fl_path, "w", encoding="utf-8") as f:
        f.write("\n".join(fl_lines) + "\n")
    print(f"[gen_lcs_c] wrote {fl_path}  ({dim_rows*dim_cols} blocks)")

    # ---- 2. lcs_c_supers.c ----
    c_path = os.path.join(out_dir, "lcs_c_supers.c")
    c_src = f"""/* Auto-generated: LCS wavefront supers (C implementation).
 * Mirrors supers_inject.hs:lcsBlock byte-for-byte. Same LCG, same boundary
 * arrays, same inner DP loop. No Haskell, no FFI cross — the entire compute
 * is in C inside libsupers.so.
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#define LCS_SEQ_LEN  {seq_len}
#define LCS_ALPHA    {alphabet}
#define LCS_SEED     {seed}ULL
#define LCS_DIM_ROWS {dim_rows}
#define LCS_DIM_COLS {dim_cols}

/* Global state shared by all blocks (same role as Haskell's globalLCS IORef). */
static int  *g_sa;      /* sequence A, LCS_SEQ_LEN ints */
static int  *g_sb;      /* sequence B, LCS_SEQ_LEN ints */
static int  *g_hBound;  /* (LCS_DIM_ROWS+1) rows of (LCS_SEQ_LEN+1) ints */
static int  *g_vBound;  /* (LCS_DIM_COLS+1) cols of (LCS_SEQ_LEN+1) ints */

/* LCG: same as Haskell lcsNextRng. */
static inline uint64_t lcs_next_rng(uint64_t r) {{
    return (6364136223846793005ULL * r + 1442695040888963407ULL) & 0x7FFFFFFFFFFFFFFFULL;
}}

/* Generate `length` ints into `arr` from LCG state `rng0`. Returns final state. */
static uint64_t lcs_gen_seq(uint64_t rng0, int *arr, int length, int alpha) {{
    uint64_t r = rng0;
    for (int i = 0; i < length; i++) {{
        r = lcs_next_rng(r);
        arr[i] = (int)((r >> 33) % (uint64_t)alpha);
    }}
    return r;
}}

/* Init: generate seq_a, seq_b, allocate boundary arrays (zeroed). */
static int64_t lcs_init(int64_t seed_arg) {{
    (void)seed_arg;
    int n    = LCS_SEQ_LEN;
    int cols = n + 1;
    g_sa     = (int *)malloc(n * sizeof(int));
    g_sb     = (int *)malloc(n * sizeof(int));
    g_hBound = (int *)calloc((LCS_DIM_ROWS + 1) * (size_t)cols, sizeof(int));
    g_vBound = (int *)calloc((LCS_DIM_COLS + 1) * (size_t)cols, sizeof(int));
    uint64_t rng = LCS_SEED;
    rng = lcs_gen_seq(rng, g_sa, n, LCS_ALPHA);
    lcs_gen_seq(rng, g_sb, n, LCS_ALPHA);
    return 0;
}}

/* Block computation: compute block (bi, bj). Same DP recurrence as the
 * Haskell version; boundary IO via g_hBound/g_vBound. */
static int64_t lcs_block(int64_t blockIdx) {{
    int bi = (int)(blockIdx / LCS_DIM_COLS);
    int bj = (int)(blockIdx % LCS_DIM_COLS);
    int n        = LCS_SEQ_LEN;
    int cols     = n + 1;
    int chunkR   = n / LCS_DIM_ROWS;
    int chunkC   = n / LCS_DIM_COLS;
    int rowStart = bi * chunkR + 1;
    int rowEnd   = (bi == LCS_DIM_ROWS - 1) ? n : (bi + 1) * chunkR;
    int colStart = bj * chunkC + 1;
    int colEnd   = (bj == LCS_DIM_COLS - 1) ? n : (bj + 1) * chunkC;
    int localCols = colEnd - colStart + 1;

    int *sa = g_sa;
    int *sb = g_sb;
    int *hB = g_hBound;
    int *vB = g_vBound;

    int hBReadBase  = bi * cols + colStart - 1;
    int hBWriteBase = (bi + 1) * cols + colStart - 1;
    int vBReadBase  = bj * cols;
    int vBWriteBase = (bj + 1) * cols;
    int sbBase      = colStart - 2;

    int buf1_sz = localCols + 1;
    int *buf1 = (int *)malloc(buf1_sz * sizeof(int));
    int *buf2 = (int *)malloc(buf1_sz * sizeof(int));

    for (int lj = 0; lj <= localCols; lj++) buf1[lj] = hB[hBReadBase + lj];

    int *prev = buf1;
    int *cur  = buf2;

    for (int i = rowStart; i <= rowEnd; i++) {{
        cur[0] = vB[vBReadBase + i];
        int ai = sa[i - 1];
        for (int lj = 1; lj <= localCols; lj++) {{
            int bj_val = sb[sbBase + lj];
            if (ai == bj_val) {{
                cur[lj] = prev[lj - 1] + 1;
            }} else {{
                int u = prev[lj];
                int l = cur[lj - 1];
                cur[lj] = (u > l) ? u : l;
            }}
        }}
        vB[vBWriteBase + i] = cur[localCols];
        int *tmp = prev; prev = cur; cur = tmp;
    }}

    for (int lj = 0; lj <= localCols; lj++) hB[hBWriteBase + lj] = prev[lj];

    free(buf1);
    free(buf2);
    return 0;
}}

/* Result: print final LCS score from hBound[DIM_ROWS][N]. */
static int64_t lcs_result(int64_t dep) {{
    (void)dep;
    int score = g_hBound[LCS_DIM_ROWS * (LCS_SEQ_LEN + 1) + LCS_SEQ_LEN];
    printf("RESULT=%d\\n", score);
    fflush(stdout);
    return 0;
}}

/* Strong symbols overriding the WEAK_FN Haskell exports in supers_wrappers.c.
 * Naming follows supers_wrappers convention: sN(int64_t *in, int64_t *out). */

void s{SUPER_INIT}(int64_t *in, int64_t *out) {{ out[0] = lcs_init(in[0]); }}

void s{SUPER_BLOCK1}(int64_t *in, int64_t *out) {{
    /* in[0]=dep (unused), in[1]=blockIdx */
    out[0] = lcs_block(in[1]);
}}

void s{SUPER_BLOCK2}(int64_t *in, int64_t *out) {{
    /* in[0]=top (unused), in[1]=left (unused), in[2]=blockIdx */
    out[0] = lcs_block(in[2]);
}}

void s{SUPER_RESULT}(int64_t *in, int64_t *out) {{ out[0] = lcs_result(in[0]); }}

/* Stub exports to switch the Trebuchet interpreter from "central supers
 * worker" mode (used for Haskell, mutex-serialized) to "per-thread direct
 * call" mode. The interp keys off the presence of supers_hs_init_thread to
 * decide. C supers need no per-thread init, so these are no-ops. */
__attribute__((visibility("default"))) void supers_hs_init(void) {{ }}
__attribute__((visibility("default"))) void supers_hs_exit(void) {{ }}
__attribute__((visibility("default"))) void supers_hs_init_thread(void) {{ }}
__attribute__((visibility("default"))) void supers_hs_thread_done(void) {{ }}
"""
    with open(c_path, "w", encoding="utf-8") as f:
        f.write(c_src)
    print(f"[gen_lcs_c] wrote {c_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--input-dir", required=True,
                    help="Directory containing params.txt (N alpha seed)")
    ap.add_argument("--dim-rows", type=int, required=True)
    ap.add_argument("--dim-cols", type=int, required=True)
    args = ap.parse_args()

    with open(os.path.join(args.input_dir, "params.txt")) as f:
        parts = f.read().split()
        seq_len, alphabet, seed = int(parts[0]), int(parts[1]), int(parts[2])

    emit(args.out_dir, seq_len, alphabet, seed, args.dim_rows, args.dim_cols)


if __name__ == "__main__":
    main()
