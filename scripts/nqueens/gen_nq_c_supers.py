#!/usr/bin/env python3
"""Generate a C super-body file for Ribault N-Queens.

Pairs with the SAME .hss / .fl produced by codegen for the ribault_hs
variant.  This file overrides the WEAK Haskell supers s10 (safeImpl)
and s11 (solveRestImpl) with strong C symbols.  N is baked at
compile time via the NQ_N macro.

solveRestImpl semantics (mirrors nq_seq.c bit-for-bit):
    Given a partial board encoded as a base-N integer `path`
    (q_0 + q_1*N + ... + q_{row-1}*N^(row-1)) and current row,
    return the number of solutions completing the board.
"""

import argparse, os


C_TEMPLATE = r"""/* Auto-generated Ribault N-Queens C supers (N=__N__).
 *
 * Overrides s10 (safeImpl) and s11 (solveRestImpl) with strong C symbols.
 * The dataflow graph (.fl) is unchanged -- compiled from the same .hss
 * that ribault_hs uses.  Only the leaf compute moves from Haskell to C.
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define NQ_N __N__

/* Decode path-integer into queens[0..row-1] (col choices, base N). */
static inline void nq_decode(int64_t path, int row, int *queens) {
    int64_t p = path;
    for (int i = 0; i < row; i++) {
        queens[i] = (int)(p % NQ_N);
        p /= NQ_N;
    }
}

/* Check whether placing column `col` at row `row` is safe given queens[0..row-1]. */
static inline int nq_safe(const int *queens, int row, int col) {
    for (int r = 0; r < row; r++) {
        int c = queens[r];
        if (c == col) return 0;
        if (c - r == col - row) return 0;
        if (c + r == col + row) return 0;
    }
    return 1;
}

/* Sequential N-Queens from (queens, row); mutates queens but restores it.
 * Returns the number of complete solutions reachable. */
static uint64_t nq_solve(int *queens, int row) {
    if (row == NQ_N) return 1ULL;
    uint64_t cnt = 0;
    for (int c = 0; c < NQ_N; c++) {
        if (nq_safe(queens, row, c)) {
            queens[row] = c;
            cnt += nq_solve(queens, row + 1);
        }
    }
    return cnt;
}

/* safeImpl: path, currRow, col -> 1 if `col` is safe at depth currRow given
 * the partial board encoded in `path`, 0 otherwise. */
static int64_t safe_impl(int64_t path, int64_t currRow, int64_t col) {
    int queens[NQ_N];
    nq_decode(path, (int)currRow, queens);
    return (int64_t)nq_safe(queens, (int)currRow, (int)col);
}

/* solveRestImpl: path, pow (unused at C level), row -> count of solutions
 * completing the partial board encoded in `path` (which has `row` queens
 * already placed). */
static int64_t solve_rest_impl(int64_t path, int64_t pow, int64_t row) {
    (void)pow;
    int queens[NQ_N];
    nq_decode(path, (int)row, queens);
    return (int64_t)nq_solve(queens, (int)row);
}

/* Strong-symbol overrides of the supers_wrappers.c weak forward decls. */

/* s4: built-in `print` for scalars (the .hss does `main = print (solve ...)`).
 * Without this override, the print super is missing and the program emits
 * nothing on stdout. */
void s4(int64_t *in, int64_t *out) {
    printf("%lld\n", (long long)in[0]);
    fflush(stdout);
    out[0] = in[0];
}

void s10(int64_t *in, int64_t *out) {
    /* in: [path, currRow, col] */
    out[0] = safe_impl(in[0], in[1], in[2]);
}

void s11(int64_t *in, int64_t *out) {
    /* in: [path, pow, row] */
    out[0] = solve_rest_impl(in[0], in[1], in[2]);
}

/* Trebuchet keys off the presence of supers_hs_init_thread to switch from
 * "mutex-serialized Haskell" mode to "per-thread direct call" mode.
 * C supers need no per-thread init; these are no-ops. */
__attribute__((visibility("default"))) void supers_hs_init(void) {}
__attribute__((visibility("default"))) void supers_hs_exit(void) {}
__attribute__((visibility("default"))) void supers_hs_init_thread(void) {}
__attribute__((visibility("default"))) void supers_hs_thread_done(void) {}
"""


def emit(out_path, N):
    dirpath = os.path.dirname(out_path)
    if dirpath:
        os.makedirs(dirpath, exist_ok=True)
    src = C_TEMPLATE.replace("__N__", str(N))
    with open(out_path, "w") as f:
        f.write(src)
    print(f"[gen_nq_c_supers] wrote {out_path} (N={N})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", required=True)
    ap.add_argument("--N", type=int, required=True)
    args = ap.parse_args()
    emit(args.out, args.N)


if __name__ == "__main__":
    main()
