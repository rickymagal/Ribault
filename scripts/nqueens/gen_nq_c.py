#!/usr/bin/env python3
"""Generate Ribault N-Queens files with C-implemented supers.

DAG: one `solve` super per prefix state (n_states supers), fan-in
sync tree (MAX_FANIN=30 to fit TALM 5-bit src-count), output super
sums counts and emits CHECKSUM.

Super numbering:
  init     -> s10
  solve    -> s11
  sync     -> s15
  output   -> s16
"""

import argparse, os, struct


SUPER_INIT   = 10
SUPER_SOLVE  = 11
SUPER_SYNC   = 15
SUPER_OUTPUT = 16

MAX_FANIN    = 30


C_TEMPLATE = r"""/* Auto-generated: Ribault N-Queens C supers. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_N 16
#define N           __N__
#define CUTOFF      __CUTOFF__
#define N_STATES    __N_STATES__

static const char *DATA_DIR = "__DATA_DIR__";

static int32_t *states;      /* [N_STATES * CUTOFF] */
static uint64_t *counts;     /* [N_STATES] per-state subtree count */
static uint64_t  total;      /* set by s_output() */

static void *xmalloc(size_t n) { void *p = malloc(n); if (!p) { fprintf(stderr,"OOM %zu\n",n); exit(1); } return p; }

static void load_states(void) {
    char path[1024]; snprintf(path, sizeof path, "%s/states.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    int32_t hdr[2];
    if (fread(hdr, sizeof(int32_t), 2, f) != 2) exit(1);
    if (hdr[0] != N_STATES || hdr[1] != CUTOFF) {
        fprintf(stderr, "states.bin header mismatch\n"); exit(1);
    }
    states = xmalloc((size_t)N_STATES * CUTOFF * sizeof(int32_t));
    if (fread(states, sizeof(int32_t), (size_t)N_STATES * CUTOFF, f) !=
        (size_t)N_STATES * CUTOFF) exit(1);
    fclose(f);
    counts = xmalloc((size_t)N_STATES * sizeof(uint64_t));
    memset(counts, 0, (size_t)N_STATES * sizeof(uint64_t));
}

static int64_t s_init(void) {
    load_states();
    return 0;
}


static inline int safe_q(const int *queens, int row, int col) {
    for (int r = 0; r < row; r++) {
        int c = queens[r];
        if (c == col)             return 0;
        if (c - r == col - row)   return 0;
        if (c + r == col + row)   return 0;
    }
    return 1;
}

static uint64_t solve_sub(int *queens, int row) {
    if (row == N) return 1ULL;
    uint64_t cnt = 0;
    for (int c = 0; c < N; c++) {
        if (safe_q(queens, row, c)) {
            queens[row] = c;
            cnt += solve_sub(queens, row + 1);
        }
    }
    return cnt;
}


static int64_t s_solve(int64_t state_idx) {
    int si = (int)state_idx;
    int queens[MAX_N];
    const int32_t *prefix = states + (size_t)si * CUTOFF;
    for (int r = 0; r < CUTOFF; r++) queens[r] = prefix[r];
    counts[si] = solve_sub(queens, CUTOFF);
    return 0;
}

static int64_t s_sync(void) { return 0; }

static int64_t s_output(void) {
    total = 0;
    for (int i = 0; i < N_STATES; i++) total += counts[i];
    printf("CHECKSUM=%lu\n", (unsigned long)total);
    fflush(stdout);
    return (int64_t)total;
}


/* Trebuchet glue.  in[0] = const state_idx (solve) or unused. */
void s__SUPER_INIT__(int64_t *in, int64_t *out)   { (void)in; out[0] = s_init(); }
void s__SUPER_SOLVE__(int64_t *in, int64_t *out)  { out[0] = s_solve(in[0]); }
void s__SUPER_SYNC__(int64_t *in, int64_t *out)   { (void)in; out[0] = s_sync(); }
void s__SUPER_OUTPUT__(int64_t *in, int64_t *out) { (void)in; out[0] = s_output(); }

__attribute__((visibility("default"))) void supers_hs_init(void) {}
__attribute__((visibility("default"))) void supers_hs_exit(void) {}
__attribute__((visibility("default"))) void supers_hs_init_thread(void) {}
__attribute__((visibility("default"))) void supers_hs_thread_done(void) {}
"""


def emit_fl(out_dir, n_states):
    fl = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('solve',  {SUPER_SOLVE},  1, False, False)",
        f"superinst('sync',   {SUPER_SYNC},   1, False, False)",
        f"superinst('output', {SUPER_OUTPUT}, 1, False, False)",
        "avgtime('solve', 1000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for s in range(n_states):
        fl.append(f"const sid_{s}, {s}")
        fl.append(f"solve sv_{s}, sid_{s}, ini")
    # Sync tree fan-in.
    current = [f"sv_{s}" for s in range(n_states)]
    sync_id = 0
    if len(current) == 1:
        root = current[0]
    else:
        while len(current) > 1:
            nxt = []
            for i in range(0, len(current), MAX_FANIN):
                batch = current[i:i + MAX_FANIN]
                cname = f"k_sync_{sync_id}"
                sname = f"sync_{sync_id}"
                fl.append(f"const {cname}, 0")
                fl.append(f"sync {sname}, {cname}, " + ", ".join(batch))
                nxt.append(sname); sync_id += 1
            current = nxt
        root = current[0]
    fl.append(f"output out, {root}")
    p = os.path.join(out_dir, "attn.fl")
    with open(p, "w") as f:
        f.write("\n".join(fl) + "\n")
    return p


def emit(out_dir, data_dir, N, CUTOFF, n_states):
    os.makedirs(out_dir, exist_ok=True)
    emit_fl(out_dir, n_states)
    print(f"[gen_nq_c] wrote {out_dir}/attn.fl  (n_states={n_states})")
    src = (C_TEMPLATE
           .replace("__N__", str(N))
           .replace("__CUTOFF__", str(CUTOFF))
           .replace("__N_STATES__", str(n_states))
           .replace("__DATA_DIR__", data_dir)
           .replace("__SUPER_INIT__",   str(SUPER_INIT))
           .replace("__SUPER_SOLVE__",  str(SUPER_SOLVE))
           .replace("__SUPER_SYNC__",   str(SUPER_SYNC))
           .replace("__SUPER_OUTPUT__", str(SUPER_OUTPUT)))
    cp = os.path.join(out_dir, "attn_c_supers.c")
    with open(cp, "w") as f: f.write(src)
    print(f"[gen_nq_c] wrote {cp}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            ws = line.split()
            if len(ws) >= 2: cfg[ws[0]] = ws[1]
    emit(args.out_dir, os.path.abspath(args.data_dir),
         int(cfg["N"]), int(cfg["CUTOFF"]), int(cfg["N_STATES"]))


if __name__ == "__main__":
    main()
