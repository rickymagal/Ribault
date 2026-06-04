#!/usr/bin/env python3
"""Generate Ribault mergesort files with C-implemented supers.

Mergesort as a real Ribault dataflow DAG: each leaf is a single super
that sorts CUTOFF elements; each internal tree node is a merge super
that fires when both its children complete. The DAG is generated in
Python from the recursive top-down split and emitted as a flat .fl —
NO par/pseq inside a single super.

Emits:
  attn.fl              dataflow graph: init -> leaf_0..L-1 -> merge_0..M-1 -> output
  attn_c_supers.c      C super bodies (init/leaf/merge/output)
  tree.bin             binary blob with leaf and merge ranges (loaded at init)

5 super types in the .fl (numbers chosen to match LCS/attention convention):
  init      (s13)  -- load input.bin, allocate arrays, load tree.bin
  leaf      (s12)  -- per leaf: insertion-sort arr[lo, hi)
  merge     (s11)  -- per internal node: merge arr[lo, mid) and arr[mid, hi)
  barrier   (s14)  -- unused but declared for symmetry (legacy supers_aliases)
  output    (s10)  -- read sorted arr, compute & print CHECKSUM, verify monotone

The leaf and merge ops write to disjoint regions of arr[] and tmp[]
within a tree level, so multiple workers can fire them concurrently
without locking. The firing rule ensures a parent merge only fires
after both children have completed (synchronisation by tokens).
"""

import argparse, os, struct


SUPER_INIT    = 13
SUPER_LEAF    = 12
SUPER_BARRIER = 14
SUPER_MERGE   = 11
SUPER_RESULT  = 10


def build_tree(N, cutoff):
    """Returns (leaves, merges, root_kind, root_id) where
       leaves = [(lo, hi), ...]
       merges = [(lo, mid, hi, (left_kind, left_id), (right_kind, right_id)), ...]
                where kind in {"leaf", "merge"} and id indexes into leaves/merges
       root_kind, root_id: identifies the top of the tree."""
    leaves = []
    merges = []

    def recurse(lo, hi):
        if hi - lo <= cutoff:
            leaf_id = len(leaves)
            leaves.append((lo, hi))
            return ("leaf", leaf_id)
        mid = lo + (hi - lo) // 2
        left = recurse(lo, mid)
        right = recurse(mid, hi)
        merge_id = len(merges)
        merges.append((lo, mid, hi, left, right))
        return ("merge", merge_id)

    root_kind, root_id = recurse(0, N)
    return leaves, merges, (root_kind, root_id)


def write_tree_bin(out_dir, leaves, merges):
    """Binary layout: int32 little-endian.
       Header: [N_LEAVES, N_MERGES]
       Leaves: [lo_0, hi_0, lo_1, hi_1, ...]   (2 * N_LEAVES ints)
       Merges: [lo_0, mid_0, hi_0, lo_1, ...]  (3 * N_MERGES ints)"""
    path = os.path.join(out_dir, "tree.bin")
    with open(path, "wb") as f:
        f.write(struct.pack("<2i", len(leaves), len(merges)))
        for (lo, hi) in leaves:
            f.write(struct.pack("<2i", lo, hi))
        for (lo, mid, hi, _l, _r) in merges:
            f.write(struct.pack("<3i", lo, mid, hi))
    return path


C_TEMPLATE = r"""/* Auto-generated: Ribault mergesort C supers. */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define N         __N__
#define CUTOFF    __CUTOFF__
#define N_LEAVES  __N_LEAVES__
#define N_MERGES  __N_MERGES__

static const char *DATA_DIR = "__DATA_DIR__";

static int *arr;
static int *tmp;
static int *leaf_lo;   /* [N_LEAVES] */
static int *leaf_hi;
static int *merge_lo;  /* [N_MERGES] */
static int *merge_mid;
static int *merge_hi;

static void *xmalloc(size_t n) { void *p = malloc(n); if (!p) { fprintf(stderr, "OOM %zu\n", n); exit(1); } return p; }

static void load_tree(void) {
    char path[1024]; snprintf(path, sizeof path, "%s/tree.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    int hdr[2];
    if (fread(hdr, sizeof(int), 2, f) != 2) { fprintf(stderr, "short read tree.bin hdr\n"); exit(1); }
    if (hdr[0] != N_LEAVES || hdr[1] != N_MERGES) {
        fprintf(stderr, "tree.bin shape mismatch: hdr=(%d,%d) expected=(%d,%d)\n",
                hdr[0], hdr[1], N_LEAVES, N_MERGES); exit(1);
    }
    leaf_lo  = xmalloc((size_t)N_LEAVES * sizeof(int));
    leaf_hi  = xmalloc((size_t)N_LEAVES * sizeof(int));
    merge_lo = xmalloc((size_t)N_MERGES * sizeof(int));
    merge_mid= xmalloc((size_t)N_MERGES * sizeof(int));
    merge_hi = xmalloc((size_t)N_MERGES * sizeof(int));
    for (int i = 0; i < N_LEAVES; i++) {
        int pair[2];
        if (fread(pair, sizeof(int), 2, f) != 2) { fprintf(stderr, "short read leaf %d\n", i); exit(1); }
        leaf_lo[i] = pair[0]; leaf_hi[i] = pair[1];
    }
    for (int i = 0; i < N_MERGES; i++) {
        int quad[4];  /* lo, mid, hi, level — level is for STRAT/parpseq only */
        if (fread(quad, sizeof(int), 4, f) != 4) { fprintf(stderr, "short read merge %d\n", i); exit(1); }
        merge_lo[i] = quad[0]; merge_mid[i] = quad[1]; merge_hi[i] = quad[2];
        /* quad[3] = level, ignored here (Ribault firing rule handles deps) */
    }
    fclose(f);
}

static int64_t s_init(void) {
    arr = xmalloc((size_t)N * sizeof(int));
    tmp = xmalloc((size_t)N * sizeof(int));
    char path[1024]; snprintf(path, sizeof path, "%s/input.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    if (fread(arr, sizeof(int), N, f) != (size_t)N) { fprintf(stderr, "short read input.bin\n"); exit(1); }
    fclose(f);
    load_tree();
    return 0;
}

/* Leaf sort: libc qsort (glibc uses introsort). O(B log B). Same as ms_seq.c. */
static int cmp_int(const void *a, const void *b) {
    int x = *(const int *)a, y = *(const int *)b;
    return (x > y) - (x < y);
}
static inline void leaf_sort(int *a, int lo, int hi) {
    qsort(a + lo, (size_t)(hi - lo), sizeof(int), cmp_int);
}

/* Merge arr[lo, mid) + arr[mid, hi) via tmp, copy back. Range disjoint
   across concurrent merges at same tree level, so no locking. */
static inline void merge_op(int lo, int mid, int hi) {
    int i = lo, j = mid, k = lo;
    while (i < mid && j < hi) {
        if (arr[i] <= arr[j]) tmp[k++] = arr[i++];
        else                  tmp[k++] = arr[j++];
    }
    while (i < mid) tmp[k++] = arr[i++];
    while (j < hi)  tmp[k++] = arr[j++];
    memcpy(arr + lo, tmp + lo, (size_t)(hi - lo) * sizeof(int));
}

static int64_t s_leaf(int64_t leaf_idx) {
    int idx = (int)leaf_idx;
    leaf_sort(arr, leaf_lo[idx], leaf_hi[idx]);
    return 0;
}

static int64_t s_merge(int64_t merge_idx) {
    int idx = (int)merge_idx;
    merge_op(merge_lo[idx], merge_mid[idx], merge_hi[idx]);
    return 0;
}

static int64_t s_barrier(void) { return 0; }  /* unused */

static int64_t s_output(void) {
    uint64_t cs = 0;
    int ok = 1;
    for (int i = 0; i < N; i++) {
        if (i > 0 && arr[i] < arr[i - 1]) ok = 0;
        cs = (cs + (uint64_t)(uint32_t)arr[i]) & 0xFFFFFFFFu;
    }
    if (!ok) fprintf(stderr, "WARN: array not sorted\n");
    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    fflush(stdout);
    return (int64_t)cs;
}

/* Trebuchet bindings: in[] order = order of edges declared in .fl. */
void s__SUPER_INIT__(int64_t *in, int64_t *out) { (void)in; out[0] = s_init(); }
/* leaf: in[0] = trigger from init (unused), in[1] = leaf_idx */
void s__SUPER_LEAF__(int64_t *in, int64_t *out) { out[0] = s_leaf(in[1]); }
/* merge: in[0] = left child trigger, in[1] = right child trigger, in[2] = merge_idx */
void s__SUPER_MERGE__(int64_t *in, int64_t *out) { out[0] = s_merge(in[2]); }
void s__SUPER_BARRIER__(int64_t *in, int64_t *out) { (void)in; out[0] = s_barrier(); }
void s__SUPER_RESULT__(int64_t *in, int64_t *out) { (void)in; out[0] = s_output(); }

__attribute__((visibility("default"))) void supers_hs_init(void) { }
__attribute__((visibility("default"))) void supers_hs_exit(void) { }
__attribute__((visibility("default"))) void supers_hs_init_thread(void) { }
__attribute__((visibility("default"))) void supers_hs_thread_done(void) { }
"""


def node_var(kind, idx):
    return f"l_{idx}" if kind == "leaf" else f"m_{idx}"


def emit_fl(out_dir, leaves, merges, root):
    fl_lines = [
        f"superinst('init',    {SUPER_INIT},    1, False, False)",
        f"superinst('leaf',    {SUPER_LEAF},    1, False, False)",
        f"superinst('merge',   {SUPER_MERGE},   1, False, False)",
        f"superinst('barrier', {SUPER_BARRIER}, 1, False, False)",
        f"superinst('output',  {SUPER_RESULT},  1, False, False)",
        "avgtime('leaf', 100)",
        "avgtime('merge', 1000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    # Leaves: each takes ini + leaf_idx const
    for i in range(len(leaves)):
        fl_lines.append(f"const lk_{i}, {i}")
        fl_lines.append(f"leaf l_{i}, ini, lk_{i}")
    # Merges in dependency order (merges[] is bottom-up post-order)
    for i, (lo, mid, hi, left, right) in enumerate(merges):
        lvar = node_var(*left)
        rvar = node_var(*right)
        fl_lines.append(f"const mk_{i}, {i}")
        fl_lines.append(f"merge m_{i}, {lvar}, {rvar}, mk_{i}")
    # Output: 1 input from root
    root_var = node_var(*root)
    fl_lines.append(f"output out, {root_var}")

    path = os.path.join(out_dir, "attn.fl")  # using "attn" prefix for runner compat
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


def emit(out_dir, data_dir, N, cutoff):
    os.makedirs(out_dir, exist_ok=True)
    leaves, merges, root = build_tree(N, cutoff)
    # NOTE: tree.bin is written by gen_input.py (it owns the shared topology);
    # this script only builds the tree locally to emit the .fl DAG.

    fl_path = emit_fl(out_dir, leaves, merges, root)
    print(f"[gen_ms_c] wrote {fl_path}  (N={N} cutoff={cutoff} leaves={len(leaves)} merges={len(merges)})")

    c_src = (C_TEMPLATE
             .replace("__N__", str(N))
             .replace("__CUTOFF__", str(cutoff))
             .replace("__N_LEAVES__", str(len(leaves)))
             .replace("__N_MERGES__", str(len(merges)))
             .replace("__DATA_DIR__", data_dir)
             .replace("__SUPER_INIT__",    str(SUPER_INIT))
             .replace("__SUPER_LEAF__",    str(SUPER_LEAF))
             .replace("__SUPER_MERGE__",   str(SUPER_MERGE))
             .replace("__SUPER_BARRIER__", str(SUPER_BARRIER))
             .replace("__SUPER_RESULT__",  str(SUPER_RESULT)))
    c_path = os.path.join(out_dir, "attn_c_supers.c")
    with open(c_path, "w") as f:
        f.write(c_src)
    print(f"[gen_ms_c] wrote {c_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.out_dir, os.path.abspath(args.data_dir), cfg["N"], cfg["CUTOFF"])


if __name__ == "__main__":
    main()
