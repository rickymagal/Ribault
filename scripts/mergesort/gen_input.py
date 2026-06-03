#!/usr/bin/env python3
"""Generate deterministic int32 input array AND the binary tree topology
for the mergesort benchmark.

Outputs in <out-dir>:
  input.bin                N * 4 bytes, little-endian int32, LCG-generated
  config.txt               N, CUTOFF, SEED (one per line)
  expected_checksum.txt    sum of array mod 2^32 (order-invariant)
  tree.bin                 dataflow DAG topology shared by all variants:
                             header [N_LEAVES (int32), N_MERGES (int32)]
                             leaves [(lo, hi) ...]    int32 pairs
                             merges [(lo, mid, hi, level) ...]  int32 4-tuples
                           Ribault variants load tree.bin for the per-leaf
                           and per-merge ranges (ignoring level). STRAT
                           and par/pseq variants use level to group merges
                           into per-level barriers.

Every variant reads input.bin + config.txt + tree.bin. The sum-based
checksum is order-invariant so it cross-validates any correct sort.
"""

import argparse, os, struct


SEED   = 42
A_LCG  = 6364136223846793005
C_LCG  = 1442695040888963407
MASK63 = 0x7FFFFFFFFFFFFFFF
LOW31  = 0x7FFFFFFF


def lcg_int32_stream(seed, count):
    r = seed
    if r == 0: r = 1
    for _ in range(count):
        r = (A_LCG * r + C_LCG) & MASK63
        yield int((r >> 33) & LOW31)


def build_tree(N, cutoff):
    """Build the binary mergesort DAG by top-down bisection. Returns:
       leaves: [(lo, hi) ...]
       merges: [(lo, mid, hi, level) ...]   level=1 for merges of two
                                            leaves, level=k+1 for merges
                                            of two level<=k merges
       root:   ('leaf'|'merge', id)
    """
    leaves = []
    merges = []
    def recurse(lo, hi):
        if hi - lo <= cutoff:
            i = len(leaves); leaves.append((lo, hi)); return ("leaf", i, 0)
        mid = lo + (hi - lo) // 2
        left  = recurse(lo, mid)
        right = recurse(mid, hi)
        my_lev = 1 + max(left[2], right[2])
        i = len(merges); merges.append((lo, mid, hi, my_lev))
        return ("merge", i, my_lev)
    root_kind, root_id, _ = recurse(0, N)
    return leaves, merges, (root_kind, root_id)


def write_tree_bin(out_dir, leaves, merges):
    path = os.path.join(out_dir, "tree.bin")
    with open(path, "wb") as f:
        f.write(struct.pack("<2i", len(leaves), len(merges)))
        for (lo, hi) in leaves:
            f.write(struct.pack("<2i", lo, hi))
        for (lo, mid, hi, level) in merges:
            f.write(struct.pack("<4i", lo, mid, hi, level))
    return path


def write_input(out_dir, N, cutoff, seed):
    os.makedirs(out_dir, exist_ok=True)
    path = os.path.join(out_dir, "input.bin")
    with open(path, "wb") as f:
        CHUNK = 1 << 20
        buf = []
        total = 0
        for v in lcg_int32_stream(seed, N):
            buf.append(v)
            if len(buf) == CHUNK:
                f.write(struct.pack(f"<{CHUNK}i", *buf))
                total += sum(v & 0xFFFFFFFF for v in buf)
                buf = []
        if buf:
            f.write(struct.pack(f"<{len(buf)}i", *buf))
            total += sum(v & 0xFFFFFFFF for v in buf)
    expected = total & 0xFFFFFFFF
    with open(os.path.join(out_dir, "expected_checksum.txt"), "w") as f:
        f.write(f"{expected}\n")
    with open(os.path.join(out_dir, "config.txt"), "w") as f:
        f.write(f"N {N}\n")
        f.write(f"CUTOFF {cutoff}\n")
        f.write(f"SEED {seed}\n")
    print(f"[gen_input] wrote {path} ({N} int32, {N*4} bytes)")
    print(f"[gen_input] expected checksum = {expected}")

    # Build + write the DAG topology shared by all variants.
    leaves, merges, root = build_tree(N, cutoff)
    tpath = write_tree_bin(out_dir, leaves, merges)
    max_lev = max((m[3] for m in merges), default=0)
    print(f"[gen_input] wrote {tpath} (leaves={len(leaves)} merges={len(merges)} "
          f"max_level={max_lev} root={root[0]}_{root[1]})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--cutoff", type=int, default=64)
    ap.add_argument("--seed", type=int, default=SEED)
    args = ap.parse_args()
    write_input(args.out_dir, args.N, args.cutoff, args.seed)


if __name__ == "__main__":
    main()
