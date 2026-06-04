#!/usr/bin/env python3
"""Generate input matrix + Cholesky DAG for the dense block Cholesky benchmark.

Outputs in <out-dir>:
  A.bin                    block-storage of a symmetric positive-definite
                           N x N matrix where N = NB * B. Lower-triangular
                           blocks only, row-major:
                             block (i, j) at byte offset
                               block_index(i, j) * B * B * 8
                             where block_index(i, j) = i*(i+1)/2 + j (for i >= j)
                           Generated as A = I*alpha + R*R^T (R = small LCG-derived
                           dense matrix) — alpha tuned so A is well-conditioned SPD.

  config.txt               NB B SEED
  expected_checksum.txt    sum(L) mod 2^32 of the dense lower-triangular factor

  dag.bin                  DAG topology: header + ops + deps. See write_dag_bin.
                           Each op is (kind, target_block_id, source_block_ids...,
                           plus dependencies in a flat array).
                           Op kinds:
                             0 = POTRF(k)       — factorize A[k,k]
                             1 = TRSM(i, k)     — A[i,k] = A[i,k] * L[k,k]^-T
                             2 = SYRK(k, j)     — A[k,k] -= A[k,j] * A[k,j]^T
                             3 = GEMM(i, k, j)  — A[i,k] -= A[i,j] * A[k,j]^T
                           Layout:
                             [N_OPS (int32)]
                             For each op:
                               [op_kind (int32), target_i (int32), target_j (int32),
                                src1_i (int32), src1_j (int32),    (or -1, -1 if N/A)
                                src2_i (int32), src2_j (int32),    (or -1, -1)
                                level (int32),                     (for STRAT/parpseq)
                                n_deps (int32), dep_op_ids (int32)*n_deps]
                           Ops are listed in topological order so dep_op_ids
                           always refer to earlier ops.

After running gen_input the algorithm is fully determined: every variant
loads A.bin into its block storage, follows the DAG, and produces L.
Checksum is sum of L's lower-triangular elements mod 2^32 (order-invariant).
"""

import argparse, os, struct
import numpy as np


SEED   = 42
A_LCG  = 6364136223846793005
C_LCG  = 1442695040888963407
MASK63 = 0x7FFFFFFFFFFFFFFF


def lcg_double_stream(seed, count, scale=0.01):
    """Yield count doubles in approx [-scale, scale]."""
    r = seed
    if r == 0: r = 1
    for _ in range(count):
        r = (A_LCG * r + C_LCG) & MASK63
        # Map to [0, 1) then to [-scale, scale)
        u = (r >> 11) / (1 << 52)  # 53-bit double precision
        yield (u * 2.0 - 1.0) * scale


def block_idx(i, j):
    """Row-major lower-triangular block index. Caller guarantees i >= j."""
    return i * (i + 1) // 2 + j


def gen_spd_matrix(NB, B, seed):
    """Generate a banded SPD matrix as A = D + R*R^T where R is small.
    Then store only lower-triangular blocks in row-major block index order.
    Returns a numpy array shape (n_blocks, B, B) where n_blocks = NB*(NB+1)/2."""
    N = NB * B
    # Generate R as a dense N x N matrix with small LCG-derived values
    rng_stream = lcg_double_stream(seed, N * N)
    R = np.fromiter(rng_stream, dtype=np.float64, count=N*N).reshape(N, N)
    # A = R @ R^T + alpha * I  (always SPD with alpha > 0)
    A = R @ R.T + np.eye(N) * (N * 0.01)  # diagonal boost for conditioning
    # Extract lower-triangular blocks
    n_blocks = NB * (NB + 1) // 2
    blocks = np.zeros((n_blocks, B, B), dtype=np.float64)
    for i in range(NB):
        for j in range(i + 1):
            blocks[block_idx(i, j)] = A[i*B:(i+1)*B, j*B:(j+1)*B]
    return blocks


def reference_cholesky(blocks, NB, B):
    """Reference factorization (block in-place). Same algorithm every variant
    follows, but here computed with numpy operations. Returns L blocks and
    the sum-checksum of L's lower-triangular entries.

    Algorithm (left-looking blocked Cholesky):
      for k = 0..NB-1:
        for j = 0..k-1:    SYRK:  A[k,k] -= A[k,j] @ A[k,j].T
        POTRF:             A[k,k] = chol_lower(A[k,k])
        for i = k+1..NB-1:
          for j = 0..k-1:  GEMM:  A[i,k] -= A[i,j] @ A[k,j].T
          TRSM:            A[i,k] = A[i,k] @ inv(A[k,k].T)
    """
    L = blocks.copy()
    for k in range(NB):
        for j in range(k):
            L[block_idx(k, k)] -= L[block_idx(k, j)] @ L[block_idx(k, j)].T
        L[block_idx(k, k)] = np.linalg.cholesky(L[block_idx(k, k)])
        for i in range(k + 1, NB):
            for j in range(k):
                L[block_idx(i, k)] -= L[block_idx(i, j)] @ L[block_idx(k, j)].T
            # Solve L[k,k] · X.T = L[i,k].T   →   X = L[i,k] @ L[k,k]^-T
            L[block_idx(i, k)] = np.linalg.solve(L[block_idx(k, k)].T,
                                                  L[block_idx(i, k)].T).T

    # Checksum: sum of all elements in lower-triangular L (block-storage)
    # Order-invariant — but only over the lower-tri elements (j <= i within block
    # at diagonal blocks; full block for off-diagonal). To keep simple, we sum
    # over ALL stored elements (i.e., the full block contents incl. upper-tri
    # within diagonal blocks). The upper-tri of diagonal blocks is UNDEFINED
    # by Cholesky's lower-only output; reset to zero for cleanliness.
    for k in range(NB):
        # Zero the upper-triangular part of diagonal blocks
        d = L[block_idx(k, k)]
        for ii in range(B):
            for jj in range(ii + 1, B):
                d[ii, jj] = 0.0
    cs = 0
    for blk in L:
        # Use fixed-point arithmetic so checksum is reproducible across variants
        for v in blk.flat:
            # Convert to fixed-point with 1e6 multiplier, then sum as u64
            cs = (cs + int(v * 1e6) & 0xFFFFFFFF) & 0xFFFFFFFF
    return L, cs


def build_dag(NB):
    """Build the operation DAG with chained serial updates to each block.
    Returns list of dicts: { id, kind, ti, tj, s1i, s1j, s2i, s2j, level, deps[] }.
    Op kinds: 0=POTRF, 1=TRSM, 2=SYRK, 3=GEMM.
    'level' = max(dep.level) + 1 (for STRAT/parpseq level-by-level).
    """
    ops = []
    # block_producer[(i, j)] = op_id that produces the FINAL value of A[i, j]
    block_producer = {}
    # last_update_target[(i, j)] = op_id of last SYRK/GEMM updating A[i, j]
    # (used to chain serial in-place updates)
    last_update = {}

    def add_op(kind, ti, tj, s1i, s1j, s2i, s2j, deps):
        op_id = len(ops)
        level = 1 + max((ops[d]['level'] for d in deps), default=0) if deps else 1
        ops.append({
            'id': op_id, 'kind': kind, 'ti': ti, 'tj': tj,
            's1i': s1i, 's1j': s1j, 's2i': s2i, 's2j': s2j,
            'level': level, 'deps': deps
        })
        return op_id

    for k in range(NB):
        # SYRK chain: A[k,k] -= A[k,j] · A[k,j]^T for j = 0..k-1
        for j in range(k):
            deps = []
            if (k, j) in block_producer:
                deps.append(block_producer[(k, j)])
            if (k, k) in last_update:
                deps.append(last_update[(k, k)])  # chain on A[k,k]
            op_id = add_op(2, k, k, k, j, -1, -1, deps)
            last_update[(k, k)] = op_id
        # POTRF: A[k,k] = chol_lower(A[k,k])
        deps = []
        if (k, k) in last_update:
            deps.append(last_update[(k, k)])
        op_id = add_op(0, k, k, -1, -1, -1, -1, deps)
        block_producer[(k, k)] = op_id

        # For each i > k: GEMM chain updates to A[i,k], then TRSM
        for i in range(k + 1, NB):
            for j in range(k):
                deps = []
                if (i, j) in block_producer:
                    deps.append(block_producer[(i, j)])
                if (k, j) in block_producer:
                    deps.append(block_producer[(k, j)])
                if (i, k) in last_update:
                    deps.append(last_update[(i, k)])  # chain on A[i,k]
                op_id = add_op(3, i, k, i, j, k, j, deps)
                last_update[(i, k)] = op_id
            # TRSM: A[i,k] = A[i,k] · L[k,k]^-T
            deps = [block_producer[(k, k)]]
            if (i, k) in last_update:
                deps.append(last_update[(i, k)])
            op_id = add_op(1, i, k, k, k, -1, -1, deps)
            block_producer[(i, k)] = op_id

    return ops


def write_dag_bin(out_dir, ops):
    path = os.path.join(out_dir, "dag.bin")
    with open(path, "wb") as f:
        f.write(struct.pack("<i", len(ops)))
        for op in ops:
            f.write(struct.pack("<9i",
                op['kind'], op['ti'], op['tj'],
                op['s1i'], op['s1j'], op['s2i'], op['s2j'],
                op['level'], len(op['deps'])))
            if op['deps']:
                f.write(struct.pack(f"<{len(op['deps'])}i", *op['deps']))
    return path


def write_A_bin(out_dir, blocks):
    path = os.path.join(out_dir, "A.bin")
    blocks.astype(np.float64).tofile(path)
    return path


def write_input(out_dir, NB, B, seed):
    os.makedirs(out_dir, exist_ok=True)
    print(f"[gen_input] generating SPD matrix N={NB*B} (NB={NB}, B={B})...")
    A_blocks = gen_spd_matrix(NB, B, seed)
    write_A_bin(out_dir, A_blocks)
    # SPD verification: numpy.linalg.cholesky must succeed (else our LCG seed
    # produced a non-SPD matrix and we'd be running garbage). This is fast
    # (single call, no checksum needed).
    import numpy as _np
    N = NB * B
    A_dense = _np.zeros((N, N))
    for i in range(NB):
        for j in range(i + 1):
            A_dense[i*B:(i+1)*B, j*B:(j+1)*B] = A_blocks[block_idx(i, j)]
            if i != j:
                A_dense[j*B:(j+1)*B, i*B:(i+1)*B] = A_blocks[block_idx(i, j)].T
    _np.linalg.cholesky(A_dense)  # raises if not SPD
    print(f"[gen_input] SPD verification OK (numpy.linalg.cholesky succeeded)")
    # Canonical checksum: written by the runner after running sc_seq_c once
    # (it knows the algorithm order we use). For now write placeholder.
    with open(os.path.join(out_dir, "expected_checksum.txt"), "w") as f:
        f.write("0\n")
    with open(os.path.join(out_dir, "config.txt"), "w") as f:
        f.write(f"NB {NB}\n")
        f.write(f"B {B}\n")
        f.write(f"SEED {seed}\n")
    print(f"[gen_input] wrote A.bin ({A_blocks.nbytes} bytes)")
    print(f"[gen_input] building DAG...")
    ops = build_dag(NB)
    max_lev = max((o['level'] for o in ops), default=0)
    write_dag_bin(out_dir, ops)
    print(f"[gen_input] wrote dag.bin ({len(ops)} ops, max_level={max_lev}, "
          f"avg_deps={sum(len(o['deps']) for o in ops) / max(1, len(ops)):.1f})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--NB", type=int, required=True)
    ap.add_argument("--B", type=int, default=64)
    ap.add_argument("--seed", type=int, default=SEED)
    args = ap.parse_args()
    write_input(args.out_dir, args.NB, args.B, args.seed)


if __name__ == "__main__":
    main()
