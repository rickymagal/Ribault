#!/usr/bin/env python3
"""Generate input + weights + DAG + expected checksum for the
Cascading Inference Pipeline benchmark.

Outputs in <out-dir>:
  input.bin             N * D bytes of LCG-generated items
  weights.bin           accept_table[1024] u32, reject_weights[256] i16,
                        ref_vec[8*64] f64, W1[128*64] f64, b1[128] f64,
                        W2[16*128] f64, b2[16] f64,
                        cos_table[64*256] f64  (pre-computed for stage 3)
  config.txt            N D CHUNK_SIZE B2_SLOTS E K3 H C T2 T3 SEED
                        + EXPECTED_S1_RATE..EXPECTED_S4_RATE
                        + ACTUAL_S1_RATE..ACTUAL_S4_RATE (post-calibration)
  dag.bin               <n_ops i32> then per op:
                        [kind i32, chunk_id i32, src1_op_id i32, level i32,
                         n_deps i32, dep_op_ids i32 * n_deps]
                        Kinds: 0=S1, 1=S2, 2=S3, 3=S4, 4=aggregate.
  expected_checksum.txt canonical checksum (sum of decisions mod 2^32)

Stage kernels (per item):
  S1  fingerprint                  O(D)        ~60% exit  -> ACCEPT_S1 = 1
  S2  bigram histogram + dot       O(D)        ~25% exit  -> REJECT_S2 = 2
  S3  cosine projection + max-cos  O(D*E)      ~10% exit  -> ACCEPT_S3 = 0x40 | best_k
  S4  MLP (W1 -> ReLU -> W2)       O(E*H+H*C)   ~5% reach -> CLASS    = 0x80 | argmax_out

All threshold/table parameters calibrated here on a subsample so every
variant reads identical config + weights and produces an identical
per-item decision (and thus identical checksum).
"""
import argparse, math, os, struct, sys


# ---------- LCG (same param as other benchmarks for cross-bench consistency) ----------
LCG_A = 1103515245
LCG_C = 12345
LCG_M = 2**31


def lcg_next(state):
    return (LCG_A * state + LCG_C) % LCG_M


def lcg_bytes(seed, n):
    """Yield n unsigned bytes deterministically from LCG seed."""
    s = seed
    for _ in range(n):
        s = lcg_next(s)
        yield (s >> 16) & 0xFF


# ---------- Constants (must match config.txt names exactly) ----------
D         = 256
B2_SLOTS  = 256
E         = 64
K3        = 8
H         = 128
C         = 16
SIG_BITS  = 16                    # sig = (sum of bytes) mod 2^16
SIG_RANGE = 1 << SIG_BITS         # 65536 possible sigs
ACCEPT_BITMAP_BYTES = SIG_RANGE // 8   # 8192 bytes (1 bit per possible sig)

EXPECTED_S1_RATE = 0.60  # fraction of N
EXPECTED_S2_RATE = 0.10  # fraction of N exiting at S2 (= 25% of S2 arrivals)
EXPECTED_S3_RATE = 0.25  # fraction of N exiting at S3 (= 83.3% of S3 arrivals)
EXPECTED_S4_REACH = 0.05  # fraction of N reaching S4


ACCEPT_S1   = 1
REJECT_S2   = 2
ACCEPT_S3_BASE = 0x40
CLASS_BASE     = 0x80


# ---------- Stage kernels in Python (calibration only — slow, reference) ----------

def stage1_decide(item_bytes, accept_bitmap):
    """Return True if item exits at S1 (bit set in cumulative-frequency bitmap)."""
    sig = sum(item_bytes) & 0xFFFF
    return ((accept_bitmap[sig >> 3] >> (sig & 7)) & 1) != 0


def stage2_score(item_bytes, reject_weights):
    """Return the int score used to decide REJECT_S2."""
    hist = [0] * B2_SLOTS
    for i in range(D - 1):
        bigram_id = (item_bytes[i] * 7 + item_bytes[i + 1]) & 0xFF
        hist[bigram_id] += 1
    return sum(hist[k] * reject_weights[k] for k in range(B2_SLOTS))


def stage3_embed(item_bytes, cos_table):
    """Return normalized embedding emb[E]."""
    emb = [0.0] * E
    for j in range(E):
        s = 0.0
        for i in range(D):
            s += cos_table[j * D + i] * (item_bytes[i] / 255.0)
        emb[j] = s
    nrm = math.sqrt(sum(x * x for x in emb))
    if nrm > 0:
        emb = [x / nrm for x in emb]
    return emb


def stage3_best(emb, ref_vec):
    best = 0
    best_sim = -1e30
    for k in range(K3):
        s = sum(emb[j] * ref_vec[k * E + j] for j in range(E))
        if s > best_sim:
            best_sim = s
            best = k
    return best, best_sim


def stage4_classify(emb, W1, b1, W2, b2):
    hidden = [0.0] * H
    for h in range(H):
        s = b1[h]
        for j in range(E):
            s += W1[h * E + j] * emb[j]
        hidden[h] = s if s > 0 else 0.0
    out = [0.0] * C
    for c in range(C):
        s = b2[c]
        for h in range(H):
            s += W2[c * H + h] * hidden[h]
        out[c] = s
    best = 0
    best_v = out[0]
    for c in range(1, C):
        if out[c] > best_v:
            best_v = out[c]
            best = c
    return best


# ---------- LCG-derived helpers for weights ----------

def lcg_floats_centered(seed, n, scale=1.0):
    """Return n floats in roughly (-scale, +scale)."""
    s = seed
    out = []
    for _ in range(n):
        s = lcg_next(s)
        # use 31 bits -> [-1, 1] normalized then scaled
        out.append(((s & 0x3FFFFFFF) / float(0x20000000) - 1.0) * scale)
    return out


def lcg_ints_centered(seed, n, lo, hi):
    s = seed
    out = []
    span = hi - lo + 1
    for _ in range(n):
        s = lcg_next(s)
        out.append(lo + (s % span))
    return out


# ---------- Weight generation ----------

def make_reject_weights(seed):
    # Centered int16 weights in [-32, 32].
    return lcg_ints_centered(seed, B2_SLOTS, -32, 32)


def make_ref_vec(seed):
    """K3 reference vectors of dim E, each L2-normalized."""
    raw = lcg_floats_centered(seed, K3 * E, 1.0)
    out = [0.0] * (K3 * E)
    for k in range(K3):
        base = k * E
        block = raw[base:base + E]
        nrm = math.sqrt(sum(x * x for x in block))
        if nrm == 0:
            nrm = 1.0
        for j in range(E):
            out[base + j] = block[j] / nrm
    return out


def make_W_b(seed_w, seed_b, rows, cols):
    # Xavier-ish: scale by 1/sqrt(cols).
    scale = 1.0 / math.sqrt(cols)
    W = lcg_floats_centered(seed_w, rows * cols, scale)
    b = lcg_floats_centered(seed_b, rows, scale * 0.1)
    return W, b


def make_cos_table():
    tbl = [0.0] * (E * D)
    for j in range(E):
        for i in range(D):
            tbl[j * D + i] = math.cos(2.0 * math.pi * i * j / D)
    return tbl


# ---------- Calibration pass ----------

def calibrate(items_subsample, seed):
    """Run a calibration pre-scan to determine accept_bitmap / T2 / T3.

    items_subsample: list of items, each a list of D bytes.
    Returns (accept_bitmap, reject_weights, T2, ref_vec, T3, W1, b1, W2, b2,
             cos_table, actual_rates).
    """
    n_sub = len(items_subsample)
    sys.stderr.write(f"[gen_input] calibrating on subsample of {n_sub} items\n")

    # Stage 1 calibration: cumulative-frequency bitmap.
    # Each of 65536 possible sigs has a bit (one byte per 8 sigs, 8192
    # bytes total).  We count sig frequencies in the subsample, sort by
    # frequency descending, and set bits until the cumulative count
    # reaches 60% of n_sub.  Result: ~60% of items will have their sig
    # bit set and thus exit at S1.
    sig_count = [0] * SIG_RANGE
    item_sigs = [None] * n_sub
    for idx, it in enumerate(items_subsample):
        sig = sum(it) & 0xFFFF
        item_sigs[idx] = sig
        sig_count[sig] += 1
    # Sort sigs by frequency descending.
    order = sorted(range(SIG_RANGE), key=lambda s: -sig_count[s])
    accept_bitmap = bytearray(ACCEPT_BITMAP_BYTES)
    cum = 0
    target = int(EXPECTED_S1_RATE * n_sub)
    for sig in order:
        if sig_count[sig] == 0: break
        accept_bitmap[sig >> 3] |= (1 << (sig & 7))
        cum += sig_count[sig]
        if cum >= target: break

    # Measure S1 actual exit rate.
    n_s1_exit = 0
    s2_arrivals = []
    for it in items_subsample:
        if stage1_decide(it, accept_bitmap):
            n_s1_exit += 1
        else:
            s2_arrivals.append(it)
    actual_s1 = n_s1_exit / n_sub

    # Stage 2 calibration: compute scores on s2_arrivals, pick T2 = 75th
    # percentile so 25% of arrivals exit (= ~10% of N).
    reject_weights = make_reject_weights(seed + 1)
    s2_scores = [stage2_score(it, reject_weights) for it in s2_arrivals]
    s2_scores_sorted = sorted(s2_scores, reverse=True)
    k_top = max(1, int(0.25 * len(s2_scores_sorted)))
    T2 = s2_scores_sorted[k_top - 1] if s2_scores_sorted else 0
    n_s2_exit = sum(1 for s in s2_scores if s > T2)
    s3_arrivals = [it for it, s in zip(s2_arrivals, s2_scores) if s <= T2]
    actual_s2 = n_s2_exit / n_sub

    # Stage 3 calibration.
    cos_table = make_cos_table()
    ref_vec = make_ref_vec(seed + 2)
    s3_sims = []
    s3_embs = []
    for it in s3_arrivals:
        emb = stage3_embed(it, cos_table)
        _, best_sim = stage3_best(emb, ref_vec)
        s3_sims.append(best_sim)
        s3_embs.append(emb)
    s3_sims_sorted = sorted(s3_sims, reverse=True)
    # Target: of items reaching S3 (~30% of N), exit fraction s.t. only
    # ~5% of N reach S4.  S3 exit = 30% - 5% = 25% of N = 25/30 = 83.3%
    # of S3 arrivals.  This is the spec's 60/25/10/5 split with S4 = 5%.
    target_s3_exit_of_arrivals = 5.0 / 6.0
    k_top3 = max(1, int(target_s3_exit_of_arrivals * len(s3_sims_sorted)))
    T3 = s3_sims_sorted[k_top3 - 1] if s3_sims_sorted else 0.0
    n_s3_exit = sum(1 for s in s3_sims if s > T3)
    actual_s3 = n_s3_exit / n_sub
    actual_s4 = (len(s3_arrivals) - n_s3_exit) / n_sub

    # Stage 4 weights (calibration of weights doesn't change decisions
    # qualitatively; just deterministic).
    W1, b1 = make_W_b(seed + 3, seed + 4, H, E)
    W2, b2 = make_W_b(seed + 5, seed + 6, C, H)

    actual_rates = {
        "s1": actual_s1, "s2": actual_s2, "s3": actual_s3, "s4": actual_s4,
    }
    sys.stderr.write(
        f"[gen_input] calibration actual rates: "
        f"S1={actual_s1:.3f} S2={actual_s2:.3f} S3={actual_s3:.3f} S4={actual_s4:.3f}\n"
    )

    # Warn if any rate deviates more than 5 pp from target.
    targets = {"s1": EXPECTED_S1_RATE, "s2": EXPECTED_S2_RATE, "s3": EXPECTED_S3_RATE, "s4": EXPECTED_S4_REACH}
    for k, t in targets.items():
        if abs(actual_rates[k] - t) > 0.05:
            sys.stderr.write(
                f"[gen_input] WARN: stage {k} rate {actual_rates[k]:.3f} "
                f"deviates >5pp from target {t:.3f}\n"
            )

    return (accept_bitmap, reject_weights, T2, ref_vec, T3,
            W1, b1, W2, b2, cos_table, actual_rates)


# ---------- Full decision pass (for expected_checksum) ----------

def decide_all(items, params):
    (accept_bitmap, reject_weights, T2, ref_vec, T3,
     W1, b1, W2, b2, cos_table, _) = params
    cs = 0
    for it in items:
        if stage1_decide(it, accept_bitmap):
            d = ACCEPT_S1
        else:
            score = stage2_score(it, reject_weights)
            if score > T2:
                d = REJECT_S2
            else:
                emb = stage3_embed(it, cos_table)
                best, best_sim = stage3_best(emb, ref_vec)
                if best_sim > T3:
                    d = ACCEPT_S3_BASE | best
                else:
                    cls = stage4_classify(emb, W1, b1, W2, b2)
                    d = CLASS_BASE | cls
        cs = (cs + (d & 0xFFFFFFFF)) & 0xFFFFFFFF
    return cs


# ---------- Writers ----------

def write_input_bin(path, items):
    with open(path, "wb") as f:
        for it in items:
            f.write(bytes(it))


def write_weights_bin(path, accept_bitmap, reject_weights, ref_vec, W1, b1, W2, b2, cos_table):
    with open(path, "wb") as f:
        # accept_bitmap: 8192 bytes (one bit per possible sig in [0, 65536))
        f.write(bytes(accept_bitmap))
        # reject_weights: 256 * i16
        f.write(struct.pack(f"<{B2_SLOTS}h", *reject_weights))
        # ref_vec: K3*E * f64
        f.write(struct.pack(f"<{K3 * E}d", *ref_vec))
        # W1: H*E * f64
        f.write(struct.pack(f"<{H * E}d", *W1))
        # b1: H * f64
        f.write(struct.pack(f"<{H}d", *b1))
        # W2: C*H * f64
        f.write(struct.pack(f"<{C * H}d", *W2))
        # b2: C * f64
        f.write(struct.pack(f"<{C}d", *b2))
        # cos_table: E*D * f64
        f.write(struct.pack(f"<{E * D}d", *cos_table))


def write_dag_bin(path, n_chunks):
    """DAG layout: for each chunk k, 4 stage ops (S1..S4) with linear deps
    + a final aggregate op depending on all S4 ops.

    op_kind: 0=S1, 1=S2, 2=S3, 3=S4, 4=aggregate.
    chunk_id: 0..n_chunks-1 for stage ops, -1 for aggregate.
    src1_op_id: previous-stage op id within same chunk (or -1 for S1 / aggregate).
    level: 1..4 for stage ops, 5 for aggregate (USED ONLY BY STRAT/TIMELY —
           Ribault ignores level and reads `dep_op_ids` directly).
    """
    ops = []  # each: (kind, chunk_id, src1, level, deps)
    # Stage ops in chunk-major then stage-major order:
    #   op_id = chunk_id * 4 + (stage - 1)
    for k in range(n_chunks):
        # S1
        ops.append((0, k, -1, 1, []))
        # S2 depends on S1
        s1_id = len(ops) - 1
        ops.append((1, k, s1_id, 2, [s1_id]))
        # S3 depends on S2
        s2_id = len(ops) - 1
        ops.append((2, k, s2_id, 3, [s2_id]))
        # S4 depends on S3
        s3_id = len(ops) - 1
        ops.append((3, k, s3_id, 4, [s3_id]))
    # Aggregate depends on all S4 ops.
    s4_ids = [4 * k + 3 for k in range(n_chunks)]
    ops.append((4, -1, -1, 5, s4_ids))

    with open(path, "wb") as f:
        f.write(struct.pack("<i", len(ops)))
        for (kind, chunk_id, src1, level, deps) in ops:
            f.write(struct.pack("<5i", kind, chunk_id, src1, level, len(deps)))
            if deps:
                f.write(struct.pack(f"<{len(deps)}i", *deps))


def write_config(path, N, CHUNK_SIZE, T2, T3, SEED, actual_rates):
    with open(path, "w") as f:
        f.write(f"N {N}\n")
        f.write(f"D {D}\n")
        f.write(f"CHUNK_SIZE {CHUNK_SIZE}\n")
        f.write(f"B2_SLOTS {B2_SLOTS}\n")
        f.write(f"E {E}\n")
        f.write(f"K3 {K3}\n")
        f.write(f"H {H}\n")
        f.write(f"C_CLASSES {C}\n")
        f.write(f"ACCEPT_BITMAP_BYTES {ACCEPT_BITMAP_BYTES}\n")
        f.write(f"T2 {T2}\n")
        f.write(f"T3 {T3!r}\n")
        f.write(f"SEED {SEED}\n")
        f.write(f"EXPECTED_S1_RATE {EXPECTED_S1_RATE}\n")
        f.write(f"EXPECTED_S2_RATE {EXPECTED_S2_RATE}\n")
        f.write(f"EXPECTED_S3_RATE {EXPECTED_S3_RATE}\n")
        f.write(f"EXPECTED_S4_RATE {EXPECTED_S4_REACH}\n")
        f.write(f"ACTUAL_S1_RATE {actual_rates['s1']:.6f}\n")
        f.write(f"ACTUAL_S2_RATE {actual_rates['s2']:.6f}\n")
        f.write(f"ACTUAL_S3_RATE {actual_rates['s3']:.6f}\n")
        f.write(f"ACTUAL_S4_RATE {actual_rates['s4']:.6f}\n")


# ---------- Item generation ----------

def gen_items(N, seed):
    """Generate N items, each D bytes, deterministically from LCG."""
    # Each byte is one LCG step shifted. Stream of bytes is contiguous
    # across items so that the per-item sigs are well-distributed.
    items = [None] * N
    s = seed
    for k in range(N):
        item = bytearray(D)
        for i in range(D):
            s = lcg_next(s)
            item[i] = (s >> 16) & 0xFF
        items[k] = item
    return items


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--chunk-size", type=int, default=512)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--calib-cap", type=int, default=100_000,
                    help="cap subsample size for calibration (default 100K)")
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)

    sys.stderr.write(f"[gen_input] generating N={args.N} items, D={D}, seed={args.seed}\n")
    items = gen_items(args.N, args.seed)

    calib_n = min(args.calib_cap, args.N)
    params = calibrate(items[:calib_n], args.seed)
    (accept_bitmap, reject_weights, T2, ref_vec, T3,
     W1, b1, W2, b2, cos_table, actual_rates) = params

    sys.stderr.write(f"[gen_input] computing expected checksum over all N items\n")
    expected_cs = decide_all(items, params)

    write_input_bin(os.path.join(args.out_dir, "input.bin"), items)
    write_weights_bin(os.path.join(args.out_dir, "weights.bin"),
                      accept_bitmap, reject_weights, ref_vec, W1, b1, W2, b2, cos_table)
    write_dag_bin(os.path.join(args.out_dir, "dag.bin"),
                  (args.N + args.chunk_size - 1) // args.chunk_size)
    write_config(os.path.join(args.out_dir, "config.txt"),
                 args.N, args.chunk_size, T2, T3, args.seed, actual_rates)
    with open(os.path.join(args.out_dir, "expected_checksum.txt"), "w") as f:
        f.write(f"{expected_cs}\n")

    print(f"[gen_input] wrote {args.out_dir}/  N={args.N}  expected_checksum={expected_cs}  "
          f"n_chunks={(args.N + args.chunk_size - 1) // args.chunk_size}")


if __name__ == "__main__":
    main()
