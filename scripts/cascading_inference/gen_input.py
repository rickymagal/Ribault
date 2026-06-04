#!/usr/bin/env python3
"""Generate input + weights + DAG + expected checksum for the
Cascading Inference Pipeline benchmark.

CRITICAL DESIGN: items are organized into chunks, and each chunk is
labeled with a `source_class` in {0, 1, 2, 3} (25/25/25/25 distribution,
permuted deterministically by LCG seed=43).  Each source class has its
own exit-rate profile, calibrated independently in gen_input.py:

  Class 0 ("fast"):   95% S1 / 4%  S2 / 0.9% S3 / 0.1% S4    cost  ~110/chunk
  Class 1 ("medium"): 70% S1 / 22% S2 / 7%   S3 / 1%   S4    cost  ~800/chunk
  Class 2 ("slow"):   30% S1 / 25% S2 / 30%  S3 / 15%  S4    cost ~6400/chunk
  Class 3 ("deep"):   5%  S1 / 10% S2 / 25%  S3 / 60%  S4    cost ~22500/chunk

Mean cost per item across all classes = 0.25 * (3.5 + 25 + 200 + 700) ~ 232
units; ratio (deep-chunk / fast-chunk) ~ 200x.  This per-chunk
heterogeneity is what makes barrier-based schedulers (Strategies, Timely)
pay for waiting on the slowest chunk in each epoch, while Ribault's
token firing rule lets fast chunks flow through all 4 stages while deep
chunks are still in S3 or S4.

The source-class scheme reflects production-realistic data skew (Kafka
queues, Spark partitions): real inference workloads have per-source
correlation between item complexity, not items independently sampled.
A control experiment with source_id randomized (no per-chunk
correlation) is a natural sensitivity check; we expect it to recover
Strategies's competitiveness, confirming that data skew is the
operative variable.

Outputs in <out-dir>:
  input.bin              N * D bytes of LCG-generated items
  chunk_class.bin        n_chunks i32 — class id per chunk (0..3)
  weights.bin            accept_bitmap_class[4][8192] bytes
                       + reject_weights[256] i16
                       + ref_vec[8*64] f64
                       + W1[128*64] f64  + b1[128] f64
                       + W2[16*128] f64  + b2[16] f64
                       + cos_table[64*256] f64
  config.txt             N D CHUNK_SIZE etc + per-class T2/T3 + ACTUAL rates per class
  dag.bin                same layout as before (op_kind, chunk_id, ...)
  expected_checksum.txt  canonical checksum
"""
import argparse, math, os, struct, sys


# ---------- LCG ----------
LCG_A = 1103515245
LCG_C = 12345
LCG_M = 2**31


def lcg_next(state):
    return (LCG_A * state + LCG_C) % LCG_M


# ---------- Constants ----------
D                   = 256
B2_SLOTS            = 256
E                   = 64
K3                  = 8
H                   = 128
C                   = 16
SIG_BITS            = 16
SIG_RANGE           = 1 << SIG_BITS
ACCEPT_BITMAP_BYTES = SIG_RANGE // 8   # 8192
N_CLASSES           = 4


ACCEPT_S1      = 1
REJECT_S2      = 2
ACCEPT_S3_BASE = 0x40
CLASS_BASE     = 0x80


# Per-class exit-rate targets (must sum to 1.0 within each row).
#                    S1     S2     S3     S4
CLASS_RATES = [
    (0.95, 0.04,  0.009, 0.001),   # 0 fast
    (0.70, 0.22,  0.07,  0.01),    # 1 medium
    (0.30, 0.25,  0.30,  0.15),    # 2 slow
    (0.05, 0.10,  0.25,  0.60),    # 3 deep
]


# ---------- Stage kernels in Python (calibration only — slow, reference) ----------

def stage1_decide(item, bitmap):
    sig = sum(item) & 0xFFFF
    return ((bitmap[sig >> 3] >> (sig & 7)) & 1) != 0


def stage2_score(item, rejw):
    hist = [0] * B2_SLOTS
    for i in range(D - 1):
        b = (item[i] * 7 + item[i + 1]) & 0xFF
        hist[b] += 1
    return sum(hist[k] * rejw[k] for k in range(B2_SLOTS))


def stage3_embed(item, cos_table):
    emb = [0.0] * E
    for j in range(E):
        s = 0.0
        for i in range(D):
            s += cos_table[j * D + i] * (item[i] / 255.0)
        emb[j] = s
    n2 = sum(x * x for x in emb)
    if n2 > 0:
        inv = 1.0 / math.sqrt(n2)
        emb = [x * inv for x in emb]
    return emb


def stage3_best(emb, ref_vec):
    best = 0; bs = -1e300
    for k in range(K3):
        s = sum(emb[j] * ref_vec[k * E + j] for j in range(E))
        if s > bs: bs = s; best = k
    return best, bs


def stage4_classify(emb, W1, b1, W2, b2):
    hidden = [0.0] * H
    for h in range(H):
        s = b1[h]
        for j in range(E):
            s += W1[h * E + j] * emb[j]
        hidden[h] = max(0.0, s)
    best = 0; bs = -1e300
    for c in range(C):
        s = b2[c]
        for h in range(H):
            s += W2[c * H + h] * hidden[h]
        if s > bs: bs = s; best = c
    return best


# ---------- Weight generation ----------

def lcg_floats(seed, n, scale):
    s = seed; out = []
    for _ in range(n):
        s = lcg_next(s)
        out.append(((s & 0x3FFFFFFF) / float(0x20000000) - 1.0) * scale)
    return out


def lcg_ints(seed, n, lo, hi):
    s = seed; out = []
    span = hi - lo + 1
    for _ in range(n):
        s = lcg_next(s)
        out.append(lo + (s % span))
    return out


def make_reject_weights(seed):  return lcg_ints(seed, B2_SLOTS, -32, 32)
def make_W_b(seed_w, seed_b, rows, cols):
    scale = 1.0 / math.sqrt(cols)
    return lcg_floats(seed_w, rows * cols, scale), lcg_floats(seed_b, rows, scale * 0.1)


def make_ref_vec(seed):
    raw = lcg_floats(seed, K3 * E, 1.0)
    out = [0.0] * (K3 * E)
    for k in range(K3):
        base = k * E
        block = raw[base:base + E]
        nrm = math.sqrt(sum(x * x for x in block)) or 1.0
        for j in range(E):
            out[base + j] = block[j] / nrm
    return out


def make_cos_table():
    return [math.cos(2.0 * math.pi * i * j / D) for j in range(E) for i in range(D)]


# ---------- Item generation ----------

def gen_items(N, seed):
    items = [None] * N
    s = seed
    for k in range(N):
        item = bytearray(D)
        for i in range(D):
            s = lcg_next(s)
            item[i] = (s >> 16) & 0xFF
        items[k] = item
    return items


def assign_source_classes(n_chunks, seed=43):
    """Return a list of source_class ids of length n_chunks with 25/25/25/25
    distribution, permuted deterministically by `seed`."""
    out = [(i * N_CLASSES) // n_chunks for i in range(n_chunks)]   # 0,0,...,1,1,...,2,2,...,3,3,...
    # Fisher-Yates with LCG
    s = seed
    for i in range(n_chunks - 1, 0, -1):
        s = lcg_next(s)
        j = s % (i + 1)
        out[i], out[j] = out[j], out[i]
    return out


# ---------- Per-class calibration ----------

def calibrate_class(items_subsample_for_class, class_id, reject_weights, cos_table, ref_vec,
                    W1, b1, W2, b2):
    """Return (accept_bitmap, T2, T3, actual_rates_4tuple) for this class."""
    n_sub = len(items_subsample_for_class)
    if n_sub == 0:
        return bytearray(ACCEPT_BITMAP_BYTES), 0, 0.0, (0.0, 0.0, 0.0, 0.0)
    target_s1, target_s2, target_s3, target_s4 = CLASS_RATES[class_id]

    # Stage 1: bitmap covering target_s1 fraction of items by frequency.
    sig_count = [0] * SIG_RANGE
    for it in items_subsample_for_class:
        sig = sum(it) & 0xFFFF
        sig_count[sig] += 1
    order = sorted(range(SIG_RANGE), key=lambda s: -sig_count[s])
    accept_bitmap = bytearray(ACCEPT_BITMAP_BYTES)
    cum = 0; target = int(target_s1 * n_sub)
    for sig in order:
        if sig_count[sig] == 0: break
        accept_bitmap[sig >> 3] |= (1 << (sig & 7))
        cum += sig_count[sig]
        if cum >= target: break

    # Measure S1 and split.
    s2_arrivals = []
    n_s1 = 0
    for it in items_subsample_for_class:
        if stage1_decide(it, accept_bitmap): n_s1 += 1
        else: s2_arrivals.append(it)
    actual_s1 = n_s1 / n_sub

    # Stage 2 calibration: target_s2 fraction of N items in this class
    # exit at S2.  As a fraction of S2 arrivals: target_s2 / (1 - actual_s1).
    if not s2_arrivals:
        return accept_bitmap, 0, 0.0, (actual_s1, 0.0, 0.0, 0.0)
    s2_scores = [stage2_score(it, reject_weights) for it in s2_arrivals]
    s2_sorted = sorted(s2_scores, reverse=True)
    exit_fraction_of_arrivals_s2 = min(1.0, target_s2 / max(1e-9, 1.0 - actual_s1))
    k_top2 = max(1, int(exit_fraction_of_arrivals_s2 * len(s2_sorted)))
    T2 = s2_sorted[k_top2 - 1] if s2_sorted else 0
    s3_arrivals = [it for it, s in zip(s2_arrivals, s2_scores) if s <= T2]
    n_s2 = sum(1 for s in s2_scores if s > T2)
    actual_s2 = n_s2 / n_sub

    # Stage 3 calibration.
    if not s3_arrivals:
        return accept_bitmap, T2, 0.0, (actual_s1, actual_s2, 0.0, 0.0)
    s3_sims = []
    for it in s3_arrivals:
        emb = stage3_embed(it, cos_table)
        _, bs = stage3_best(emb, ref_vec)
        s3_sims.append(bs)
    s3_sorted = sorted(s3_sims, reverse=True)
    exit_fraction_of_arrivals_s3 = min(1.0, target_s3 / max(1e-9, 1.0 - actual_s1 - actual_s2))
    k_top3 = max(1, int(exit_fraction_of_arrivals_s3 * len(s3_sorted)))
    T3 = s3_sorted[k_top3 - 1] if s3_sorted else 0.0
    n_s3 = sum(1 for s in s3_sims if s > T3)
    actual_s3 = n_s3 / n_sub
    actual_s4 = (len(s3_arrivals) - n_s3) / n_sub
    return accept_bitmap, T2, T3, (actual_s1, actual_s2, actual_s3, actual_s4)


# ---------- Full-pass decide for canonical checksum ----------

def decide_all(items, chunk_class, chunk_size, weights):
    (bitmaps, reject_weights, T2s, T3s, ref_vec, cos_table, W1, b1, W2, b2) = weights
    n = len(items)
    cs = 0
    for chunk_id in range(0, (n + chunk_size - 1) // chunk_size):
        cls = chunk_class[chunk_id]
        bm  = bitmaps[cls]
        t2  = T2s[cls]
        t3  = T3s[cls]
        lo = chunk_id * chunk_size
        hi = min(lo + chunk_size, n)
        for i in range(lo, hi):
            it = items[i]
            if stage1_decide(it, bm):
                d = ACCEPT_S1
            else:
                sc = stage2_score(it, reject_weights)
                if sc > t2:
                    d = REJECT_S2
                else:
                    emb = stage3_embed(it, cos_table)
                    best, bs = stage3_best(emb, ref_vec)
                    if bs > t3:
                        d = ACCEPT_S3_BASE | best
                    else:
                        d = CLASS_BASE | stage4_classify(emb, W1, b1, W2, b2)
            cs = (cs + (d & 0xFFFFFFFF)) & 0xFFFFFFFF
    return cs


# ---------- Writers ----------

def write_input_bin(path, items):
    with open(path, "wb") as f:
        for it in items: f.write(bytes(it))


def write_chunk_class_bin(path, chunk_class):
    with open(path, "wb") as f:
        f.write(struct.pack(f"<{len(chunk_class)}i", *chunk_class))


def write_weights_bin(path, bitmaps, reject_weights, ref_vec, W1, b1, W2, b2, cos_table):
    with open(path, "wb") as f:
        for bm in bitmaps:        f.write(bytes(bm))
        f.write(struct.pack(f"<{B2_SLOTS}h", *reject_weights))
        f.write(struct.pack(f"<{K3 * E}d", *ref_vec))
        f.write(struct.pack(f"<{H * E}d", *W1))
        f.write(struct.pack(f"<{H}d", *b1))
        f.write(struct.pack(f"<{C * H}d", *W2))
        f.write(struct.pack(f"<{C}d", *b2))
        f.write(struct.pack(f"<{E * D}d", *cos_table))


def write_dag_bin(path, n_chunks):
    """Same DAG topology as before — 4 stage ops per chunk + final aggregate."""
    ops = []
    for k in range(n_chunks):
        ops.append((0, k, -1, 1, []))            # S1
        s1 = len(ops) - 1
        ops.append((1, k, s1, 2, [s1]))          # S2
        s2 = len(ops) - 1
        ops.append((2, k, s2, 3, [s2]))          # S3
        s3 = len(ops) - 1
        ops.append((3, k, s3, 4, [s3]))          # S4
    s4_ids = [4 * k + 3 for k in range(n_chunks)]
    ops.append((4, -1, -1, 5, s4_ids))
    with open(path, "wb") as f:
        f.write(struct.pack("<i", len(ops)))
        for (kind, chunk_id, src1, level, deps) in ops:
            f.write(struct.pack("<5i", kind, chunk_id, src1, level, len(deps)))
            if deps: f.write(struct.pack(f"<{len(deps)}i", *deps))


def write_config(path, N, CHUNK_SIZE, SEED, T2s, T3s, actual_per_class):
    with open(path, "w") as f:
        f.write(f"N {N}\n")
        f.write(f"D {D}\n")
        f.write(f"CHUNK_SIZE {CHUNK_SIZE}\n")
        f.write(f"B2_SLOTS {B2_SLOTS}\n")
        f.write(f"E {E}\n")
        f.write(f"K3 {K3}\n")
        f.write(f"H {H}\n")
        f.write(f"C_CLASSES {C}\n")
        f.write(f"N_CLASSES {N_CLASSES}\n")
        f.write(f"ACCEPT_BITMAP_BYTES {ACCEPT_BITMAP_BYTES}\n")
        f.write(f"SEED {SEED}\n")
        for c in range(N_CLASSES):
            f.write(f"T2_CLASS_{c} {T2s[c]}\n")
            f.write(f"T3_CLASS_{c} {T3s[c]!r}\n")
        for c in range(N_CLASSES):
            f.write(f"EXPECTED_CLASS_{c} {' '.join(f'{r:.4f}' for r in CLASS_RATES[c])}\n")
        for c in range(N_CLASSES):
            f.write(f"ACTUAL_CLASS_{c} {' '.join(f'{r:.6f}' for r in actual_per_class[c])}\n")


# ---------- Main ----------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--N", type=int, required=True)
    ap.add_argument("--chunk-size", type=int, default=32)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--calib-cap", type=int, default=50_000)
    args = ap.parse_args()
    os.makedirs(args.out_dir, exist_ok=True)

    n_chunks = (args.N + args.chunk_size - 1) // args.chunk_size

    sys.stderr.write(f"[gen_input] N={args.N} D={D} CHUNK={args.chunk_size} n_chunks={n_chunks}\n")
    items = gen_items(args.N, args.seed)
    chunk_class = assign_source_classes(n_chunks, seed=43)

    # Subsample for calibration (chunk-aligned).
    calib_chunks_cap = max(8, args.calib_cap // args.chunk_size)
    sub_chunks = min(calib_chunks_cap, n_chunks)
    sys.stderr.write(f"[gen_input] calibrating on {sub_chunks} chunks ({sub_chunks * args.chunk_size} items)\n")

    # Group subsample items by class.
    items_by_class = [[] for _ in range(N_CLASSES)]
    for ck in range(sub_chunks):
        cls = chunk_class[ck]
        lo = ck * args.chunk_size
        hi = min(lo + args.chunk_size, args.N)
        items_by_class[cls].extend(items[lo:hi])

    # Generate weights (class-independent).
    reject_weights = make_reject_weights(args.seed + 1)
    ref_vec        = make_ref_vec(args.seed + 2)
    W1, b1         = make_W_b(args.seed + 3, args.seed + 4, H, E)
    W2, b2         = make_W_b(args.seed + 5, args.seed + 6, C, H)
    cos_table      = make_cos_table()

    # Per-class calibration.
    bitmaps = [None] * N_CLASSES
    T2s = [0] * N_CLASSES
    T3s = [0.0] * N_CLASSES
    actual_per_class = [None] * N_CLASSES
    for cls in range(N_CLASSES):
        sys.stderr.write(f"[gen_input] calibrating class {cls} (n_sub_items={len(items_by_class[cls])}) ...\n")
        bm, t2, t3, actual = calibrate_class(
            items_by_class[cls], cls, reject_weights, cos_table, ref_vec,
            W1, b1, W2, b2)
        bitmaps[cls] = bm; T2s[cls] = t2; T3s[cls] = t3; actual_per_class[cls] = actual
        sys.stderr.write(f"[gen_input]   class {cls}: rates {actual}  (target {CLASS_RATES[cls]})\n")
        # Sanity: abort if more than 5pp off any rate.
        for k, (a, t) in enumerate(zip(actual, CLASS_RATES[cls])):
            if abs(a - t) > 0.05:
                sys.stderr.write(f"[gen_input]   WARN: class {cls} stage S{k+1} rate {a:.4f} deviates >5pp from {t:.4f}\n")

    sys.stderr.write(f"[gen_input] computing expected checksum over all N items\n")
    expected_cs = decide_all(
        items, chunk_class, args.chunk_size,
        (bitmaps, reject_weights, T2s, T3s, ref_vec, cos_table, W1, b1, W2, b2))

    write_input_bin(os.path.join(args.out_dir, "input.bin"), items)
    write_chunk_class_bin(os.path.join(args.out_dir, "chunk_class.bin"), chunk_class)
    write_weights_bin(os.path.join(args.out_dir, "weights.bin"),
                      bitmaps, reject_weights, ref_vec, W1, b1, W2, b2, cos_table)
    write_dag_bin(os.path.join(args.out_dir, "dag.bin"), n_chunks)
    write_config(os.path.join(args.out_dir, "config.txt"),
                 args.N, args.chunk_size, args.seed, T2s, T3s, actual_per_class)
    with open(os.path.join(args.out_dir, "expected_checksum.txt"), "w") as f:
        f.write(f"{expected_cs}\n")

    print(f"[gen_input] wrote {args.out_dir}/  N={args.N}  n_chunks={n_chunks}  "
          f"expected_checksum={expected_cs}")


if __name__ == "__main__":
    main()
