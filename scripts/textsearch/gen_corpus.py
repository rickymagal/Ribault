#!/usr/bin/env python3
"""Generate a corpus of text files with a known keyword scattered at controlled positions.

Fast generation: pre-builds a base block of random lowercase words, tiles it
for each file, then injects uppercase keyword at random positions.  Since the
base text is all lowercase, the uppercase keyword cannot appear by chance —
count = number of injections (exact).
"""

import argparse, os, random

WORDS = [
    "the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog",
    "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
    "lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscing",
    "data", "stream", "buffer", "token", "parse", "graph", "node", "edge",
    "compute", "result", "value", "index", "array", "queue", "stack", "heap",
    "search", "match", "split", "merge", "sort", "filter", "reduce", "scan",
]

BASE_BLOCK_SIZE = 1 << 20  # 1 MB


def _build_base_block(rng):
    """Build a ~1MB block of random lowercase words (no uppercase)."""
    parts = []
    total = 0
    while total < BASE_BLOCK_SIZE:
        w = rng.choice(WORDS)
        parts.append(w)
        parts.append(" ")
        total += len(w) + 1
    return "".join(parts).encode("ascii")


def gen_file(base_block, rng, file_size, keyword_bytes, density):
    """Generate one file: tile base block, inject keyword. Returns (bytes, count)."""
    blen = len(base_block)
    # Tile base block to fill file_size
    reps = file_size // blen + 1
    data = bytearray((base_block * reps)[:file_size])

    # Inject keyword at random positions (non-overlapping)
    kw_len = len(keyword_bytes)
    count = 0
    avg_gap = int(1.0 / density) if density > 0 else file_size
    pos = rng.randint(0, min(avg_gap, file_size - 1))
    while pos <= file_size - kw_len:
        data[pos:pos + kw_len] = keyword_bytes
        count += 1
        gap = rng.randint(max(1, avg_gap // 2), avg_gap * 2)
        pos += kw_len + gap

    return bytes(data), count


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--n-files", type=int, default=200)
    ap.add_argument("--file-size", type=int, default=50000)
    ap.add_argument("--keyword", default="FINDME")
    ap.add_argument("--density", type=float, default=0.002)
    ap.add_argument("--out-dir", default="corpus")
    ap.add_argument("--seed", type=int, default=42)
    args = ap.parse_args()

    os.makedirs(args.out_dir, exist_ok=True)
    rng = random.Random(args.seed)

    keyword_bytes = args.keyword.encode("ascii")
    base_block = _build_base_block(rng)

    # Determine padding width
    pad_width = max(4, len(str(args.n_files - 1)))

    total_count = 0
    manifest = []
    for i in range(args.n_files):
        data, cnt = gen_file(base_block, rng, args.file_size, keyword_bytes, args.density)
        fname = f"file_{i:0{pad_width}d}.txt"
        path = os.path.join(args.out_dir, fname)
        with open(path, "wb") as f:
            f.write(data)
        manifest.append((fname, cnt))
        total_count += cnt
        if (i + 1) % 500 == 0:
            print(f"  [{i+1}/{args.n_files}] generated...")

    with open(os.path.join(args.out_dir, "manifest.txt"), "w") as f:
        for fname, cnt in manifest:
            f.write(f"{fname} {cnt}\n")

    with open(os.path.join(args.out_dir, "expected_total.txt"), "w") as f:
        f.write(f"{total_count}\n")

    print(f"[gen_corpus] {args.n_files} files, ~{args.file_size}B each, "
          f"keyword='{args.keyword}', total occurrences={total_count}")
    print(f"[gen_corpus] output: {args.out_dir}/")


if __name__ == "__main__":
    main()
