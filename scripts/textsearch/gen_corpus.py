#!/usr/bin/env python3
"""Generate a corpus of text files with a known keyword scattered at controlled positions."""

import argparse, os, random, string

WORDS = [
    "the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog",
    "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
    "lorem", "ipsum", "dolor", "sit", "amet", "consectetur", "adipiscing",
    "data", "stream", "buffer", "token", "parse", "graph", "node", "edge",
    "compute", "result", "value", "index", "array", "queue", "stack", "heap",
    "search", "match", "split", "merge", "sort", "filter", "reduce", "scan",
]


def gen_file(rng, file_size, keyword, density):
    """Return (text_bytes, keyword_count)."""
    parts = []
    total = 0
    count = 0
    while total < file_size:
        if rng.random() < density:
            parts.append(keyword)
            total += len(keyword)
            count += 1
        else:
            w = rng.choice(WORDS)
            parts.append(w)
            total += len(w)
        parts.append(" ")
        total += 1
    text = "".join(parts)[:file_size]
    # Recount in the truncated text
    actual = text.count(keyword)
    return text.encode("ascii"), actual


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

    total_count = 0
    manifest = []
    for i in range(args.n_files):
        data, cnt = gen_file(rng, args.file_size, args.keyword, args.density)
        fname = f"file_{i:04d}.txt"
        path = os.path.join(args.out_dir, fname)
        with open(path, "wb") as f:
            f.write(data)
        manifest.append((fname, cnt))
        total_count += cnt

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
