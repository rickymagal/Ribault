#!/usr/bin/env python3
"""Regenerate tables_with_ci.txt for the attention end-to-end benchmark
with per-language baselines and 95% bootstrap CIs.

Per-language tier layout (mirrors lcs_paper_final tables_with_ci.txt):

  Haskell tier:  Ribault-Hs (ribault_hs), GHC Strat (strategies)
                  vs  seq_haskell
  C tier:        Ribault-C (ribault_c)
                  vs  seq_c
  Rust tier:     Ribault-Rust (ribault_rust), Timely (timely),
                 Sucuri (sucuri) [if rows present]
                  vs  seq_rust

Bootstrap: 10000 resamples, INDEPENDENT resampling of baseline and
variant samples (the runner does not collect them in paired trials, so
independent resampling gives the conservative — slightly wider — CI
without assuming a phantom correlation). 95% percentile CI on the ratio
of medians.

Reads:  results/attn_paper_final/master.csv  (or path via --csv arg)
Writes: results/attn_paper_final/tables_with_ci.txt

Usage:
  python3 scripts/attention/regen_tables_ci.py
  python3 scripts/attention/regen_tables_ci.py \\
      --csv path/to/master.csv --out path/to/tables_with_ci.txt
"""
import argparse, csv, os, random, statistics, sys
from collections import defaultdict

DEFAULT_CSV = "results/attn_paper_final/master.csv"
DEFAULT_OUT = "results/attn_paper_final/tables_with_ci.txt"

# Per-language tier layout: (display tier name, baseline variant, [(csv_variant, display_name), ...])
LAYOUT = [
    ("Haskell", "seq_haskell", [
        ("ribault_hs",  "Ribault-Hs"),
        ("strategies",  "GHC Strat"),
    ]),
    ("C", "seq_c", [
        ("ribault_c",   "Ribault-C"),
    ]),
    ("Rust", "seq_rust", [
        ("ribault_rust", "Ribault-Rust"),
        ("timely",       "Timely"),
        ("sucuri",       "Sucuri"),
    ]),
]

BOOTSTRAP_B = 10000
SEED = 20260523  # matches LCS regen_tables_ci.py seed convention


def bootstrap_ci(seq_samples, par_samples, B=BOOTSTRAP_B, alpha=0.05):
    """Independent bootstrap of seq/par; statistic = median(seq) / median(par).
    Returns (median, lo, hi). 95% percentile CI by default."""
    ns, np_ = len(seq_samples), len(par_samples)
    ratios = []
    for _ in range(B):
        rs = [seq_samples[random.randrange(ns)] for _ in range(ns)]
        rp = [par_samples[random.randrange(np_)] for _ in range(np_)]
        ratios.append(statistics.median(rs) / statistics.median(rp))
    ratios.sort()
    lo = ratios[int((alpha/2) * B)]
    hi = ratios[int((1 - alpha/2) * B) - 1]
    med = statistics.median(seq_samples) / statistics.median(par_samples)
    return med, lo, hi


def load_csv(csv_path):
    """Load attention master.csv into data[variant][N][P] = [seconds, ...].
    Schema: variant,N,D,n_heads,n_blocks,P,rep,seconds,checksum,expected.
    Cross-validation: warns (does not abort) on any checksum mismatch."""
    data = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    cs_mismatches = 0
    with open(csv_path) as f:
        r = csv.DictReader(f)
        for row in r:
            v = row["variant"]
            N = int(row["N"])
            P = int(row["P"])
            try:
                t = float(row["seconds"])
            except ValueError:
                continue
            data[v][N][P].append(t)
            cs = row.get("checksum", "0")
            exp = row.get("expected", "0")
            if cs != exp and cs != "0" and exp != "0":
                cs_mismatches += 1
    if cs_mismatches:
        sys.stderr.write(
            f"[WARN] {cs_mismatches} rows have CHECKSUM != expected — see master.csv\n")
    return data


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", default=DEFAULT_CSV)
    ap.add_argument("--out", default=DEFAULT_OUT)
    ap.add_argument("--bootstrap", type=int, default=BOOTSTRAP_B)
    args = ap.parse_args()

    random.seed(SEED)
    B = args.bootstrap

    data = load_csv(args.csv)
    # Take Ns / Ps from the first parallel variant available; baselines
    # contribute only the P=1 column, which we read via per-tier lookup.
    parallel_keys = [v for _, _, vs in LAYOUT for v, _ in vs if v in data]
    if not parallel_keys:
        sys.stderr.write("[ERROR] no parallel variants in CSV — nothing to plot\n")
        sys.exit(2)
    pivot = parallel_keys[0]
    Ns = sorted(data[pivot].keys())
    Ps = sorted({p for N in Ns for p in data[pivot][N].keys() if p > 1})

    out = []
    out.append(f"ATTENTION END-TO-END — median speedup with 95% bootstrap CI "
               f"({B} independent resamples)")
    out.append("Format: median× [lo×, hi×]")
    out.append("")
    out.append("Per-language baselines:")
    out.append("  Haskell tier (Ribault-Hs, GHC Strat)             "
               "-> seq_haskell   (GHC 9.6.6 -O2 +RTS -A256m)")
    out.append("  C tier       (Ribault-C)                         "
               "-> seq_c         (gcc -O3 -march=native)")
    out.append("  Rust tier    (Ribault-Rust, Timely, Sucuri)      "
               "-> seq_rust      (rustc release, raw-pointer unsafe inner loops)")
    out.append("")

    for N in Ns:
        for lang, base_v, variants in LAYOUT:
            base_samples = data.get(base_v, {}).get(N, {}).get(1, [])
            if not base_samples:
                continue
            base_med = statistics.median(base_samples)
            n_base = len(base_samples)

            # Only include variants that have any rows at this N.
            active = [(v, disp) for v, disp in variants
                      if data.get(v, {}).get(N, {})]
            if not active:
                continue
            header_variants = " | ".join(f"{disp:>26}" for _, disp in active)
            out.append(
                f"=== N={N} | {lang} tier "
                f"(baseline = {base_v}, median = {base_med:.3f}s, n={n_base}) ===")
            out.append(f"   P | {header_variants}")

            for P in Ps:
                cells = []
                for v, disp in active:
                    par_samples = data[v][N].get(P, [])
                    if not par_samples:
                        cells.append(f"{'n/a':>26}")
                    else:
                        med, lo, hi = bootstrap_ci(base_samples, par_samples, B=B)
                        cells.append(f"{med:5.2f}× [{lo:5.2f}, {hi:5.2f}]".rjust(26))
                out.append(f"  {P:>2} | " + " | ".join(cells))
            out.append("")
        out.append("")

    os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)
    with open(args.out, "w") as f:
        f.write("\n".join(out) + "\n")
    print(f"wrote {args.out} ({len(out)} lines)")


if __name__ == "__main__":
    main()
