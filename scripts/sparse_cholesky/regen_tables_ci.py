#!/usr/bin/env python3
"""Regenerate tables_with_ci.txt for the sparse-Cholesky benchmark with
per-language baselines and 95% bootstrap CIs.

Same layout as mergesort / LCS / attention CI scripts. Workload key here
is (NB, B) instead of N.

CSV schema: variant,NB,B,P,rep,seconds,checksum,expected
"""
import argparse, csv, os, random, statistics, sys
from collections import defaultdict

DEFAULT_CSV = "results/sparse_cholesky_paper_final/master.csv"
DEFAULT_OUT = "results/sparse_cholesky_paper_final/tables_with_ci.txt"

LAYOUT = [
    ("Haskell", "seq_haskell", [
        ("ribault_hs",  "Ribault-Hs"),
        ("strategies",  "GHC Strat"),
        ("parpseq",     "GHC par/pseq"),
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
SEED = 20260604


def bootstrap_ci(seq_samples, par_samples, B=BOOTSTRAP_B, alpha=0.05):
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
    # data[variant][(NB, B)][P] -> [seconds...]
    data = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    cs_mismatches = 0
    failed_runs = 0
    with open(csv_path) as f:
        r = csv.DictReader(f)
        for row in r:
            v = row["variant"]
            try: NB = int(row["NB"]); BB = int(row["B"]); P = int(row["P"])
            except (KeyError, ValueError): continue
            try: t = float(row["seconds"])
            except ValueError: continue
            cs = row.get("checksum", "0")
            exp = row.get("expected", "0")
            if t <= 0.0 or (cs != exp and cs != "0" and exp != "0"):
                failed_runs += 1
                if cs != exp and cs != "0" and exp != "0":
                    cs_mismatches += 1
                continue
            data[v][(NB, BB)][P].append(t)
    if failed_runs:
        sys.stderr.write(f"[INFO] dropped {failed_runs} failed/segfaulted runs ({cs_mismatches} checksum mismatch)\n")
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
    parallel_keys = [v for _, _, vs in LAYOUT for v, _ in vs if v in data]
    if not parallel_keys:
        sys.stderr.write("[ERROR] no parallel variants in CSV\n"); sys.exit(2)
    pivot = parallel_keys[0]
    Ws = sorted(data[pivot].keys())
    Ps = sorted({p for w in Ws for p in data[pivot][w].keys() if p > 1})

    out = []
    out.append(f"SPARSE CHOLESKY — median speedup with 95% bootstrap CI ({B} independent resamples)")
    out.append("Format: median× [lo×, hi×]   (workload key = NB × B)")
    out.append("")
    out.append("Per-language baselines:")
    out.append("  Haskell tier (Ribault-Hs, GHC Strat, GHC par/pseq)  -> seq_haskell  (GHC 9.6.6 -O2 +RTS -A256m)")
    out.append("  C tier       (Ribault-C)                            -> seq_c        (gcc -O3 -march=native)")
    out.append("  Rust tier    (Ribault-Rust, Timely, Sucuri)         -> seq_rust     (rustc release, raw-ptr unsafe inner loops)")
    out.append("")

    for w in Ws:
        NB, BB = w
        for lang, base_v, variants in LAYOUT:
            base_samples = data.get(base_v, {}).get(w, {}).get(1, [])
            if not base_samples: continue
            base_med = statistics.median(base_samples); n_base = len(base_samples)
            active = [(v, disp) for v, disp in variants if data.get(v, {}).get(w, {})]
            if not active: continue
            header = " | ".join(f"{disp:>26}" for _, disp in active)
            out.append(f"=== NB={NB} B={BB} | {lang} tier (baseline = {base_v}, median = {base_med:.3f}s, n={n_base}) ===")
            out.append(f"   P | {header}")
            for P in Ps:
                cells = []
                for v, disp in active:
                    par_samples = data[v][w].get(P, [])
                    if not par_samples:
                        cells.append(f"{'n/a':>26}")
                    else:
                        med, lo, hi = bootstrap_ci(base_samples, par_samples, B=B)
                        cells.append(f"{med:5.2f}× [{lo:5.2f}, {hi:5.2f}]".rjust(26))
                out.append(f"  {P:>2} | " + " | ".join(cells))
            out.append("")
        out.append("")

    all_variants = ["seq_haskell", "seq_c", "seq_rust",
                    "ribault_hs", "strategies", "parpseq",
                    "ribault_c",
                    "ribault_rust", "timely", "sucuri"]
    out.append("=" * 60)
    out.append("RAW MEDIAN WALLTIME (seconds)")
    out.append("=" * 60); out.append("")
    for w in Ws:
        NB, BB = w
        present = [v for v in all_variants if data.get(v, {}).get(w)]
        if not present: continue
        header = " | ".join(f"{v:>13}" for v in present)
        out.append(f"=== NB={NB} B={BB} ===")
        out.append(f"   P | {header}")
        for P in sorted({1} | set(Ps)):
            cells = []
            for v in present:
                samples = data[v][w].get(P, [])
                if not samples: cells.append(f"{'n/a':>13}")
                else:           cells.append(f"{statistics.median(samples):11.3f}s".rjust(13))
            out.append(f"  {P:>2} | " + " | ".join(cells))
        out.append("")
    out.append("")

    # Head-to-head: Ribault-Rust vs Timely (this is where Ribault's irregular-DAG
    # advantage should be most visible — Timely pays a barrier at each epoch).
    out.append("=" * 60)
    out.append("HEAD-TO-HEAD: Ribault-Rust vs Timely")
    out.append("Ratio = T_timely / T_ribault_rust  (>1 means Ribault-Rust is faster)")
    out.append("=" * 60); out.append("")
    for w in Ws:
        NB, BB = w
        rh = data.get("ribault_rust", {}).get(w, {})
        st = data.get("timely", {}).get(w, {})
        if not rh or not st: continue
        out.append(f"=== NB={NB} B={BB} ===")
        out.append(f"   P |  T_ribault_rs |      T_timely | T_tim/T_rrs (median × [lo, hi])")
        for P in Ps:
            rhs = rh.get(P, []); sts = st.get(P, [])
            if not rhs or not sts:
                out.append(f"  {P:>2} | {'n/a':>13} | {'n/a':>13} | {'n/a':>30}")
                continue
            med, lo, hi = bootstrap_ci(sts, rhs, B=B)
            rh_med = statistics.median(rhs); st_med = statistics.median(sts)
            tag = "  <- Ribault-Rust faster" if med > 1.0 else "  <- Timely faster"
            out.append(f"  {P:>2} | {rh_med:11.3f}s | {st_med:11.3f}s | "
                       f"{med:5.2f}× [{lo:5.2f}, {hi:5.2f}]{tag}")
        out.append("")
    out.append("")

    out.append("=" * 60)
    out.append("HEAD-TO-HEAD: Ribault-Hs vs GHC Strategies")
    out.append("=" * 60); out.append("")
    for w in Ws:
        NB, BB = w
        rh = data.get("ribault_hs", {}).get(w, {})
        st = data.get("strategies", {}).get(w, {})
        if not rh or not st: continue
        out.append(f"=== NB={NB} B={BB} ===")
        out.append(f"   P |  T_ribault_hs |    T_strat | T_strat/T_rhs")
        for P in Ps:
            rhs = rh.get(P, []); sts = st.get(P, [])
            if not rhs or not sts:
                out.append(f"  {P:>2} | {'n/a':>13} | {'n/a':>13} | {'n/a':>30}")
                continue
            med, lo, hi = bootstrap_ci(sts, rhs, B=B)
            rh_med = statistics.median(rhs); st_med = statistics.median(sts)
            tag = "  <- Ribault-Hs faster" if med > 1.0 else "  <- STRAT faster"
            out.append(f"  {P:>2} | {rh_med:11.3f}s | {st_med:11.3f}s | "
                       f"{med:5.2f}× [{lo:5.2f}, {hi:5.2f}]{tag}")
        out.append("")

    os.makedirs(os.path.dirname(args.out) or ".", exist_ok=True)
    with open(args.out, "w") as f:
        f.write("\n".join(out) + "\n")
    print(f"wrote {args.out} ({len(out)} lines)")


if __name__ == "__main__":
    main()
