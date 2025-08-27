#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
import sys

import pandas as pd

# Use a non-interactive backend
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


def pick_runtime_col(df: pd.DataFrame) -> str:
    for c in ["dt_ms", "elapsed_ms", "runtime_ms", "time_ms"]:
        if c in df.columns:
            return c
    raise KeyError("No runtime column found (expected one of: dt_ms, elapsed_ms, runtime_ms, time_ms).")


def compute_speedup_efficiency(df: pd.DataFrame, rtcol: str) -> pd.DataFrame:
    """
    Expects columns: n, p, (rep), runtime column.
    Returns long table with columns: n, p, T1, Tp, speedup, efficiency.
    Uses mean across reps per (n, p).
    Only keeps (n) that have baseline p==1.
    """
    # mean runtime per (n, p)
    mean_rt = df.groupby(["n", "p"], as_index=False)[rtcol].mean().rename(columns={rtcol: "Tp"})
    # baseline T1 for each n
    base = mean_rt[mean_rt["p"] == 1][["n", "Tp"]].rename(columns={"Tp": "T1"})
    out = mean_rt.merge(base, on="n", how="inner")  # drop N without T1
    if out.empty:
        return out
    out["speedup"] = out["T1"] / out["Tp"]
    out["efficiency"] = out["speedup"] / out["p"]
    return out


def plot_lines(df: pd.DataFrame, ycol: str, title: str, ylabel: str, outpath: str):
    if df.empty:
        print(f"Nothing to plot for {ycol}.", file=sys.stderr)
        return

    plt.figure(figsize=(8, 5))
    for n, sub in df.groupby("n"):
        sub = sub.sort_values("p")
        plt.plot(sub["p"], sub[ycol], marker="o", label=f"N = {n}")
    plt.xlabel("#Threads (p)")
    plt.ylabel(ylabel)
    plt.title(title)
    plt.grid(True, linestyle="--", alpha=0.4)
    plt.legend(title="Input size", loc="best")
    os.makedirs(os.path.dirname(outpath), exist_ok=True)
    plt.tight_layout()
    plt.savefig(outpath, dpi=150)
    print(f"Plotted {ycol} -> {outpath}")


def main():
    ap = argparse.ArgumentParser(description="Plot speedup and efficiency vs threads.")
    ap.add_argument("--metrics", required=True, help="CSV with runs/metrics.")
    ap.add_argument("--out-speedup", required=True, help="Output PNG path for speedup plot.")
    ap.add_argument("--out-efficiency", required=True, help="Output PNG path for efficiency plot.")
    ap.add_argument("--title-base", default="Scaling", help="Base title used in figures.")
    ap.add_argument("--variant", choices=["asm", "super"], default=None,
                    help="Filter by variant.")
    ap.add_argument("--ns", nargs="*", type=int, default=None,
                    help="Filter by specific N values (e.g., --ns 500 1000 2000).")
    ap.add_argument("--threads", nargs="*", type=int, default=None,
                    help="Filter by thread counts (e.g., --threads 1 2 4 8).")
    args = ap.parse_args()

    # Load CSV robustly
    try:
        df = pd.read_csv(args.metrics)
    except pd.errors.EmptyDataError:
        print("Nothing to plot: metrics CSV is empty.", file=sys.stderr)
        return 0

    if df.empty:
        print("Nothing to plot: metrics CSV has no rows.", file=sys.stderr)
        return 0

    # Required columns
    needed = {"n", "p"}
    missing = [c for c in needed if c not in df.columns]
    if missing:
        print(f"Nothing to plot: missing required columns {missing}.", file=sys.stderr)
        return 0

    if args.variant:
        if "variant" not in df.columns:
            print("Warning: --variant given but 'variant' column not found; ignoring.", file=sys.stderr)
        else:
            df = df[df["variant"] == args.variant]

    if args.ns:
        df = df[df["n"].isin(args.ns)]
    if args.threads:
        df = df[df["p"].isin(args.threads)]

    if df.empty:
        print("Nothing to plot after filtering.", file=sys.stderr)
        return 0

    # runtime column
    try:
        rtcol = pick_runtime_col(df)
    except KeyError as e:
        print(f"Nothing to plot: {e}", file=sys.stderr)
        return 0

    se = compute_speedup_efficiency(df, rtcol)
    if se.empty:
        print("Nothing to plot: no baseline with p==1 found for any N.", file=sys.stderr)
        return 0

    plot_lines(se, "speedup", f"{args.title-base if hasattr(args,'title-base') else args.title_base} — Speedup",
               "Speedup (S_p = T1/Tp)", args.out_speedup)
    plot_lines(se, "efficiency", f"{args.title-base if hasattr(args,'title-base') else args.title_base} — Efficiency",
               "Efficiency (E_p = S_p / p)", args.out_efficiency)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
