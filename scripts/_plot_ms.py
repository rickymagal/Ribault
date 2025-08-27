#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import os
import sys
import math

import pandas as pd

# Use a non-interactive backend
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


def pick_runtime_col(df: pd.DataFrame) -> str:
    """Choose the best available runtime column (ms)."""
    for c in ["dt_ms", "elapsed_ms", "runtime_ms", "time_ms"]:
        if c in df.columns:
            return c
    raise KeyError("No runtime column found (expected one of: dt_ms, elapsed_ms, runtime_ms, time_ms).")


def main():
    ap = argparse.ArgumentParser(description="Plot runtime vs N.")
    ap.add_argument("--metrics", required=True, help="CSV with runs/metrics.")
    ap.add_argument("--out", required=True, help="Output PNG path.")
    ap.add_argument("--title", default="Runtime vs N", help="Figure title (English).")
    ap.add_argument("--threads", nargs="*", type=int, default=None,
                    help="Filter by thread counts (e.g., --threads 1 2 4 8).")
    ap.add_argument("--variant", choices=["asm", "super"], default=None,
                    help="Filter by variant.")
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

    # Required columns (with fallback for runtime)
    needed = {"n", "p"}
    missing = [c for c in needed if c not in df.columns]
    if missing:
        print(f"Nothing to plot: missing required columns {missing}.", file=sys.stderr)
        return 0

    # Filter variant and threads if asked
    if args.variant:
        if "variant" not in df.columns:
            print("Warning: --variant given but 'variant' column not found; ignoring.", file=sys.stderr)
        else:
            df = df[df["variant"] == args.variant]

    if args.threads:
        df = df[df["p"].isin(args.threads)]

    if df.empty:
        print("Nothing to plot after filtering.", file=sys.stderr)
        return 0

    # Pick runtime column
    try:
        rtcol = pick_runtime_col(df)
    except KeyError as e:
        print(f"Nothing to plot: {e}", file=sys.stderr)
        return 0

    # Aggregate by (n, p): average across reps
    group_cols = ["n", "p"]
    agg = df.groupby(group_cols, as_index=False)[rtcol].mean()
    agg = agg.sort_values(["p", "n"])

    # Build plot
    plt.figure(figsize=(8, 5))
    # Lines: one per p
    for p, sub in agg.groupby("p"):
        sub = sub.sort_values("n")
        plt.plot(sub["n"], sub[rtcol], marker="o", label=f"{p} threads")

    plt.xlabel("Input size N")
    plt.ylabel("Runtime (ms)")
    plt.title(args.title)
    plt.grid(True, linestyle="--", alpha=0.4)
    plt.legend(title="Parallelism", loc="best")

    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    plt.tight_layout()
    plt.savefig(args.out, dpi=150)
    print(f"Plotted runtime vs N -> {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
