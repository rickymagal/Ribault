#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import csv
import os
import sys
from pathlib import Path

import pandas as pd


def discover_runs(root: Path):
    """Walk result tree and collect run.json/csv-like data."""
    rows = []
    for p in root.rglob("*/rep_*/run.csv"):
        # expected columns: variant,n,p,rep,dt_ms (and optionally others)
        try:
            df = pd.read_csv(p)
        except Exception:
            continue
        for _, r in df.iterrows():
            rows.append(dict(r))
    return rows


def main():
    ap = argparse.ArgumentParser(description="Collect raw runs and produce metrics CSV.")
    ap.add_argument("--root", required=True, help="Root path to scan (results dir).")
    ap.add_argument("--raw", required=True, help="Output CSV with all raw rows.")
    ap.add_argument("--out", required=True, help="Output CSV with aggregated metrics.")
    args = ap.parse_args()

    root = Path(args.root)
    rows = discover_runs(root)

    if not rows:
        # still create empty files that are valid CSVs
        empty_cols = ["variant", "n", "p", "rep", "dt_ms"]
        pd.DataFrame(columns=empty_cols).to_csv(args.raw, index=False)
        pd.DataFrame(columns=empty_cols).to_csv(args.out, index=False)
        print(f"[collect] No runs found under {root}. Wrote empty CSVs.")
        return 0

    raw = pd.DataFrame(rows)
    raw.to_csv(args.raw, index=False)

    # normalize column names if present with different spellings
    if "elapsed_ms" in raw.columns and "dt_ms" not in raw.columns:
        raw = raw.rename(columns={"elapsed_ms": "dt_ms"})
    if "runtime_ms" in raw.columns and "dt_ms" not in raw.columns:
        raw = raw.rename(columns={"runtime_ms": "dt_ms"})

    # Simple aggregated metrics (mean per variant/n/p)
    have_rep = "rep" in raw.columns
    gcols = ["variant", "n", "p"] + (["rep"] if not have_rep else [])
    agg = raw.groupby(["variant", "n", "p"], as_index=False)["dt_ms"].mean().rename(columns={"dt_ms": "mean_dt_ms"})

    agg.to_csv(args.out, index=False)
    print(f"[collect] OK: {args.out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
