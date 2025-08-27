#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, pathlib
import pandas as pd
import matplotlib.pyplot as plt
from pandas.errors import EmptyDataError

def safe_read_csv(path):
    p = pathlib.Path(path)
    if (not p.exists()) or p.stat().st_size == 0:
        return None
    try:
        return pd.read_csv(p)
    except EmptyDataError:
        return None

def plot_vs_n(df, ycol, title, out_path):
    if ycol not in df.columns:
        return False
    plt.figure()
    any_line = False
    for p, g in df.groupby("p"):
        g = g.sort_values("N")
        g = g[g[ycol].notna()]
        if g.empty: 
            continue
        plt.plot(g["N"], g[ycol], marker="o", label=f"p={p}")
        any_line = True
    if not any_line:
        plt.close()
        return False
    plt.xlabel("N")
    plt.ylabel(ycol)
    plt.title(title)
    plt.grid(True, alpha=0.3)
    plt.legend()
    plt.tight_layout()
    plt.savefig(out_path, dpi=150)
    plt.close()
    print(f"[plot] {out_path}")
    return True

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--title", default="Métricas vs N")
    args = ap.parse_args()

    outdir = pathlib.Path(args.outdir); outdir.mkdir(parents=True, exist_ok=True)
    df = safe_read_csv(args.metrics)
    if df is None or df.empty:
        print("nada para plotar"); return

    any_plot = False
    any_plot |= plot_vs_n(df, "mean_ms",    f"{args.title} — Runtime (ms) vs N", outdir/"runtime_vs_n.png")
    any_plot |= plot_vs_n(df, "speedup",    f"{args.title} — Speedup vs N",      outdir/"speedup_vs_n.png")
    any_plot |= plot_vs_n(df, "efficiency", f"{args.title} — Efficiency vs N",   outdir/"efficiency_vs_n.png")
    if not any_plot:
        print("nada para plotar")

if __name__ == "__main__":
    main()
