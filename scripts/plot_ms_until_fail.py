#!/usr/bin/env python3
import argparse
from pathlib import Path
import pandas as pd
import matplotlib.pyplot as plt

def main():
    ap = argparse.ArgumentParser(description="Plots simples ms_until_fail.")
    ap.add_argument("--csv", required=True)
    ap.add_argument("--outdir", required=True)
    args = ap.parse_args()

    outdir = Path(args.outdir); outdir.mkdir(parents=True, exist_ok=True)
    df = pd.read_csv(args.csv)

    # Plot 1: tempo médio vs P para alguns N
    for variant in sorted(df["variant"].unique()):
        sub = df[df["variant"]==variant]
        for n in sorted(sub["n"].unique()):
            d = sub[sub["n"]==n].sort_values("p")
            plt.figure()
            plt.plot(d["p"], d["mean_dt_ms"], marker="o")
            plt.xlabel("P"); plt.ylabel("tempo médio (ms)")
            plt.title(f"{variant} - N={n} (ms_until_fail)")
            plt.grid(True)
            fn = outdir / f"plot_{variant}_N{n}_vsP.png"
            plt.savefig(fn, dpi=150, bbox_inches="tight")
            plt.close()

    # Plot 2: tempo médio vs N para cada P
    for variant in sorted(df["variant"].unique()):
        sub = df[df["variant"]==variant]
        for p in sorted(sub["p"].unique()):
            d = sub[sub["p"]==p].sort_values("n")
            plt.figure()
            plt.plot(d["n"], d["mean_dt_ms"], marker="o")
            plt.xlabel("N"); plt.ylabel("tempo médio (ms)")
            plt.title(f"{variant} - P={p} (ms_until_fail)")
            plt.grid(True)
            fn = outdir / f"plot_{variant}_P{p}_vsN.png"
            plt.savefig(fn, dpi=150, bbox_inches="tight")
            plt.close()

if __name__ == "__main__":
    main()
