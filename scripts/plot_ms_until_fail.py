#!/usr/bin/env python3
import argparse, os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def savefig(path):
    plt.tight_layout()
    plt.savefig(path, dpi=150)
    plt.close()

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--csv", required=True)
    ap.add_argument("--outdir", required=True)
    args = ap.parse_args()
    os.makedirs(args.outdir, exist_ok=True)

    df = pd.read_csv(args.csv)
    # filtra rc==0
    df = df[df["rc"] == 0].copy()
    if df.empty:
        print("[plot] no successful runs to plot.")
        return

    # agrega por (variant,N,P)
    agg = df.groupby(["variant","N","P"])["ms"].mean().reset_index()
    # baseline por variant (P=1)
    base = agg[agg["P"]==1].set_index(["variant","N"])["ms"]

    # speedup e efficiency
    def speedup_row(r):
        return base.loc[(r["variant"], r["N"])] / r["ms"]
    agg["speedup"] = agg.apply(speedup_row, axis=1)
    agg["efficiency"] = agg["speedup"] / agg["P"]

    # 1) Speedup vs P (um gráfico por variant e N) + 2) Efficiency vs P
    for variant in agg["variant"].unique():
        for N in sorted(agg[agg["variant"]==variant]["N"].unique()):
            d = agg[(agg["variant"]==variant) & (agg["N"]==N)].sort_values("P")
            if d.empty: continue

            # speedup
            plt.figure()
            plt.plot(d["P"], d["speedup"], marker="o")
            plt.title(f"Speedup vs Cores — {variant.upper()} (N={N})")
            plt.xlabel("Cores (P)")
            plt.ylabel("Speedup (T1 / TP)")
            plt.grid(True, linestyle=":")
            savefig(os.path.join(args.outdir, f"speedup_{variant}_N{N}.png"))

            # efficiency
            plt.figure()
            plt.plot(d["P"], d["efficiency"], marker="o")
            plt.title(f"Efficiency vs Cores — {variant.upper()} (N={N})")
            plt.xlabel("Cores (P)")
            plt.ylabel("Efficiency (Speedup / P)")
            plt.ylim(0, 1.05)
            plt.grid(True, linestyle=":")
            savefig(os.path.join(args.outdir, f"efficiency_{variant}_N{N}.png"))

    # 3) Time vs P por variant e N
    for variant in agg["variant"].unique():
        for N in sorted(agg[agg["variant"]==variant]["N"].unique()):
            d = agg[(agg["variant"]==variant) & (agg["N"]==N)].sort_values("P")
            if d.empty: continue
            plt.figure()
            plt.plot(d["P"], d["ms"], marker="o")
            plt.title(f"Time vs Cores — {variant.upper()} (N={N})")
            plt.xlabel("Cores (P)")
            plt.ylabel("Time (ms)")
            plt.grid(True, linestyle=":")
            savefig(os.path.join(args.outdir, f"time_vs_P_{variant}_N{N}.png"))

    # 4) Time vs N (um gráfico por variant)
    for variant in agg["variant"].unique():
        plt.figure()
        for P in sorted(agg["P"].unique(), key=int):
            d = agg[(agg["variant"]==variant) & (agg["P"]==P)].sort_values("N")
            if d.empty: continue
            plt.plot(d["N"], d["ms"], marker="o", label=f"P={P}")
        plt.title(f"Time vs Problem Size — {variant.upper()}")
        plt.xlabel("N")
        plt.ylabel("Time (ms)")
        plt.legend()
        plt.grid(True, linestyle=":")
        savefig(os.path.join(args.outdir, f"time_vs_N_{variant}.png"))

    # 5) Para cada P, comparar ASM vs SUPER variando N
    for P in sorted(agg["P"].unique(), key=int):
        plt.figure()
        for variant in ["asm","super"]:
            d = agg[(agg["variant"]==variant) & (agg["P"]==P)].sort_values("N")
            if d.empty: continue
            plt.plot(d["N"], d["ms"], marker="o", label=variant.upper())
        plt.title(f"ASM vs SUPER — Time vs N (P={P})")
        plt.xlabel("N")
        plt.ylabel("Time (ms)")
        plt.legend()
        plt.grid(True, linestyle=":")
        savefig(os.path.join(args.outdir, f"asm_vs_super_time_vs_N_P{P}.png"))

    # salva também um CSV agregando médias
    agg.to_csv(os.path.join(args.outdir, "summary_agg.csv"), index=False)
    print(f"[plot] wrote plots and summary_agg.csv to {args.outdir}")

if __name__ == "__main__":
    main()
