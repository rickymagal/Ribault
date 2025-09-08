#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os, sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

def load_metrics(path):
    df = pd.read_csv(path)
    # Esperado: variant,N,P,imb,delta,rep,seconds,rc
    df = df[df["rc"] == 0].copy()
    if df.empty:
        print("[plot] WARNING: no successful runs (rc==0).", file=sys.stderr)
    return df

def agg_medians(df):
    # Mediana por (N,P,imb,delta)
    g = df.groupby(["N","P","imb","delta"], as_index=False)["seconds"].median()
    g.rename(columns={"seconds":"median_seconds"}, inplace=True)
    return g

def ensure_outdir(outdir):
    os.makedirs(outdir, exist_ok=True)

def plot_runtime_vs_N(g, outdir, tag):
    # Um gráfico por par (imb, delta); linhas = P
    for (imb, delta), sub in g.groupby(["imb","delta"]):
        pivot = sub.pivot(index="N", columns="P", values="median_seconds").sort_index()
        plt.figure()
        pivot.plot(marker="o")
        plt.xlabel("Input size N")
        plt.ylabel("Runtime (s)")
        plt.title(f"Dyck Path — Runtime vs N (imb={imb}, delta={delta})")
        plt.legend(title="Processors (P)")
        plt.tight_layout()
        base = os.path.join(outdir, f"runtime_{tag}_imb{imb}_delta{delta}")
        plt.savefig(base + ".png", dpi=200)
        plt.savefig(base + ".pdf")
        plt.close()

def plot_speedup_vs_P(g, outdir, tag):
    # Um gráfico por par (imb, delta); linhas = N; speedup relativo ao menor P disponível
    for (imb, delta), sub in g.groupby(["imb","delta"]):
        # baseline por N = tempo com menor P
        Pmin = sub["P"].min()
        base = sub[sub["P"] == Pmin][["N","median_seconds"]].set_index("N")["median_seconds"]
        sub2 = sub.copy()
        sub2["speedup"] = sub2.apply(lambda r: float(base.loc[r["N"]]) / r["median_seconds"] if r["N"] in base.index else np.nan, axis=1)
        pivot = sub2.pivot(index="P", columns="N", values="speedup").sort_index()
        plt.figure()
        pivot.plot(marker="o")
        plt.xlabel("Processors (P)")
        plt.ylabel("Relative speedup (×)")
        plt.title(f"Dyck Path — Speedup vs P (baseline P={Pmin}, imb={imb}, delta={delta})")
        plt.legend(title="Input size (N)")
        plt.tight_layout()
        basef = os.path.join(outdir, f"speedup_{tag}_imb{imb}_delta{delta}")
        plt.savefig(basef + ".png", dpi=200)
        plt.savefig(basef + ".pdf")
        plt.close()

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--tag", required=True)
    args = ap.parse_args()

    ensure_outdir(args.outdir)
    df = load_metrics(args.metrics)
    if df.empty:
        sys.exit(0)
    g = agg_medians(df)

    plot_runtime_vs_N(g, args.outdir, args.tag)
    plot_speedup_vs_P(g, args.outdir, args.tag)
    print(f"[plot] saved figures into {args.outdir}")

if __name__ == "__main__":
    main()
