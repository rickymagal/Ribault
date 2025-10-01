#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os
import pandas as pd
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

def load_super(path_super):
    df = pd.read_csv(path_super)
    df = df[(df["variant"]=="super") & (df["rc"]==0)].copy()
    return df

def load_hs(path_hs):
    df = pd.read_csv(path_hs)
    df = df[(df["variant"]=="ghc") & (df["rc"]==0)].copy()
    return df

def mean_std(df):
    # mean/std por (impl,N,P,imb,delta)
    g = df.groupby(["N","P","imb","delta"], as_index=False)["seconds"].agg(["mean","std"]).reset_index()
    g.rename(columns={"mean":"mean_seconds","std":"std_seconds"}, inplace=True)
    return g

def pick_best_per_N(g):
    # para cada (N,imb,delta): pega P com menor mean_seconds
    g["rank"] = g.groupby(["N","imb","delta"])["mean_seconds"].rank(method="first")
    best = g[g["rank"]==1].drop(columns=["rank"])
    return best

def plot_compare(best_super, best_hs, outdir, tag):
    # um gráfico por (imb,delta): N x mean_seconds (com barras de erro)
    keys = sorted(set(map(tuple, best_super[["imb","delta"]].drop_duplicates().values.tolist() +
                                best_hs[["imb","delta"]].drop_duplicates().values.tolist())))
    for (imb, delta) in keys:
        s = best_super[(best_super["imb"]==imb) & (best_super["delta"]==delta)]
        h = best_hs[(best_hs["imb"]==imb) & (best_hs["delta"]==delta)]
        if s.empty and h.empty: 
            continue
        plt.figure()
        if not s.empty:
            plt.errorbar(s["N"], s["mean_seconds"], yerr=s["std_seconds"].fillna(0.0),
                         marker="o", capsize=3, label="TALM (best P)")
        if not h.empty:
            plt.errorbar(h["N"], h["mean_seconds"], yerr=h["std_seconds"].fillna(0.0),
                         marker="s", capsize=3, label="GHC Parallel (best P)")
        plt.title(f"Dyck Path: Best-of-Breed Runtime vs N  (imb={imb}, delta={delta})")
        plt.xlabel("Input size N"); plt.ylabel("Runtime (seconds)")
        plt.grid(True, linestyle=":", linewidth=0.8)
        plt.legend()
        plt.tight_layout()
        base = os.path.join(outdir, f"compare_best_{tag}_imb{imb}_delta{delta}")
        plt.savefig(base + ".png", dpi=200); plt.savefig(base + ".pdf")
        plt.close()

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics-super", required=True)
    ap.add_argument("--metrics-hs", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--tag", required=True)
    args = ap.parse_args()
    os.makedirs(args.outdir, exist_ok=True)

    ds = load_super(args.metrics_super)
    dh = load_hs(args.metrics_hs)
    if ds.empty or dh.empty:
        print("[warn] one of the metrics files has no rows.")
    gs = mean_std(ds); gh = mean_std(dh)
    bs = pick_best_per_N(gs); bh = pick_best_per_N(gh)
    plot_compare(bs, bh, args.outdir, args.tag)
    print(f"[compare] saved into {args.outdir}")

if __name__ == "__main__":
    main()
