#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, pathlib, pandas as pd, matplotlib.pyplot as plt

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--title", default="Tempo vs n")
    args = ap.parse_args()

    df = pd.read_csv(args.metrics)
    if df.empty:
        print("nada para plotar"); return

    # se houver vários p, plota uma linha por p
    plt.figure()
    for p_val, g in df.groupby("p", sort=True):
        g = g.sort_values("n")
        plt.plot(g["n"], g["mean_ms"], marker="o", label=f"p={p_val}")

    plt.xlabel("n (tamanho da lista)")
    plt.ylabel("tempo médio (ms)")
    plt.title(args.title)
    if len(df["p"].unique()) > 1:
        plt.legend()
    pathlib.Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    plt.tight_layout()
    plt.savefig(args.out, dpi=160)
    print(f"[plot] salvo: {args.out}")

if __name__ == "__main__":
    main()
