#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, csv, collections
import matplotlib.pyplot as plt

def read_metrics(csv_path):
    by_p = collections.defaultdict(list)
    with open(csv_path, newline="") as f:
        for row in csv.DictReader(f):
            n = int(row["n"]); p = int(row["p"])
            t = float(row["T_mean_s"]); ci = float(row["CI95_s"])
            by_p[p].append((n, t, ci))
    for p in by_p:
        by_p[p].sort()
    return by_p

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--out", required=True, help="png de saída")
    ap.add_argument("--title", default="MergeSort: Tempo vs Tamanho")
    args = ap.parse_args()

    data = read_metrics(args.metrics)
    if not data:
        print("nada para plotar"); return

    plt.figure()
    for p, pts in sorted(data.items()):
        xs = [n for (n,_,__) in pts]
        ys = [t for (_,t,__) in pts]
        es = [ci for (_,__,ci) in pts]
        plt.errorbar(xs, ys, yerr=es, marker="o", capsize=3, label=f"p={p}")
    plt.xlabel("n (tamanho do vetor)")
    plt.ylabel("tempo médio (s) ± IC95")
    plt.title(args.title)
    plt.xscale("log", base=2)
    plt.grid(True, which="both", linestyle="--", alpha=0.4)
    plt.legend()
    plt.tight_layout()
    plt.savefig(args.out, dpi=160)
    print(f"[plot] OK: {args.out}")

if __name__ == "__main__":
    main()
