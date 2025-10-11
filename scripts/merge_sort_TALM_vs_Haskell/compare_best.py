#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, csv, os
from collections import defaultdict
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

def read_agg(path, variant):
    byP = defaultdict(dict)  # P -> { N: (mean,std) }
    with open(path, newline="") as f:
        cr = csv.DictReader(f)
        for r in cr:
            if r.get("variant","") != variant: continue
            N = int(r["N"]); P = int(r["P"])
            m = float(r["mean_seconds"]); s = float(r["std_seconds"])
            byP[P][N] = (m,s)
    return dict(byP)

def choose_bestP(byP):
    if not byP: return None, None
    common_Ns = set.intersection(*[set(d.keys()) for d in byP.values()]) if len(byP)>1 else set(next(iter(byP.values())).keys())
    if common_Ns:
        Nref = max(common_Ns)
        Pbest = min(byP.keys(), key=lambda P: byP[P][Nref][0])
        return Pbest, Nref
    # fallback: maior N disponível em qualquer P
    allNs = set().union(*[set(d.keys()) for d in byP.values()])
    if not allNs: return None, None
    Nref = max(allNs)
    cand = [(P, byP[P][Nref][0]) for P in byP if Nref in byP[P]]
    if not cand: return None, None
    return min(cand, key=lambda x: x[1])[0], Nref

def extract_series(dP):
    Ns = sorted(dP.keys())
    mus = [dP[N][0] for N in Ns]
    sds = [dP[N][1] for N in Ns]
    return Ns, mus, sds

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--agg-super", required=True, help="CSV agregado (seu) - variant=super")
    ap.add_argument("--agg-ghc",   required=True, help="CSV agregado (GHC)  - variant=ghc")
    ap.add_argument("--outdir",    required=True)
    ap.add_argument("--tag",       required=True)
    args = ap.parse_args()

    os.makedirs(args.outdir, exist_ok=True)

    sup = read_agg(args.agg_super, "super")
    ghc = read_agg(args.agg_ghc,   "ghc")

    Psup, _ = choose_bestP(sup)
    Pghc, _ = choose_bestP(ghc)
    if Psup is None or Pghc is None:
        print("[compare] insuficiente para comparar"); return

    Ns_s, mus_s, sds_s = extract_series(sup[Psup])
    Ns_g, mus_g, sds_g = extract_series(ghc[Pghc])

    plt.figure()
    plt.errorbar(Ns_s, mus_s, yerr=sds_s, fmt="-o", capsize=3, label=f"Ribault (SUPER), P = {Psup}")
    plt.errorbar(Ns_g, mus_g, yerr=sds_g, fmt="-s", capsize=3, label=f"GHC (-N), P = {Pghc}")
    plt.title("Merge Sort: Best Configuration Comparison (Runtime vs N)", fontsize=12)
    plt.xlabel("Input size N", fontsize=11)
    plt.ylabel("Runtime (seconds)", fontsize=11)
    plt.grid(True, linestyle=":", linewidth=0.8)
    plt.legend(fontsize=9)
    plt.tight_layout()
    for ext in ("png","pdf"):
        fn=os.path.join(args.outdir, f"compare_best_{args.tag}.{ext}")
        plt.savefig(fn, dpi=180)
    print("[compare] overlay salvo")

if __name__ == "__main__":
    main()
