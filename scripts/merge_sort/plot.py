#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, csv, os, math
from collections import defaultdict, OrderedDict
from statistics import mean, pstdev
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

def read_rows(metrics_path):
    rows = []
    with open(metrics_path, newline="") as f:
        cr = csv.DictReader(f)
        for row in cr:
            try:
                if row.get("variant","") != "super":
                    continue
                N  = int(row["N"])
                P  = int(row["P"])
                rc = int(row["rc"])
                if rc != 0:
                    continue
                t  = float(row["seconds"])
            except Exception:
                continue
            rows.append((N, P, t))
    return rows

def aggregate(rows):
    # group by (P,N) -> list of runtimes
    g = defaultdict(list)
    for N, P, t in rows:
        g[(P, N)].append(t)

    # build stats per P ordered by N
    stats = {}  # P -> dict with keys Ns, mean, std, count
    byP = defaultdict(list)
    for (P, N), vals in g.items():
        vals_sorted = sorted(vals)
        m = mean(vals_sorted)
        # population std dev if >=2; else 0.0
        s = pstdev(vals_sorted) if len(vals_sorted) > 1 else 0.0
        byP[P].append((N, m, s, len(vals_sorted)))

    for P, entries in byP.items():
        entries.sort(key=lambda x: x[0])
        Ns   = [e[0] for e in entries]
        mus  = [e[1] for e in entries]
        sigs = [e[2] for e in entries]
        cnts = [e[3] for e in entries]
        stats[P] = {"Ns": Ns, "mean": mus, "std": sigs, "count": cnts}
    return stats

def save_aggregated_csv(stats, outdir, tag):
    out = os.path.join(outdir, f"metrics_aggregated_{tag}.csv")
    with open(out, "w", newline="") as f:
        cw = csv.writer(f)
        cw.writerow(["variant","N","P","reps","mean_seconds","std_seconds"])
        for P in sorted(stats.keys()):
            Ns = stats[P]["Ns"]; mus = stats[P]["mean"]; sigs = stats[P]["std"]; cnts = stats[P]["count"]
            for N, m, s, c in zip(Ns, mus, sigs, cnts):
                cw.writerow(["super", N, P, c, f"{m:.6f}", f"{s:.6f}"])
    print(f"[plot] aggregated CSV: {out}")

def plot_runtime(stats, outdir, tag):
    plt.figure()
    for P in sorted(stats.keys()):
        Ns   = stats[P]["Ns"]
        mus  = stats[P]["mean"]
        sigs = stats[P]["std"]
        if not Ns: 
            continue
        # error bars: ±1σ
        plt.errorbar(Ns, mus, yerr=sigs, fmt="-o", capsize=3, label=f"P = {P}")
    plt.title("Merge Sort (SUPER): Runtime vs Input Size", fontsize=12)
    plt.xlabel("Input size N", fontsize=11)
    plt.ylabel("Runtime (seconds)", fontsize=11)
    plt.grid(True, linestyle=":", linewidth=0.8)
    plt.legend(title="Threads", fontsize=9)
    plt.tight_layout()
    png = os.path.join(outdir, f"runtime_{tag}.png")
    pdf = os.path.join(outdir, f"runtime_{tag}.pdf")
    plt.savefig(png, dpi=180); plt.savefig(pdf)
    print(f"[plot] saved {png} and {pdf}")

def _common_points(stats, P, baseP):
    # intersect Ns between P and baseP
    NsP   = stats[P]["Ns"];    musP  = stats[P]["mean"];    sigP = stats[P]["std"]
    NsB   = stats[baseP]["Ns"]; musB  = stats[baseP]["mean"]; sigB = stats[baseP]["std"]
    mp = dict(zip(NsP, zip(musP, sigP)))
    mb = dict(zip(NsB, zip(musB, sigB)))
    Ns = sorted(set(NsP).intersection(NsB))
    out = []
    for N in Ns:
        mP, sP = mp[N]; mB, sB = mb[N]
        if mP > 0 and mB > 0:
            out.append((N, mP, sP, mB, sB))
    return out

def plot_speedup(stats, outdir, tag, baselineP=None):
    Ps = sorted(stats.keys())
    if not Ps:
        return
    baseP = baselineP if (baselineP in Ps) else Ps[0]
    plt.figure()
    for P in Ps:
        pts = _common_points(stats, P, baseP)
        if not pts:
            continue
        Ns  = [N for (N, *_ ) in pts]
        # speedup = m_base / m_P
        sp  = [ (mB / mP) for (_N, mP, sP, mB, sB) in pts ]
        # error propagation for ratio: σ_r ≈ r * sqrt( (σ_B/mB)^2 + (σ_P/mP)^2 )
        yerr = []
        for (_N, mP, sP, mB, sB) in pts:
            r = mB / mP
            term = 0.0
            if mB > 0: term += (sB / mB)**2
            if mP > 0: term += (sP / mP)**2
            yerr.append(r * math.sqrt(term) if term > 0 else 0.0)
        plt.errorbar(Ns, sp, yerr=yerr, fmt="-o", capsize=3, label=f"P = {P}")
    plt.title(f"Merge Sort (SUPER): Parallel Speedup (baseline P = {baseP})", fontsize=12)
    plt.xlabel("Input size N", fontsize=11)
    plt.ylabel("Speedup", fontsize=11)
    plt.grid(True, linestyle=":", linewidth=0.8)
    plt.legend(title="Threads", fontsize=9)
    plt.tight_layout()
    png = os.path.join(outdir, f"speedup_{tag}.png")
    pdf = os.path.join(outdir, f"speedup_{tag}.pdf")
    plt.savefig(png, dpi=180); plt.savefig(pdf)
    print(f"[plot] saved {png} and {pdf}")

def plot_efficiency(stats, outdir, tag, baselineP=None):
    Ps = sorted(stats.keys())
    if not Ps:
        return
    baseP = baselineP if (baselineP in Ps) else Ps[0]
    plt.figure()
    for P in Ps:
        pts = _common_points(stats, P, baseP)
        if not pts:
            continue
        Ns   = [N for (N, *_ ) in pts]
        sp   = [ (mB / mP) for (_N, mP, sP, mB, sB) in pts ]
        # reuse speedup error propagation and divide by P
        yerr_sp = []
        for (_N, mP, sP, mB, sB) in pts:
            r = mB / mP
            term = 0.0
            if mB > 0: term += (sB / mB)**2
            if mP > 0: term += (sP / mP)**2
            yerr_sp.append(r * math.sqrt(term) if term > 0 else 0.0)
        eff  = [ s/float(P) for s in sp ]
        yerr = [ e/float(P) for e in yerr_sp ]
        plt.errorbar(Ns, eff, yerr=yerr, fmt="-o", capsize=3, label=f"P = {P}")
    plt.title(f"Merge Sort (SUPER): Parallel Efficiency (baseline P = {baseP})", fontsize=12)
    plt.xlabel("Input size N", fontsize=11)
    plt.ylabel("Efficiency", fontsize=11)
    plt.grid(True, linestyle=":", linewidth=0.8)
    plt.legend(title="Threads", fontsize=9)
    plt.tight_layout()
    png = os.path.join(outdir, f"efficiency_{tag}.png")
    pdf = os.path.join(outdir, f"efficiency_{tag}.pdf")
    plt.savefig(png, dpi=180); plt.savefig(pdf)
    print(f"[plot] saved {png} and {pdf}")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--tag", required=True)
    ap.add_argument("--baselineP", type=int, default=None,
                    help="Optional baseline P for speedup/efficiency (default: smallest P present)")
    args = ap.parse_args()

    os.makedirs(args.outdir, exist_ok=True)
    rows  = read_rows(args.metrics)
    if not rows:
        print("[plot] no data to plot (did all runs fail?)")
        return
    stats = aggregate(rows)
    save_aggregated_csv(stats, args.outdir, args.tag)
    plot_runtime(stats, args.outdir, args.tag)
    plot_speedup(stats, args.outdir, args.tag, baselineP=args.baselineP)
    plot_efficiency(stats, args.outdir, args.tag, baselineP=args.baselineP)

if __name__ == "__main__":
    main()
