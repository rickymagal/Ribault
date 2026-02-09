#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Plot Dyck path benchmark results.

X-axis: IMB (work imbalance)
Curves: P (processors)
One plot set per delta value.
"""

import argparse, csv, os, math
from collections import defaultdict
import statistics as stats
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# Lê CSV e retorna: data[delta][P][IMB] = [secs...]
def read_metrics(path):
    data = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    fixed_N = None
    with open(path, newline="") as f:
        cr = csv.DictReader(f)
        for row in cr:
            try:
                N     = int(row["N"])
                P     = int(row["P"])
                imb   = int(row["imb"])
                delta = int(row["delta"])
                sec   = float(row["seconds"])
                rc    = int(row["rc"])
            except Exception:
                continue
            if rc != 0 or not math.isfinite(sec):
                continue
            if fixed_N is None:
                fixed_N = N
            data[delta][P][imb].append(sec)
    return data, fixed_N

def series_mean_std(series_dict):
    """series_dict: IMB -> [secs...]"""
    IMBs = sorted(series_dict.keys())
    means, stds = [], []
    for imb in IMBs:
        xs = series_dict[imb]
        if len(xs) == 1:
            means.append(xs[0])
            stds.append(0.0)
        else:
            means.append(stats.mean(xs))
            stds.append(stats.stdev(xs) if len(xs) >= 2 else 0.0)
    return IMBs, means, stds

def plot_runtime(groups, outdir, tag, fixed_N):
    for delta, byP in sorted(groups.items()):
        plt.figure(figsize=(10, 6))
        for P in sorted(byP.keys()):
            IMBs, mu, sd = series_mean_std(byP[P])
            if not IMBs:
                continue
            plt.errorbar(IMBs, mu, yerr=sd, marker="o", capsize=3, label=f"P={P}")
        title = f"Dyck Path \u2013 Runtime vs Work Imbalance (N={fixed_N}"
        if delta != 0:
            title += f", delta={delta}"
        title += ")"
        plt.title(title, fontsize=12)
        plt.xlabel("Work Imbalance (IMB)", fontsize=11)
        plt.ylabel("Runtime (seconds) \u2013 mean \u00b1 std", fontsize=11)
        plt.grid(True, linestyle=":", linewidth=0.8)
        plt.legend(title="Processors", fontsize=9)
        plt.tight_layout()
        base = f"runtime_{tag}_delta{delta}"
        plt.savefig(os.path.join(outdir, base + ".png"), dpi=180)
        plt.savefig(os.path.join(outdir, base + ".pdf"))
        plt.close()
        print(f"[plot] saved {base}.png/.pdf")

def plot_speedup(groups, outdir, tag, fixed_N):
    for delta, byP in sorted(groups.items()):
        Ps = sorted(p for p in byP if byP[p])
        if not Ps:
            continue
        baseP = Ps[0]
        IMBs_b, mu_b, _ = series_mean_std(byP[baseP])
        base_map = dict(zip(IMBs_b, mu_b))
        plt.figure(figsize=(10, 6))
        for P in Ps:
            IMBs, mu, _ = series_mean_std(byP[P])
            IMBs_sp, sp = [], []
            for imb, tP in zip(IMBs, mu):
                if imb in base_map and tP > 0.0:
                    IMBs_sp.append(imb)
                    sp.append(base_map[imb] / tP)
            if IMBs_sp:
                plt.plot(IMBs_sp, sp, marker="o", label=f"P={P}")
        title = f"Dyck Path \u2013 Parallel Speedup (N={fixed_N}"
        if delta != 0:
            title += f", delta={delta}"
        title += ")"
        plt.title(title, fontsize=12)
        plt.xlabel("Work Imbalance (IMB)", fontsize=11)
        plt.ylabel(f"Speedup vs P={baseP}", fontsize=11)
        plt.grid(True, linestyle=":", linewidth=0.8)
        plt.legend(title="Processors", fontsize=9)
        plt.tight_layout()
        base = f"speedup_{tag}_delta{delta}"
        plt.savefig(os.path.join(outdir, base + ".png"), dpi=180)
        plt.savefig(os.path.join(outdir, base + ".pdf"))
        plt.close()
        print(f"[plot] saved {base}.png/.pdf")

def plot_efficiency(groups, outdir, tag, fixed_N):
    for delta, byP in sorted(groups.items()):
        Ps = sorted(p for p in byP if byP[p])
        if not Ps:
            continue
        baseP = Ps[0]
        IMBs_b, mu_b, _ = series_mean_std(byP[baseP])
        base_map = dict(zip(IMBs_b, mu_b))
        plt.figure(figsize=(10, 6))
        for P in Ps:
            IMBs, mu, _ = series_mean_std(byP[P])
            IMBs_eff, eff = [], []
            for imb, tP in zip(IMBs, mu):
                if imb in base_map and tP > 0.0:
                    IMBs_eff.append(imb)
                    eff.append((base_map[imb] / tP) / P)
            if IMBs_eff:
                plt.plot(IMBs_eff, eff, marker="o", label=f"P={P}")
        title = f"Dyck Path \u2013 Parallel Efficiency (N={fixed_N}"
        if delta != 0:
            title += f", delta={delta}"
        title += ")"
        plt.title(title, fontsize=12)
        plt.xlabel("Work Imbalance (IMB)", fontsize=11)
        plt.ylabel("Efficiency", fontsize=11)
        plt.grid(True, linestyle=":", linewidth=0.8)
        plt.legend(title="Processors", fontsize=9)
        plt.tight_layout()
        base = f"efficiency_{tag}_delta{delta}"
        plt.savefig(os.path.join(outdir, base + ".png"), dpi=180)
        plt.savefig(os.path.join(outdir, base + ".pdf"))
        plt.close()
        print(f"[plot] saved {base}.png/.pdf")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--tag", required=True)
    args = ap.parse_args()
    os.makedirs(args.outdir, exist_ok=True)
    groups, fixed_N = read_metrics(args.metrics)
    if not groups:
        print("[plot] no data to plot")
        return
    plot_runtime(groups, args.outdir, args.tag, fixed_N)
    plot_speedup(groups, args.outdir, args.tag, fixed_N)
    plot_efficiency(groups, args.outdir, args.tag, fixed_N)

if __name__ == "__main__":
    main()
