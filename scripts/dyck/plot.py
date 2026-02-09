#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Plot Dyck path benchmark results.

Reads one or more CSV files (with 'variant' column: super, ghc).
Produces per-variant plots (runtime, speedup, efficiency) and,
when both variants are present, comparison plots.
"""

import argparse, csv, os, math
from collections import defaultdict
import statistics as stats
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# ── data loading ──────────────────────────────────────────────

def read_metrics(*paths):
    """Read CSV files.  Returns data[variant][delta][P][IMB] = [secs...], fixed_N."""
    data = defaultdict(lambda: defaultdict(lambda: defaultdict(lambda: defaultdict(list))))
    fixed_N = None
    for path in paths:
        with open(path, newline="") as f:
            for row in csv.DictReader(f):
                try:
                    variant = row["variant"].strip()
                    N       = int(row["N"])
                    P       = int(row["P"])
                    imb     = int(row["imb"])
                    delta   = int(row["delta"])
                    sec     = float(row["seconds"])
                    rc      = int(row["rc"])
                except Exception:
                    continue
                if rc != 0 or not math.isfinite(sec):
                    continue
                if fixed_N is None:
                    fixed_N = N
                data[variant][delta][P][imb].append(sec)
    return data, fixed_N

# ── helpers ───────────────────────────────────────────────────

def series_stats(series):
    """series: IMB -> [secs...].  Returns IMBs, means, stds."""
    IMBs = sorted(series.keys())
    means, stds = [], []
    for imb in IMBs:
        xs = series[imb]
        means.append(stats.mean(xs))
        stds.append(stats.stdev(xs) if len(xs) >= 2 else 0.0)
    return IMBs, means, stds

def best_runtime(byP):
    """byP[P][IMB] = [secs...].  For each IMB, min mean across P.
    Returns (IMBs, best_means, best_Ps)."""
    all_imbs = sorted({imb for P in byP for imb in byP[P]})
    best, best_Ps = [], []
    for imb in all_imbs:
        cands = [(stats.mean(byP[P][imb]), P) for P in byP
                 if imb in byP[P] and byP[P][imb]]
        if cands:
            val, p = min(cands)
            best.append(val); best_Ps.append(p)
        else:
            best.append(float('nan')); best_Ps.append(0)
    return all_imbs, best, best_Ps

def _save(outdir, base):
    plt.savefig(os.path.join(outdir, base + ".png"), dpi=180)
    plt.savefig(os.path.join(outdir, base + ".pdf"))
    plt.close()
    print(f"[plot] saved {base}.png/.pdf")

# ── single-variant plots ─────────────────────────────────────

def plot_runtime(byP, outdir, tag, N, delta):
    plt.figure(figsize=(10, 6))
    for P in sorted(byP):
        xs, mu, sd = series_stats(byP[P])
        if xs:
            plt.errorbar(xs, mu, yerr=sd, marker="o", capsize=3, label=f"P={P}")
    t = f"Runtime vs IMB (N={N})"
    if delta: t += f", delta={delta}"
    plt.title(t); plt.xlabel("IMB"); plt.ylabel("Runtime (s)")
    plt.grid(True, ls=":", lw=.8); plt.legend(title="Processors"); plt.tight_layout()
    _save(outdir, f"runtime_{tag}_delta{delta}")

def plot_speedup(byP, outdir, tag, N, delta):
    Ps = sorted(byP)
    if not Ps: return
    bP = Ps[0]; bmap = dict(zip(*series_stats(byP[bP])[:2]))
    plt.figure(figsize=(10, 6))
    for P in Ps:
        xs, mu, _ = series_stats(byP[P])
        si, sv = [], []
        for imb, t in zip(xs, mu):
            if imb in bmap and t > 0:
                si.append(imb); sv.append(bmap[imb] / t)
        if si: plt.plot(si, sv, marker="o", label=f"P={P}")
    t = f"Speedup vs IMB (N={N})"
    if delta: t += f", delta={delta}"
    plt.title(t); plt.xlabel("IMB"); plt.ylabel(f"Speedup vs P={bP}")
    plt.grid(True, ls=":", lw=.8); plt.legend(title="Processors"); plt.tight_layout()
    _save(outdir, f"speedup_{tag}_delta{delta}")

def plot_efficiency(byP, outdir, tag, N, delta):
    Ps = sorted(byP)
    if not Ps: return
    bP = Ps[0]; bmap = dict(zip(*series_stats(byP[bP])[:2]))
    plt.figure(figsize=(10, 6))
    for P in Ps:
        xs, mu, _ = series_stats(byP[P])
        ei, ev = [], []
        for imb, t in zip(xs, mu):
            if imb in bmap and t > 0:
                ei.append(imb); ev.append((bmap[imb] / t) / P)
        if ei: plt.plot(ei, ev, marker="o", label=f"P={P}")
    t = f"Efficiency vs IMB (N={N})"
    if delta: t += f", delta={delta}"
    plt.title(t); plt.xlabel("IMB"); plt.ylabel("Efficiency")
    plt.grid(True, ls=":", lw=.8); plt.legend(title="Processors"); plt.tight_layout()
    _save(outdir, f"efficiency_{tag}_delta{delta}")

# ── comparison plots ──────────────────────────────────────────

def plot_compare_runtime(data, outdir, tag, N, delta):
    """All TALM P-curves vs all GHC P-curves, each with a unique color."""
    plt.figure(figsize=(10, 6))
    colors = plt.cm.tab10.colors
    ci = 0
    if 'super' in data and delta in data['super']:
        for P in sorted(data['super'][delta]):
            xs, mu, sd = series_stats(data['super'][delta][P])
            plt.errorbar(xs, mu, yerr=sd, color=colors[ci % len(colors)],
                         ls='-', marker='s', ms=6, capsize=3, lw=1.8,
                         label=f'TALM P={P}')
            ci += 1
    if 'ghc' in data and delta in data['ghc']:
        for P in sorted(data['ghc'][delta]):
            xs, mu, sd = series_stats(data['ghc'][delta][P])
            plt.errorbar(xs, mu, yerr=sd, color=colors[ci % len(colors)],
                         ls='--', marker='o', ms=5, capsize=3, lw=1.8,
                         label=f'GHC P={P}')
            ci += 1
    plt.title(f"TALM vs GHC \u2013 Runtime (N={N})")
    plt.xlabel("Work Imbalance (IMB)"); plt.ylabel("Runtime (s)")
    plt.grid(True, ls=":", lw=.8); plt.legend(fontsize=8, ncol=2); plt.tight_layout()
    _save(outdir, f"compare_runtime_{tag}_delta{delta}")

def plot_compare_speedup(data, outdir, tag, N, delta):
    """Speedup comparison: TALM vs GHC, both relative to own P=1. Unique color per curve."""
    plt.figure(figsize=(10, 6))
    colors = plt.cm.tab10.colors
    ci = 0
    for variant, (ls, vlbl) in [('super', ('-', 'TALM')), ('ghc', ('--', 'GHC'))]:
        if variant not in data or delta not in data[variant]:
            continue
        byP = data[variant][delta]
        Ps = sorted(byP)
        if not Ps: continue
        bP = Ps[0]; bmap = dict(zip(*series_stats(byP[bP])[:2]))
        for P in Ps:
            xs, mu, _ = series_stats(byP[P])
            si, sv = [], []
            for imb, t in zip(xs, mu):
                if imb in bmap and t > 0:
                    si.append(imb); sv.append(bmap[imb] / t)
            if si:
                plt.plot(si, sv, ls, color=colors[ci % len(colors)],
                         marker='o', ms=5, lw=1.8, label=f'{vlbl} P={P}')
            ci += 1
    plt.title(f"TALM vs GHC \u2013 Speedup (N={N})")
    plt.xlabel("IMB"); plt.ylabel("Speedup vs own P=1")
    plt.grid(True, ls=":", lw=.8); plt.legend(fontsize=8, ncol=2); plt.tight_layout()
    _save(outdir, f"compare_speedup_{tag}_delta{delta}")

def plot_compare_best(data, outdir, tag, N, delta):
    """Best-of-each-variant runtime comparison, annotated with chosen P."""
    plt.figure(figsize=(10, 6))
    styles = {'super': ('k-', 's', 'TALM best'), 'ghc': ('r--', 'D', 'GHC best')}
    for variant in ('super', 'ghc'):
        if variant not in data or delta not in data[variant]:
            continue
        imbs, best, best_Ps = best_runtime(data[variant][delta])
        ls, mk, lbl = styles[variant]
        plt.plot(imbs, best, ls, marker=mk, ms=7, lw=2.5, label=lbl)
        for x, y, p in zip(imbs, best, best_Ps):
            plt.annotate(f'P={p}', (x, y), textcoords="offset points",
                         xytext=(0, 10), ha='center', fontsize=7,
                         color='black' if variant == 'super' else 'red')
    plt.title(f"Best TALM vs Best GHC \u2013 Runtime (N={N})")
    plt.xlabel("Work Imbalance (IMB)"); plt.ylabel("Runtime (s)")
    plt.grid(True, ls=":", lw=.8); plt.legend(fontsize=11); plt.tight_layout()
    _save(outdir, f"compare_best_{tag}_delta{delta}")

# ── main ──────────────────────────────────────────────────────

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True, nargs="+",
                    help="One or more CSV files (variant column distinguishes super/ghc)")
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--tag", required=True)
    args = ap.parse_args()
    os.makedirs(args.outdir, exist_ok=True)

    data, N = read_metrics(*args.metrics)
    if not data:
        print("[plot] no data to plot"); return

    variants = sorted(data.keys())

    # Per-variant plots
    for v in variants:
        vtag = f"{args.tag}_{v}"
        for delta, byP in sorted(data[v].items()):
            if byP:
                plot_runtime(byP, args.outdir, vtag, N, delta)
                plot_speedup(byP, args.outdir, vtag, N, delta)
                plot_efficiency(byP, args.outdir, vtag, N, delta)

    # Comparison plots (when both variants present)
    if len(variants) >= 2:
        all_deltas = sorted({d for v in variants for d in data[v]})
        for delta in all_deltas:
            plot_compare_runtime(data, args.outdir, args.tag, N, delta)
            plot_compare_speedup(data, args.outdir, args.tag, N, delta)
            plot_compare_best(data, args.outdir, args.tag, N, delta)

if __name__ == "__main__":
    main()
