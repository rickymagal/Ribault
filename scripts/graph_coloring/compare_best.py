#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Graph Coloring Benchmark Results Aggregator and Visualizer
===========================================================

This script aggregates results from multiple benchmark runs and generates
comparison summaries (and plots if matplotlib is available).

Input:
------
CSV files with columns: variant,N,P,edge_prob,seed,rep,runtime_sec,colors,valid,rc

Output:
-------
- Text summary table comparing all variants
- Speedup analysis (GHC vs par/pseq)
- Optional: PNG plots if matplotlib is available

Metrics Computed:
-----------------
- Median runtime per (variant, N, P) combination
- Parallel speedup relative to P=1
- Cross-variant speedup ratios

Usage:
------
    python3 compare_best.py \
        --metrics metrics_super.csv metrics_ghc.csv metrics_parpseq.csv \
        --outdir ./results

Author: Graph Coloring Benchmark for Ribault Project
"""

import argparse
import csv
import os
from collections import defaultdict

# Try to import matplotlib, fall back to text-only output
try:
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    HAS_MATPLOTLIB = True
except ImportError:
    HAS_MATPLOTLIB = False
    print("[WARN] matplotlib not available, generating text summary only")


def load_metrics(paths):
    """Load metrics from multiple CSV files."""
    data = []
    for path in paths:
        if not os.path.exists(path):
            print(f"[WARN] File not found: {path}")
            continue
        with open(path, 'r') as f:
            reader = csv.DictReader(f)
            for row in reader:
                try:
                    data.append({
                        'variant': row['variant'],
                        'N': int(row['N']),
                        'P': int(row['P']),
                        'edge_prob': float(row['edge_prob']),
                        'seconds': float(row['seconds']) if row['seconds'] != 'NaN' else float('nan'),
                        'colors': int(row['colors']) if row['colors'] != '0' else 0,
                        'valid': row['valid'] == 'True',
                        'rc': int(row['rc']),
                    })
                except (ValueError, KeyError) as e:
                    print(f"[WARN] Skipping row: {e}")
    return data


def aggregate_data(data):
    """Compute median time per (variant, N, P)."""
    groups = defaultdict(list)
    for row in data:
        if row['rc'] == 0:  # Only successful runs
            key = (row['variant'], row['N'], row['P'])
            groups[key].append(row['seconds'])

    result = {}
    for key, times in groups.items():
        times = sorted(times)
        n = len(times)
        median = times[n // 2] if n % 2 == 1 else (times[n // 2 - 1] + times[n // 2]) / 2
        result[key] = median
    return result


def print_summary(agg_data):
    """Print text summary of results."""
    # Group by variant
    variants = sorted(set(k[0] for k in agg_data.keys()))
    ns = sorted(set(k[1] for k in agg_data.keys()))
    ps = sorted(set(k[2] for k in agg_data.keys()))

    print("\n" + "=" * 60)
    print("GRAPH COLORING BENCHMARK RESULTS")
    print("=" * 60)

    for n in ns:
        print(f"\nN = {n:,}")
        print("-" * 40)
        print(f"{'Variant':<12} {'P=1':>10} {'P=2':>10} {'P=4':>10} {'P=8':>10}")
        print("-" * 40)
        for v in variants:
            row = f"{v:<12}"
            for p in ps:
                key = (v, n, p)
                if key in agg_data:
                    row += f" {agg_data[key]:>9.4f}s"
                else:
                    row += f" {'N/A':>10}"
            print(row)

    # Speedup comparison (if we have multiple variants)
    if len(variants) > 1:
        print(f"\n{'Speedup (ghc/parpseq)':}")
        print("-" * 40)
        for n in ns:
            print(f"\nN = {n:,}")
            for p in ps:
                ghc_key = ('ghc', n, p)
                parpseq_key = ('parpseq', n, p)
                if ghc_key in agg_data and parpseq_key in agg_data:
                    speedup = agg_data[ghc_key] / agg_data[parpseq_key]
                    print(f"  P={p}: {speedup:.2f}x")


def plot_results(agg_data, outdir, tag):
    """Generate comparison plots."""
    if not HAS_MATPLOTLIB:
        return

    variants = sorted(set(k[0] for k in agg_data.keys()))
    ns = sorted(set(k[1] for k in agg_data.keys()))
    ps = sorted(set(k[2] for k in agg_data.keys()))

    colors = {'ghc': 'blue', 'parpseq': 'orange', 'super': 'green', 'talm': 'green'}
    markers = {'ghc': 'o', 'parpseq': 's', 'super': '^', 'talm': '^'}

    # Plot 1: Time vs N for each P
    for p in ps:
        fig, ax = plt.subplots(figsize=(10, 6))
        for v in variants:
            x_vals = []
            y_vals = []
            for n in ns:
                key = (v, n, p)
                if key in agg_data:
                    x_vals.append(n)
                    y_vals.append(agg_data[key])
            if x_vals:
                ax.plot(x_vals, y_vals,
                       color=colors.get(v, 'gray'),
                       marker=markers.get(v, 'o'),
                       label=v, linewidth=2, markersize=8)

        ax.set_xlabel('Number of Vertices (N)', fontsize=12)
        ax.set_ylabel('Time (seconds)', fontsize=12)
        ax.set_title(f'Graph Coloring: Time vs N (P={p})', fontsize=14)
        ax.legend()
        ax.grid(True, alpha=0.3)
        ax.set_xscale('log')
        ax.set_yscale('log')

        outpath = os.path.join(outdir, f'{tag}_time_vs_N_P{p}.png')
        fig.savefig(outpath, dpi=150, bbox_inches='tight')
        plt.close(fig)
        print(f"[plot] saved: {outpath}")

    # Plot 2: Time vs P for each N
    for n in ns:
        fig, ax = plt.subplots(figsize=(10, 6))
        for v in variants:
            x_vals = []
            y_vals = []
            for p in ps:
                key = (v, n, p)
                if key in agg_data:
                    x_vals.append(p)
                    y_vals.append(agg_data[key])
            if x_vals:
                ax.plot(x_vals, y_vals,
                       color=colors.get(v, 'gray'),
                       marker=markers.get(v, 'o'),
                       label=v, linewidth=2, markersize=8)

        ax.set_xlabel('Number of Processors (P)', fontsize=12)
        ax.set_ylabel('Time (seconds)', fontsize=12)
        ax.set_title(f'Graph Coloring: Time vs P (N={n:,})', fontsize=14)
        ax.legend()
        ax.grid(True, alpha=0.3)
        ax.set_xticks(ps)

        outpath = os.path.join(outdir, f'{tag}_time_vs_P_N{n}.png')
        fig.savefig(outpath, dpi=150, bbox_inches='tight')
        plt.close(fig)
        print(f"[plot] saved: {outpath}")

    # Plot 3: Speedup vs P (relative to P=1 for each variant)
    for n in ns:
        fig, ax = plt.subplots(figsize=(10, 6))
        for v in variants:
            base_key = (v, n, 1)
            if base_key not in agg_data:
                continue
            base_time = agg_data[base_key]

            x_vals = []
            y_vals = []
            for p in ps:
                key = (v, n, p)
                if key in agg_data:
                    x_vals.append(p)
                    y_vals.append(base_time / agg_data[key])
            if x_vals:
                ax.plot(x_vals, y_vals,
                       color=colors.get(v, 'gray'),
                       marker=markers.get(v, 'o'),
                       label=v, linewidth=2, markersize=8)

        # Ideal speedup line
        ax.plot(ps, ps, 'k--', label='ideal', alpha=0.5)

        ax.set_xlabel('Number of Processors (P)', fontsize=12)
        ax.set_ylabel('Speedup (vs P=1)', fontsize=12)
        ax.set_title(f'Graph Coloring: Parallel Speedup (N={n:,})', fontsize=14)
        ax.legend()
        ax.grid(True, alpha=0.3)
        ax.set_xticks(ps)

        outpath = os.path.join(outdir, f'{tag}_speedup_N{n}.png')
        fig.savefig(outpath, dpi=150, bbox_inches='tight')
        plt.close(fig)
        print(f"[plot] saved: {outpath}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", nargs='+', required=True, help="Metrics CSV files")
    ap.add_argument("--outdir", required=True, help="Output directory for plots")
    ap.add_argument("--tag", default="gc", help="Tag for output files")
    args = ap.parse_args()

    os.makedirs(args.outdir, exist_ok=True)

    data = load_metrics(args.metrics)
    if not data:
        print("[ERROR] No valid data loaded")
        return 1

    print(f"[info] Loaded {len(data)} records from {len(args.metrics)} files")

    agg_data = aggregate_data(data)
    print_summary(agg_data)
    plot_results(agg_data, args.outdir, args.tag)

    return 0


if __name__ == "__main__":
    exit(main())
