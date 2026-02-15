#!/usr/bin/env python3
"""Generate a PDF table with N-Queens benchmark results — one sub-table per N."""

import csv, os, sys
from collections import defaultdict
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

def med(vals):
    s = sorted(vals)
    return s[len(s)//2]

def fmt(v):
    if v < 0.0001:
        return f"{v*1e6:.0f}\u00b5s"
    if v < 0.01:
        return f"{v*1000:.2f}ms"
    if v < 1:
        return f"{v*1000:.1f}ms"
    if v < 100:
        return f"{v:.2f}s"
    return f"{v:.1f}s"

def main():
    csv_path = sys.argv[1] if len(sys.argv) > 1 else "results/nqueens/metrics.csv"
    out_dir = os.path.dirname(csv_path)

    data = defaultdict(list)
    with open(csv_path) as f:
        for row in csv.DictReader(f):
            N = int(row['N'])
            P = int(row['P'])
            v = row['variant']
            s = float(row['seconds'])
            data[(N, v, P)].append(s)

    Ns = sorted(set(k[0] for k in data))
    Ps = sorted(set(k[2] for k in data))
    variants = [('super', 'Ribault'), ('ghc', 'GHC Strategies'), ('parpseq', 'GHC par/pseq')]

    # Layout: one block per N, each block has 4 rows (3 variants + ratio), Ps as columns
    # Total rows = len(Ns) * 5 (header + 3 variants + ratio) + title
    n_blocks = len(Ns)
    rows_per_block = 4  # 3 variants + ratio
    total_rows = n_blocks * (rows_per_block + 1)  # +1 for N header rows

    fig_w = 2.5 + len(Ps) * 1.5
    fig_h = 1.5 + n_blocks * 2.2
    fig, ax = plt.subplots(figsize=(fig_w, fig_h))
    ax.axis('off')

    # Build one big table
    col_labels = ['N', 'Variant'] + [f'P={P}' for P in Ps]
    n_cols = len(col_labels)

    cell_text = []
    cell_colors = []

    HDR_BG = '#4472c4'
    N_BG = '#d9e2f3'
    WHITE = 'white'
    GREEN = '#c6efce'
    YELLOW = '#ffffcc'
    L_ORANGE = '#fce4d6'
    L_RED = '#f4cccc'

    for N in Ns:
        # Compute medians and best GHC
        meds = {}
        for vk, vlabel in variants:
            for P in Ps:
                meds[(vk, P)] = med(data[(N, vk, P)])

        for vi, (vk, vlabel) in enumerate(variants):
            row_data = [str(N) if vi == 0 else '', vlabel]
            row_colors = [N_BG if vi == 0 else WHITE, WHITE]
            for P in Ps:
                t = meds[('super', P)]
                g = meds[('ghc', P)]
                p = meds[('parpseq', P)]
                best_all = min(t, g, p)
                val = meds[(vk, P)]
                # Highlight if this is the best
                if abs(val - best_all) / max(best_all, 1e-12) < 0.03:
                    bg = GREEN
                else:
                    bg = WHITE
                row_data.append(fmt(val))
                row_colors.append(bg)
            cell_text.append(row_data)
            cell_colors.append(row_colors)

        # Ratio row
        row_data = ['', 'TALM / best GHC']
        row_colors = ['#f0f0f0', '#f0f0f0']
        for P in Ps:
            t = meds[('super', P)]
            best_ghc = min(meds[('ghc', P)], meds[('parpseq', P)])
            ratio = t / best_ghc if best_ghc > 0 else float('inf')
            if ratio < 0.97:
                bg = GREEN
                s = f'{ratio:.2f}x'
            elif ratio < 1.03:
                bg = YELLOW
                s = f'{ratio:.2f}x'
            elif ratio < 2.0:
                bg = L_ORANGE
                s = f'{ratio:.1f}x'
            else:
                bg = L_RED
                s = f'{ratio:.0f}x'
            row_data.append(s)
            row_colors.append(bg)
        cell_text.append(row_data)
        cell_colors.append(row_colors)

    table = ax.table(cellText=cell_text,
                     colLabels=col_labels,
                     cellColours=cell_colors,
                     colColours=[HDR_BG] * n_cols,
                     cellLoc='center',
                     loc='center')

    # Style header
    for j in range(n_cols):
        cell = table[0, j]
        cell.set_text_props(color='white', fontweight='bold', fontsize=9)
        cell.set_height(0.035)

    n_data_rows = len(cell_text)
    for i in range(1, n_data_rows + 1):
        for j in range(n_cols):
            cell = table[i, j]
            cell.set_fontsize(8)
            cell.set_height(0.022)
            # Bold N column and ratio row
            row_idx = i - 1  # 0-based into cell_text
            if j == 0 and cell_text[row_idx][0]:
                cell.set_text_props(fontweight='bold', fontsize=9)
            if row_idx % 4 == 3:  # ratio row
                cell.set_text_props(fontweight='bold', fontsize=8)

    # Add separator lines between N groups
    for i in range(n_data_rows):
        if i % 4 == 0 and i > 0:  # first row of each N block
            for j in range(n_cols):
                cell = table[i + 1, j]  # +1 for header
                cell.set_edgecolor('black')
                cell.set_linewidth(1.2)

    table.auto_set_font_size(False)
    table.scale(1.0, 1.5)

    ax.set_title('N-Queens Benchmark: Runtime by N and P\n(12 cores, median of 3 runs)',
                 fontsize=13, fontweight='bold', pad=15)

    # Legend at bottom
    ly = -0.01
    items = [
        (GREEN, 'Best / TALM wins (<0.97x)'),
        (YELLOW, 'Tie (0.97\u20131.03x)'),
        (L_ORANGE, 'GHC wins (1.03\u20132x)'),
        (L_RED, 'GHC wins (>2x)'),
    ]
    for i, (color, label) in enumerate(items):
        x = 0.08 + i * 0.24
        ax.add_patch(plt.Rectangle((x, ly), 0.012, 0.01,
                     transform=ax.transAxes, facecolor=color, edgecolor='gray'))
        ax.text(x + 0.016, ly + 0.005, label, transform=ax.transAxes,
                fontsize=7, va='center')

    for ext in ('pdf', 'png'):
        path = os.path.join(out_dir, f'nqueens_results_table.{ext}')
        fig.savefig(path, bbox_inches='tight', dpi=300)
    plt.close()
    print(f"Saved: {out_dir}/nqueens_results_table.pdf/.png")

if __name__ == '__main__':
    main()
