#!/usr/bin/env python3
"""Generate text search figures per textsearch_figures_spec.md."""

import matplotlib
matplotlib.use("Agg")
matplotlib.rcParams.update({
    'figure.figsize': (7, 4.2),
    'figure.dpi': 300,
    'font.family': 'serif',
    'font.serif': ['Times New Roman', 'DejaVu Serif'],
    'font.size': 10,
    'axes.titlesize': 12,
    'axes.titleweight': 'bold',
    'axes.labelsize': 11,
    'xtick.labelsize': 10,
    'ytick.labelsize': 10,
    'legend.fontsize': 9,
    'legend.frameon': False,
    'axes.grid': True,
    'grid.alpha': 0.15,
    'grid.color': '#cccccc',
    'grid.linestyle': '-',
    'axes.spines.top': False,
    'axes.spines.right': False,
    'axes.linewidth': 0.8,
    'savefig.bbox': 'tight',
    'savefig.pad_inches': 0.05,
})

import matplotlib.pyplot as plt
import numpy as np
import csv
import os
import statistics

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
CSV_PATH = os.path.join(SCRIPT_DIR, "run_all", "textsearch_metrics.csv")
FIG_DIR = os.path.join(SCRIPT_DIR, "figures")
os.makedirs(FIG_DIR, exist_ok=True)

# ---------- read CSV ----------
data = {}  # variant -> P -> [times]
with open(CSV_PATH) as f:
    reader = csv.DictReader(f)
    for row in reader:
        v = row["variant"]
        p = int(row["P"])
        t = float(row["seconds"])
        data.setdefault(v, {}).setdefault(p, []).append(t)

# median per (variant, P)
best = {}
for v in data:
    best[v] = {}
    for p in sorted(data[v]):
        best[v][p] = statistics.median(data[v][p])

Ps = [1, 2, 4, 8, 16, 32]

# ---------- speedup ----------
speedup = {}
for v in best:
    t1 = best[v][1]
    speedup[v] = {p: t1 / best[v][p] for p in Ps}

# ---------- plot ----------
STYLES = {
    "super":   {"color": "#2166ac", "marker": "o",  "label": "Ribault"},
    "ghc":     {"color": "#d6604d", "marker": "s",  "label": "GHC Strategies"},
    "parpseq": {"color": "#4dac26", "marker": "D",  "label": "GHC par/pseq"},
}

fig, ax = plt.subplots()

# dashed baseline at y=1
ax.axhline(y=1, color="gray", linewidth=0.8, linestyle="--", alpha=0.5)

for v in ["super", "ghc", "parpseq"]:
    s = STYLES[v]
    ys = [speedup[v][p] for p in Ps]
    ax.plot(Ps, ys, color=s["color"], marker=s["marker"], label=s["label"],
            linewidth=2.0, markersize=7, markeredgecolor="white",
            markeredgewidth=0.5, solid_capstyle="round")

ax.set_xscale("log", base=2)
ax.set_xticks(Ps)
ax.set_xticklabels([str(p) for p in Ps])
ax.set_ylim(0, 6)
ax.set_xlabel("Number of processors $P$")
ax.set_ylabel(r"Speedup  $S_p = T_1 / T_p$")
ax.set_title("Text Search: Speedup (320 MB corpus, K = 14)")
ax.legend(loc="upper left")

out = os.path.join(FIG_DIR, "fig_ts_speedup_vs_P.png")
fig.savefig(out)
plt.close(fig)
print(f"Saved {out}")
