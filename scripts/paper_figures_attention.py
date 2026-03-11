#!/usr/bin/env python3
"""Generate speedup figure for self-attention benchmark (figure_spec.md)."""

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

# Data from figure_spec.md
P = [2, 4, 8, 16]

talm     = [1.64, 2.11, 3.39, 8.16]
ghc_str  = [1.11, 1.21, 1.69, 2.94]
ghc_pp   = [1.21, 1.30, 1.51, 3.20]

fig, ax = plt.subplots(figsize=(5.5, 3.8))

# Curves
ax.plot(P, talm,     color="#2166ac", ls="-",  lw=1.8, marker="o", ms=6, label="TALM")
ax.plot(P, ghc_str,  color="#d6604d", ls="-",  lw=1.4, marker="s", ms=5, label="GHC Strategies")
ax.plot(P, ghc_pp,   color="#4dac26", ls="-",  lw=1.4, marker="^", ms=5, label="GHC par/pseq")

# Axes
ax.set_xscale("log", base=2)
ax.set_xticks(P)
ax.set_xticklabels([str(p) for p in P])
ax.set_xlim(1.7, 19)
ax.set_ylim(0, 10)
ax.set_yticks(range(0, 11, 2))
ax.set_xlabel("Threads (P)")
ax.set_ylabel(r"Speedup $S_p = T_1 / T_p$")

# Clean spines
ax.spines["top"].set_visible(False)
ax.spines["right"].set_visible(False)

# Annotations
ax.annotate(r"8.16$\times$", xy=(16, 8.16), xytext=(5, 6),
            textcoords="offset points", fontsize=8, color="#2166ac")

# Legend
ax.legend(loc="upper left", frameon=False, fontsize=9)

fig.tight_layout()
fig.savefig("/home/ricardomag/Desktop/Ribault/results/attention/fig_speedup.pdf", dpi=300)
fig.savefig("/home/ricardomag/Desktop/Ribault/results/attention/fig_speedup.png", dpi=300)
print("Saved fig_speedup.pdf and fig_speedup.png")
