#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate Figure 2 (pipeline) and Figure 4 (mergesort DFG) for Ribault ICFP paper."""

import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.patches import FancyBboxPatch, FancyArrowPatch
import numpy as np

# ── Common style ─────────────────────────────────────────────
matplotlib.rcParams.update({
    'font.family': 'serif',
    'font.serif': ['Times New Roman', 'DejaVu Serif'],
    'font.size': 10,
    'text.usetex': False,
})

OUTDIR = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                       '..', 'RESULTS', 'paper_figures')

# ═══════════════════════════════════════════════════════════════
# FIGURE 2: Compiler Pipeline
# ═══════════════════════════════════════════════════════════════

def fig2_pipeline():
    fig, ax = plt.subplots(figsize=(8.5, 2.5))
    ax.set_xlim(0, 100)
    ax.set_ylim(0, 28)
    ax.axis('off')

    box_fill = '#EDEDED'
    box_w = 13
    box_h = 14
    gap = 11          # enough gap for labels above boxes
    y_bot = 3
    y_mid = y_bot + box_h / 2   # = 10
    y_top = y_bot + box_h        # = 17

    phases = [
        ("Phase 1", "Front-end", "(Alex + Happy)"),
        ("Phase 2", "Semantic\nAnalysis", "type inference,\n$\\lambda$-lifting"),
        ("Phase 3", "Graph\nConstruction", "goExpr, tagging"),
        ("Phase 4", "Code\nGeneration", "assembly emission"),
    ]

    x0 = 5
    x_starts = [x0 + i * (box_w + gap) for i in range(4)]
    # x_starts = [5, 29, 53, 77]  box ends: 18, 42, 66, 90
    # gaps between boxes: 11 units each (~0.94 inch in 8.5" figure)

    # Draw phase boxes
    for i, (title, subtitle, detail) in enumerate(phases):
        bx = x_starts[i]
        rect = FancyBboxPatch((bx, y_bot), box_w, box_h,
                               boxstyle="round,pad=0.6",
                               facecolor=box_fill, edgecolor='black', linewidth=1.2)
        ax.add_patch(rect)
        cx = bx + box_w / 2
        ax.text(cx, y_mid + 2.8, title, ha='center', va='center',
                fontsize=9.5, fontweight='bold')
        ax.text(cx, y_mid - 0.3, subtitle, ha='center', va='center',
                fontsize=8)
        ax.text(cx, y_mid - 3.5, detail, ha='center', va='center',
                fontsize=6.5, fontstyle='italic', color='#444444')

    # Input arrow: "H_sub source" → Phase 1
    ax.annotate("", xy=(x_starts[0], y_mid),
                xytext=(x_starts[0] - 4, y_mid),
                arrowprops=dict(arrowstyle='->', color='black', lw=1.2))
    ax.text(x_starts[0] - 4.5, y_mid, r'$H_{sub}$ source',
            ha='right', va='center', fontsize=8.5, fontstyle='italic')

    # Inter-phase arrows with labels ABOVE the boxes (clear of box edges)
    arrow_labels = [
        r'$\mathrm{AST}\ [\mathrm{Decl}]$',
        r'Typed AST + $\Phi$',
        r'DFG $(V, E, \ell)$',
    ]
    for i in range(3):
        x_from = x_starts[i] + box_w
        x_to = x_starts[i + 1]
        ax.annotate("", xy=(x_to, y_mid),
                    xytext=(x_from, y_mid),
                    arrowprops=dict(arrowstyle='->', color='black', lw=1.2))
        label_x = (x_from + x_to) / 2
        ax.text(label_x, y_top + 1.5, arrow_labels[i],
                ha='center', va='bottom', fontsize=7, fontstyle='italic')

    # Phase 4 output: two forked arrows
    p4_right = x_starts[3] + box_w
    fork_len = 5
    fork_dy = 5
    # Upper output
    ax.annotate("", xy=(p4_right + fork_len, y_mid + fork_dy),
                xytext=(p4_right, y_mid + 1.2),
                arrowprops=dict(arrowstyle='->', color='black', lw=1.2))
    ax.text(p4_right + fork_len + 0.5, y_mid + fork_dy,
            "TALM assembly (.fl)",
            ha='left', va='center', fontsize=7.5, fontstyle='italic')
    # Lower output
    ax.annotate("", xy=(p4_right + fork_len, y_mid - fork_dy),
                xytext=(p4_right, y_mid - 1.2),
                arrowprops=dict(arrowstyle='->', color='black', lw=1.2))
    ax.text(p4_right + fork_len + 0.5, y_mid - fork_dy,
            "Super-instr. Haskell (.hs)",
            ha='left', va='center', fontsize=7.5, fontstyle='italic')

    os.makedirs(OUTDIR, exist_ok=True)
    fig.savefig(os.path.join(OUTDIR, "fig2_pipeline.png"), bbox_inches='tight', dpi=300)
    print("  [ok] fig2_pipeline.png")
    plt.close()


# ═══════════════════════════════════════════════════════════════
# FIGURE 4: MergeSort Dataflow Graph
# ═══════════════════════════════════════════════════════════════

def fig4_mergesort_dfg():
    fig, ax = plt.subplots(figsize=(7, 6.5))
    ax.set_xlim(-0.5, 7.5)
    ax.set_ylim(-1.2, 7.8)
    ax.set_aspect('equal')
    ax.axis('off')

    # Node definitions: (id, label, x, y, type)
    # type: 'io' = double circle, 'split'/'merge' = white rect, 'sort' = gray rect
    nodes = {
        'IN':     (3.5, 7.0, 'io',    'IN'),
        'sp1':    (3.5, 5.9, 'coord', 'split'),
        'sp2':    (1.75, 4.7, 'coord', 'split'),
        'sp3':    (5.25, 4.7, 'coord', 'split'),
        'S1':     (0.6, 3.5, 'super', r'sort $S_1$'),
        'S2':     (2.1, 3.5, 'super', r'sort $S_2$'),
        'S3':     (4.9, 3.5, 'super', r'sort $S_3$'),
        'S4':     (6.4, 3.5, 'super', r'sort $S_4$'),
        'mg1':    (1.75, 2.3, 'coord', 'merge'),
        'mg2':    (5.25, 2.3, 'coord', 'merge'),
        'mg3':    (3.5, 1.1, 'coord', 'merge'),
        'OUT':    (3.5, 0.0, 'io',    'OUT'),
    }

    edges = [
        ('IN', 'sp1'),
        ('sp1', 'sp2'), ('sp1', 'sp3'),
        ('sp2', 'S1'), ('sp2', 'S2'),
        ('sp3', 'S3'), ('sp3', 'S4'),
        ('S1', 'mg1'), ('S2', 'mg1'),
        ('S3', 'mg2'), ('S4', 'mg2'),
        ('mg1', 'mg3'), ('mg2', 'mg3'),
        ('mg3', 'OUT'),
    ]

    rect_w, rect_h = 1.0, 0.55
    io_r = 0.25
    gray = '#D0D0D0'

    def draw_io(ax, x, y, label):
        """Double-border circle."""
        c_outer = plt.Circle((x, y), io_r, facecolor='white',
                              edgecolor='black', linewidth=1.3, zorder=3)
        c_inner = plt.Circle((x, y), io_r * 0.75, facecolor='white',
                              edgecolor='black', linewidth=0.8, zorder=4)
        ax.add_patch(c_outer)
        ax.add_patch(c_inner)
        fs = 5.5 if label == 'OUT' else 9
        ax.text(x, y, label, ha='center', va='center',
                fontsize=fs, fontweight='bold', zorder=5)

    def draw_rect(ax, x, y, label, fill='white'):
        """Rounded rectangle node."""
        rect = FancyBboxPatch((x - rect_w/2, y - rect_h/2), rect_w, rect_h,
                               boxstyle="round,pad=0.06",
                               facecolor=fill, edgecolor='black', linewidth=1.0,
                               zorder=3)
        ax.add_patch(rect)
        ax.text(x, y, label, ha='center', va='center',
                fontsize=9, zorder=5)

    def node_edge_point(nid, direction='bottom'):
        """Get connection point on node boundary."""
        x, y, ntype, _ = nodes[nid]
        if ntype == 'io':
            if direction == 'bottom':
                return x, y - io_r
            else:
                return x, y + io_r
        else:
            if direction == 'bottom':
                return x, y - rect_h/2
            else:
                return x, y + rect_h/2

    # Draw edges first (below nodes)
    for src, dst in edges:
        x1, y1 = node_edge_point(src, 'bottom')
        x2, y2 = node_edge_point(dst, 'top')
        ax.annotate("", xy=(x2, y2), xytext=(x1, y1),
                    arrowprops=dict(arrowstyle='->', color='black', lw=1.0,
                                    shrinkA=0, shrinkB=0),
                    zorder=1)

    # Draw nodes
    for nid, (x, y, ntype, label) in nodes.items():
        if ntype == 'io':
            draw_io(ax, x, y, label)
        elif ntype == 'super':
            draw_rect(ax, x, y, label, fill=gray)
        else:
            draw_rect(ax, x, y, label, fill='white')

    # Right-side brace annotation for S1-S4
    brace_x = 7.15
    brace_y_top = 3.5 + rect_h/2 + 0.05
    brace_y_bot = 3.5 - rect_h/2 - 0.05
    # Draw brace manually with lines
    brace_mid = (brace_y_top + brace_y_bot) / 2
    tip_x = brace_x + 0.15
    ax.plot([brace_x, brace_x], [brace_y_bot, brace_y_top],
            color='black', lw=0.8, zorder=2)
    ax.plot([brace_x - 0.05, brace_x], [brace_y_top, brace_y_top],
            color='black', lw=0.8, zorder=2)
    ax.plot([brace_x - 0.05, brace_x], [brace_y_bot, brace_y_bot],
            color='black', lw=0.8, zorder=2)
    ax.plot([brace_x, tip_x], [brace_mid, brace_mid],
            color='black', lw=0.8, zorder=2)
    ax.text(tip_x + 0.05, brace_mid,
            r'$P{=}4$ leaf super-instructions' + '\n(GHC-compiled)',
            ha='left', va='center', fontsize=7, fontstyle='italic')

    # Depth labels on the left
    depth_levels = [
        (7.0, "depth 0"),
        (5.9, "depth 1"),
        (4.7, "depth 2"),
        (3.5, "depth 3"),
        (2.3, "depth 4"),
        (1.1, "depth 5"),
        (0.0, "depth 6"),
    ]
    for y, label in depth_levels:
        ax.text(-0.3, y, label, ha='right', va='center',
                fontsize=5.5, color='#999999', fontstyle='italic')

    # Legend at bottom — two entries, well separated
    leg_y = -0.8
    # Gray box (left entry)
    leg_gray = FancyBboxPatch((0.8, leg_y - 0.15), 0.4, 0.3,
                               boxstyle="round,pad=0.03",
                               facecolor=gray, edgecolor='black', linewidth=0.8)
    ax.add_patch(leg_gray)
    ax.text(1.35, leg_y, "super-instruction (GHC-compiled)",
            ha='left', va='center', fontsize=7)
    # White box (right entry, shifted further right)
    leg_white = FancyBboxPatch((4.8, leg_y - 0.15), 0.4, 0.3,
                                boxstyle="round,pad=0.03",
                                facecolor='white', edgecolor='black', linewidth=0.8)
    ax.add_patch(leg_white)
    ax.text(5.35, leg_y, "dataflow coordination node",
            ha='left', va='center', fontsize=7)

    os.makedirs(OUTDIR, exist_ok=True)
    fig.savefig(os.path.join(OUTDIR, "fig4_mergesort_dfg.png"), bbox_inches='tight', dpi=300)
    print("  [ok] fig4_mergesort_dfg.png")
    plt.close()


# ── Main ─────────────────────────────────────────────────────

if __name__ == "__main__":
    print("Generating paper figures...")
    fig2_pipeline()
    fig4_mergesort_dfg()
    print(f"Done. Output in {OUTDIR}")
