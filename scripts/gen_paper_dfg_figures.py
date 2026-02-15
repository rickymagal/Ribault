#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Generate detailed dataflow graph figures for correctness-test programs.

Style matches fig4_mergesort_dfg: large nodes, generous spacing, 9-11pt fonts.
"""

import os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch

matplotlib.rcParams.update({
    'font.family': 'serif',
    'font.serif': ['Times New Roman', 'DejaVu Serif'],
    'font.size': 11,
    'text.usetex': False,
})

OUTDIR = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                       '..', 'RESULTS', 'paper_figures')

GRAY = '#D0D0D0'
IO_R = 0.38       # IO circle radius
NW = 1.4           # default node width
NH = 0.6           # default node height
DY = 1.5           # default vertical spacing between rows


def draw_io(ax, x, y, label):
    c_outer = plt.Circle((x, y), IO_R, facecolor='white',
                          edgecolor='black', linewidth=1.5, zorder=10)
    c_inner = plt.Circle((x, y), IO_R * 0.72, facecolor='white',
                          edgecolor='black', linewidth=1.0, zorder=11)
    ax.add_patch(c_outer)
    ax.add_patch(c_inner)
    fs = 7.5 if label == 'OUT' else 10
    ax.text(x, y, label, ha='center', va='center',
            fontsize=fs, fontweight='bold', zorder=12)


def N(ax, x, y, label, fill='white', w=NW, h=NH, fs=10):
    """Draw a rounded rectangle node."""
    rect = FancyBboxPatch((x - w/2, y - h/2), w, h,
                           boxstyle="round,pad=0.06",
                           facecolor=fill, edgecolor='black',
                           linewidth=1.0, zorder=10)
    ax.add_patch(rect)
    ax.text(x, y, label, ha='center', va='center',
            fontsize=fs, zorder=12)
    return (x, y, w, h)


def func_box(ax, x, y, w, h, title):
    """Dashed rectangle around a function body. Title above the box."""
    rect = FancyBboxPatch((x, y), w, h,
                           boxstyle="round,pad=0.2",
                           facecolor='#F8F8F8', edgecolor='#888888',
                           linewidth=1.2, linestyle=(0, (5, 3)), zorder=0)
    ax.add_patch(rect)
    ax.text(x + 0.3, y + h + 0.15, title,
            ha='left', va='bottom', fontsize=11,
            fontweight='bold', color='#333333', zorder=15)


def A(ax, x1, y1, x2, y2, color='black', lw=1.0, ls='-'):
    """Arrow from (x1,y1) to (x2,y2)."""
    ax.annotate("", xy=(x2, y2), xytext=(x1, y1),
                arrowprops=dict(arrowstyle='->', color=color, lw=lw,
                                linestyle=ls, shrinkA=0, shrinkB=0),
                zorder=5)


def T(ax, x, y, text, **kw):
    """Text label with white background to stay readable."""
    defaults = dict(ha='center', va='center', fontsize=9,
                    fontstyle='italic', zorder=20,
                    bbox=dict(boxstyle='round,pad=0.12', facecolor='white',
                              edgecolor='none', alpha=0.92))
    defaults.update(kw)
    ax.text(x, y, text, **defaults)


def bot(y, h=NH): return y - h/2
def top(y, h=NH): return y + h/2


def legend(ax, cx, y):
    s = 0.35
    leg_gray = FancyBboxPatch((cx - 4.0, y - s/2), s, s,
                               boxstyle="round,pad=0.03",
                               facecolor=GRAY, edgecolor='black', linewidth=0.8)
    ax.add_patch(leg_gray)
    ax.text(cx - 3.5, y, "super-instruction (GHC-compiled)",
            ha='left', va='center', fontsize=9)
    leg_white = FancyBboxPatch((cx + 1.0, y - s/2), s, s,
                                boxstyle="round,pad=0.03",
                                facecolor='white', edgecolor='black', linewidth=0.8)
    ax.add_patch(leg_white)
    ax.text(cx + 1.5, y, "dataflow node (TALM-interpreted)",
            ha='left', va='center', fontsize=9)


def save(fig, name):
    os.makedirs(OUTDIR, exist_ok=True)
    for ext in ('png', 'pdf'):
        fig.savefig(os.path.join(OUTDIR, f"{name}.{ext}"),
                    bbox_inches='tight', dpi=300)
    print(f"  [ok] {name}")
    plt.close()


# ═══════════════════════════════════════════════════════════════
# FIBONACCI
# ═══════════════════════════════════════════════════════════════

def fig_fibonacci():
    fig, ax = plt.subplots(figsize=(12, 20))
    ax.set_xlim(-4, 14)
    ax.set_ylim(-6.5, 25)
    ax.set_aspect('equal')
    ax.axis('off')

    CX = 5.0

    # ── IN ──
    draw_io(ax, CX, 24.0, 'IN')

    # ── MAIN ──
    func_box(ax, 1.5, 16.8, 7.0, 6.0, 'main')

    y = 22.0
    N(ax, CX, y, 'const 10')
    A(ax, CX, 24.0 - IO_R, CX, top(y))

    y1 = y - DY
    N(ax, CX, y1, 'callsnd fib', w=1.6)
    A(ax, CX, bot(y), CX, top(y1))

    y2 = y1 - DY
    N(ax, CX, y2, 'retsnd')
    A(ax, CX, bot(y1), CX, top(y2))

    y3 = y2 - DY
    N(ax, CX, y3, 'callsnd\nprint_final', w=1.8, h=0.8, fs=10)
    A(ax, CX, bot(y2), CX, top(y3, 0.8))

    # ── FIB BODY ──
    func_box(ax, -2.5, 3.5, 15.0, 12.5, 'fib   (recursive template)')

    fy0 = 15.0
    N(ax, CX, fy0, 'n  (input)', w=1.4)
    A(ax, CX, bot(y1), CX, top(fy0) + 0.3, color='#0066CC', ls='--')

    fy1 = fy0 - DY
    N(ax, CX, fy1, 'const 2', w=1.2)
    A(ax, CX, bot(fy0), CX, top(fy1))

    fy2 = fy1 - DY
    N(ax, CX, fy2, r'lthan  ($n < 2$)', w=2.0)
    A(ax, CX, bot(fy1), CX, top(fy2))

    fy3 = fy2 - DY
    N(ax, CX, fy3, 'steer', w=1.2)
    A(ax, CX, bot(fy2), CX, top(fy3))

    # TRUE → return n (right side)
    tx = 10.0
    ty = fy3 - DY
    N(ax, tx, ty, 'return n', w=1.5)
    A(ax, CX + 1.2/2, bot(fy3) + 0.1, tx, top(ty), color='#006600')
    T(ax, 8.0, fy3 - 0.15, 'true', fontsize=11, color='#006600', fontweight='bold')

    # FALSE label
    T(ax, 2.0, fy3 - 0.15, 'false', fontsize=11, color='#CC0000', fontweight='bold')

    # n-1 path (left)
    lx = 0.5
    ly0 = fy3 - 1.8
    N(ax, lx, ly0, 'const 1', w=1.2, fs=10)
    ly1 = ly0 - DY
    N(ax, lx, ly1, r'sub ($n{-}1$)', w=1.5)
    ly2 = ly1 - DY
    N(ax, lx, ly2, 'callsnd fib', w=1.6, fs=10)
    ly3 = ly2 - DY
    N(ax, lx, ly3, r'retsnd $r_1$', w=1.5, fs=10)

    A(ax, CX - 1.2/2, bot(fy3) + 0.1, lx, top(ly0), color='#CC0000')
    A(ax, lx, bot(ly0), lx, top(ly1))
    A(ax, lx, bot(ly1), lx, top(ly2))
    A(ax, lx, bot(ly2), lx, top(ly3))

    # n feeds sub(n-1)
    A(ax, CX - NW/2, fy0, lx + 1.5/2, top(ly1), color='#AAAAAA', lw=0.6)

    # n-2 path (center-left)
    rx = 4.5
    ry0 = fy3 - 1.8
    N(ax, rx, ry0, 'const 2', w=1.2, fs=10)
    ry1 = ry0 - DY
    N(ax, rx, ry1, r'sub ($n{-}2$)', w=1.5)
    ry2 = ry1 - DY
    N(ax, rx, ry2, 'callsnd fib', w=1.6, fs=10)
    ry3 = ry2 - DY
    N(ax, rx, ry3, r'retsnd $r_2$', w=1.5, fs=10)

    A(ax, CX - 1.2/2, bot(fy3) + 0.1, rx, top(ry0), color='#CC0000')
    A(ax, rx, bot(ry0), rx, top(ry1))
    A(ax, rx, bot(ry1), rx, top(ry2))
    A(ax, rx, bot(ry2), rx, top(ry3))

    # n feeds sub(n-2)
    A(ax, CX, bot(fy0), rx + 1.5/2, top(ry1), color='#AAAAAA', lw=0.6)

    # ADD
    ay = 6.5
    N(ax, 8.5, ay, r'add ($r_1 + r_2$)', w=2.0)
    A(ax, lx + 1.5/2, ly3, 8.5 - 2.0/2, top(ay))
    A(ax, rx + 1.5/2, ry3, 8.5 - 0.3, top(ay))

    # RET FIB
    rfy = 4.8
    N(ax, tx, rfy, 'ret fib', w=1.4)
    A(ax, tx, bot(ty), tx, top(rfy), color='#006600', lw=0.8)
    A(ax, 8.5, bot(ay), tx, top(rfy))

    # Recursive back-edge (far left)
    ax.annotate("", xy=(-2.0, fy0), xytext=(-2.0, ly2),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.5,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=0.3'),
                zorder=3)
    T(ax, -3.5, (fy0 + ly2) / 2, 'recursive\ncallsnd',
      fontsize=9, color='#0066CC')

    # ret fib → retsnd in main
    ax.annotate("", xy=(CX + 2.2, y2), xytext=(12.0, rfy),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.0,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.12'),
                zorder=3)
    T(ax, 12.8, 12.0, 'return\nto caller', fontsize=8, color='#0066CC')

    # ── PRINT_FINAL ──
    func_box(ax, 2.0, -3.5, 6.0, 3.5, 'print_final')
    pfy0 = -0.7
    N(ax, CX, pfy0, 'r  (input)', w=1.3)
    pfy1 = pfy0 - DY
    N(ax, CX, pfy1, 'super #4\n(GHC print)', w=2.0, h=0.8, fs=10, fill=GRAY)

    A(ax, CX, bot(y3, 0.8), CX, top(pfy0) + 0.5, color='#0066CC', ls='--')
    A(ax, CX, bot(pfy0), CX, top(pfy1, 0.8))

    draw_io(ax, CX, -4.5, 'OUT')
    A(ax, CX, bot(pfy1, 0.8), CX, -4.5 + IO_R)

    legend(ax, CX, -5.5)
    save(fig, "fig_fibonacci_dfg")


# ═══════════════════════════════════════════════════════════════
# VECTOR SUM
# ═══════════════════════════════════════════════════════════════

def fig_vectorsum():
    fig, ax = plt.subplots(figsize=(14, 24))
    ax.set_xlim(-4, 14)
    ax.set_ylim(-6.5, 28)
    ax.set_aspect('equal')
    ax.axis('off')

    CX = 5.0

    # ── IN ──
    draw_io(ax, CX, 27.0, 'IN')

    # ── MAIN ──
    func_box(ax, -0.5, 19.5, 11.5, 6.5, 'main')

    y0 = 25.0
    N(ax, 2.0, y0, 'const 1,2,3,4', w=2.0, fs=10)
    N(ax, 8.0, y0, 'const 10,20,30,40', w=2.4, fs=10)
    A(ax, CX, 27.0 - IO_R, 2.0, top(y0))
    A(ax, CX, 27.0 - IO_R, 8.0, top(y0))

    y1 = y0 - 1.3
    N(ax, 2.0, y1, r'cons $\times$4 = [1,2,3,4]', w=2.4, h=0.7, fs=9)
    N(ax, 8.0, y1, r'cons $\times$4 = [10,20,30,40]', w=2.8, h=0.7, fs=9)
    A(ax, 2.0, bot(y0), 2.0, top(y1, 0.7))
    A(ax, 8.0, bot(y0), 8.0, top(y1, 0.7))

    y2 = y1 - DY
    N(ax, CX, y2, 'callsnd sumVec', w=2.0)
    A(ax, 2.0, bot(y1, 0.7), CX - 0.3, top(y2))
    A(ax, 8.0, bot(y1, 0.7), CX + 0.3, top(y2))

    y3 = y2 - DY
    N(ax, CX, y3, 'retsnd', w=1.4)
    A(ax, CX, bot(y2), CX, top(y3))

    y4 = y3 - DY
    N(ax, CX, y4, 'callsnd print_final', w=2.4)
    A(ax, CX, bot(y3), CX, top(y4))

    # ── SUMVEC BODY ──
    func_box(ax, -3.0, 2.5, 16.0, 16.0, 'sumVec   (recursive template)')

    sy = 17.5
    N(ax, 2.5, sy, 'xs  (input)', w=1.5)
    N(ax, 7.5, sy, 'ys  (input)', w=1.5)
    A(ax, CX, bot(y2), 2.5, top(sy) + 0.3, color='#0066CC', ls='--')
    A(ax, CX, bot(y2), 7.5, top(sy) + 0.3, color='#0066CC', ls='--')

    # case xs
    sy1 = sy - DY
    N(ax, 2.5, sy1, 'isNil xs', w=1.5)
    A(ax, 2.5, bot(sy), 2.5, top(sy1))

    sy2 = sy1 - DY
    N(ax, 2.5, sy2, 'steer', w=1.2)
    A(ax, 2.5, bot(sy1), 2.5, top(sy2))

    # [] branch xs
    N(ax, -1.5, sy2 - DY, 'return []', w=1.4, fs=10)
    A(ax, 2.5 - 1.2/2, bot(sy2) + 0.1, -1.5, top(sy2 - DY), color='#006600')
    T(ax, 0.0, sy2 - 0.4, '[]', fontsize=11, color='#006600', fontweight='bold')

    # (x:xt) branch
    T(ax, 4.0, sy2 - 0.4, '(x:xt)', fontsize=10, color='#CC0000', fontweight='bold')

    sy3 = sy2 - 1.8
    N(ax, 1.0, sy3, 'head xs', w=1.4)
    N(ax, 4.0, sy3, 'tail xs', w=1.4)
    A(ax, 2.5, bot(sy2), 1.0, top(sy3), color='#CC0000')
    A(ax, 2.5, bot(sy2), 4.0, top(sy3), color='#CC0000')
    T(ax, 1.0, sy3 - 0.6, '= x', fontsize=9, color='#444444')
    T(ax, 4.0, sy3 - 0.6, '= xt', fontsize=9, color='#444444')

    # case ys
    sy1b = sy - DY
    N(ax, 7.5, sy1b, 'isNil ys', w=1.5)
    A(ax, 7.5, bot(sy), 7.5, top(sy1b))

    sy2b = sy1b - DY
    N(ax, 7.5, sy2b, 'steer', w=1.2)
    A(ax, 7.5, bot(sy1b), 7.5, top(sy2b))

    # [] branch ys
    N(ax, 11.0, sy2b - DY, 'return []', w=1.4, fs=10)
    A(ax, 7.5 + 1.2/2, bot(sy2b) + 0.1, 11.0, top(sy2b - DY), color='#006600')
    T(ax, 9.8, sy2b - 0.4, '[]', fontsize=11, color='#006600', fontweight='bold')

    # (y:yt) branch
    T(ax, 6.0, sy2b - 0.4, '(y:yt)', fontsize=10, color='#CC0000', fontweight='bold')

    sy3b = sy2b - 1.8
    N(ax, 6.2, sy3b, 'head ys', w=1.4)
    N(ax, 9.2, sy3b, 'tail ys', w=1.4)
    A(ax, 7.5, bot(sy2b), 6.2, top(sy3b), color='#CC0000')
    A(ax, 7.5, bot(sy2b), 9.2, top(sy3b), color='#CC0000')
    T(ax, 6.2, sy3b - 0.6, '= y', fontsize=9, color='#444444')
    T(ax, 9.2, sy3b - 0.6, '= yt', fontsize=9, color='#444444')

    # add (x + y)
    ay = sy3 - 2.8
    N(ax, 2.5, ay, r'add  ($x + y$)', w=1.8)
    A(ax, 1.0, bot(sy3), 2.5 - 0.3, top(ay))
    A(ax, 6.2, bot(sy3b), 2.5 + 0.3, top(ay))

    # callsnd sumVec recursive
    cy = sy3b - 2.8
    N(ax, 7.5, cy, 'callsnd\nsumVec', w=1.7, h=0.8, fs=10)
    A(ax, 4.0, bot(sy3), 7.5 - 0.3, top(cy, 0.8))
    A(ax, 9.2, bot(sy3b), 7.5 + 0.3, top(cy, 0.8))

    ry = cy - DY
    N(ax, 7.5, ry, r'retsnd = $rest$', w=1.8)
    A(ax, 7.5, bot(cy, 0.8), 7.5, top(ry))

    # cons
    coy = ry - DY
    N(ax, CX, coy, r'cons  $(x{+}y) : rest$', w=2.4)
    A(ax, 2.5, bot(ay), CX - 0.5, top(coy))
    A(ax, 7.5, bot(ry), CX + 0.5, top(coy))

    rety = coy - DY
    N(ax, CX, rety, 'ret sumVec', w=1.6)
    A(ax, CX, bot(coy), CX, top(rety))

    # Recursive back-edge
    ax.annotate("", xy=(12.0, sy), xytext=(12.0, cy),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.5,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.3'),
                zorder=3)
    T(ax, 13.0, (sy + cy) / 2, 'recursive\ncallsnd',
      fontsize=9, color='#0066CC')

    # ret → retsnd in main
    ax.annotate("", xy=(CX + 2.5, y3), xytext=(12.0, rety),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.0,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.1'),
                zorder=3)

    # ── PRINT_FINAL ──
    func_box(ax, 2.0, -4.0, 6.0, 3.5, 'print_final')
    N(ax, CX, -1.2, 'xs  (input)', w=1.4)
    N(ax, CX, -2.8, 'super #4\n(GHC print)', w=2.0, h=0.8, fs=10, fill=GRAY)
    A(ax, CX, bot(y4), CX, top(-1.2) + 0.5, color='#0066CC', ls='--')
    A(ax, CX, bot(-1.2), CX, top(-2.8, 0.8))

    draw_io(ax, CX, -4.8, 'OUT')
    A(ax, CX, bot(-2.8, 0.8), CX, -4.8 + IO_R)

    legend(ax, CX, -5.8)
    save(fig, "fig_vectorsum_dfg")


# ═══════════════════════════════════════════════════════════════
# MATRIX MULTIPLICATION
# ═══════════════════════════════════════════════════════════════

def fig_matmul():
    """Matrix multiplication DFG — vertically stacked, no overlaps."""
    fig, ax = plt.subplots(figsize=(20, 42))
    ax.set_xlim(-6, 22)
    ax.set_ylim(-8, 50)
    ax.set_aspect('equal')
    ax.axis('off')

    CX = 8.0

    # ── IN ──
    draw_io(ax, CX, 49.0, 'IN')

    # ══════════════════════════════════════════════
    # MAIN  (top)
    # ══════════════════════════════════════════════
    func_box(ax, 1.0, 41.0, 14.0, 7.0, 'main')

    y0 = 47.0
    N(ax, 4.5, y0, 'A = [[1.5, 2.25],\n     [3.75, 4.5]]', w=3.0, h=0.9, fs=9)
    N(ax, 11.5, y0, 'B = [[0.5, 1.25],\n     [2.5, 3.5]]', w=3.0, h=0.9, fs=9)
    A(ax, CX, 49.0 - IO_R, 4.5, top(y0, 0.9))
    A(ax, CX, 49.0 - IO_R, 11.5, top(y0, 0.9))

    y1 = y0 - 2.0
    N(ax, 11.5, y1, 'callsnd\ntranspose(B)', w=2.2, h=0.8, fs=10)
    A(ax, 11.5, bot(y0, 0.9), 11.5, top(y1, 0.8))

    y1b = y1 - 1.8
    N(ax, 11.5, y1b, 'retsnd = colsT', w=2.2, fs=10)
    A(ax, 11.5, bot(y1, 0.8), 11.5, top(y1b))

    y2 = y1b - 1.8
    N(ax, CX, y2, 'callsnd\nmmult(A, colsT)', w=2.6, h=0.8, fs=10)
    A(ax, 11.5, bot(y1b), CX + 0.8, top(y2, 0.8))
    A(ax, 4.5, bot(y0, 0.9), CX - 0.8, top(y2, 0.8))

    # ══════════════════════════════════════════════
    # TRANSPOSE  (right, below main)
    # ══════════════════════════════════════════════
    func_box(ax, 10.0, 32.5, 10.5, 7.5, 'transpose  (recursive)')

    ty0 = 39.0
    N(ax, 15.0, ty0, 'xss  (input)', w=1.8, fs=11)
    A(ax, 11.5, bot(y1, 0.8), 15.0, top(ty0) + 0.3, color='#0066CC', ls='--')

    ty1 = ty0 - DY
    N(ax, 13.0, ty1, 'case xss', w=1.5, fs=10)
    N(ax, 17.0, ty1, 'case row', w=1.4, fs=10)
    A(ax, 15.0, bot(ty0), 13.0, top(ty1))
    A(ax, 15.0, bot(ty0), 17.0, top(ty1))
    T(ax, 11.0, ty1, r'[] $\rightarrow$ []', fontsize=9, color='#006600')

    ty2 = ty1 - 1.8
    N(ax, 12.5, ty2, 'heads xss', w=1.6, fs=10)
    N(ax, 17.0, ty2, 'tails xss +\ncallsnd\ntranspose', w=2.2, h=0.9, fs=9)
    A(ax, 13.0, bot(ty1), 12.5, top(ty2))
    A(ax, 17.0, bot(ty1), 17.0, top(ty2, 0.9))

    ty3 = ty2 - 1.8
    N(ax, 15.0, ty3, 'cons\nheads : rest', w=2.0, h=0.8, fs=10)
    A(ax, 12.5, bot(ty2), 15.0 - 0.5, top(ty3, 0.8))
    A(ax, 17.0, bot(ty2, 0.9), 15.0 + 0.5, top(ty3, 0.8))

    # transpose rec arrow
    ax.annotate("", xy=(19.5, ty0), xytext=(19.5, ty2),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.2,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.3'),
                zorder=3)
    T(ax, 20.3, ty1, 'recursive\ncallsnd', fontsize=8, color='#0066CC')

    # ══════════════════════════════════════════════
    # MMULT  (left, below main)
    # ══════════════════════════════════════════════
    func_box(ax, -4.0, 32.5, 11.5, 7.5, 'mmult  (recursive)')

    my0 = 39.0
    N(ax, 2.0, my0, 'rows, colsT', w=2.0, fs=11)
    A(ax, CX, bot(y2, 0.8), 2.0, top(my0) + 0.3, color='#0066CC', ls='--')

    my1 = my0 - DY
    N(ax, 2.0, my1, 'case rows', w=1.6, fs=10)
    A(ax, 2.0, bot(my0), 2.0, top(my1))
    T(ax, -1.5, my1, r'[] $\rightarrow$ []', fontsize=9, color='#006600')

    my2 = my1 - 1.8
    N(ax, -0.5, my2, 'callsnd\nrowTimes\n(r, colsT)', w=2.0, h=0.9, fs=9)
    N(ax, 4.5, my2, 'callsnd\nmmult\n(rs, colsT)', w=2.0, h=0.9, fs=9)
    A(ax, 2.0, bot(my1), -0.5, top(my2, 0.9))
    A(ax, 2.0, bot(my1), 4.5, top(my2, 0.9))

    my3 = my2 - 1.8
    N(ax, 2.0, my3, 'cons  h : ts', w=1.8, fs=10)
    A(ax, -0.5, bot(my2, 0.9), 2.0 - 0.3, top(my3))
    A(ax, 4.5, bot(my2, 0.9), 2.0 + 0.3, top(my3))

    # mmult rec arrow
    ax.annotate("", xy=(6.5, my0), xytext=(6.5, my2),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.2,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.3'),
                zorder=3)
    T(ax, 7.3, my1, 'recursive\ncallsnd', fontsize=8, color='#0066CC')

    # ══════════════════════════════════════════════
    # ROWTIMES  (left, below mmult)
    # ══════════════════════════════════════════════
    func_box(ax, -4.0, 22.0, 11.5, 8.5, 'rowTimes  (recursive)')

    rt0 = 29.5
    N(ax, 2.0, rt0, 'row, colsT', w=1.8, fs=11)
    A(ax, -0.5, bot(my2, 0.9), 2.0, top(rt0) + 0.3, color='#0066CC', ls='--')

    rt1 = rt0 - DY
    N(ax, 2.0, rt1, 'case colsT', w=1.7, fs=10)
    A(ax, 2.0, bot(rt0), 2.0, top(rt1))
    T(ax, -1.5, rt1, r'[] $\rightarrow$ []', fontsize=9, color='#006600')

    rt2 = rt1 - 1.8
    N(ax, -0.5, rt2, 'callsnd\ndot(row, c)', w=1.8, h=0.8, fs=9)
    N(ax, 4.5, rt2, 'callsnd\nrowTimes\n(row, cs)', w=2.0, h=0.9, fs=9)
    A(ax, 2.0, bot(rt1), -0.5, top(rt2, 0.8))
    A(ax, 2.0, bot(rt1), 4.5, top(rt2, 0.9))

    rt3 = rt2 - 1.8
    N(ax, 2.0, rt3, 'cons  h : ts', w=1.8, fs=10)
    A(ax, -0.5, bot(rt2, 0.8), 2.0 - 0.3, top(rt3))
    A(ax, 4.5, bot(rt2, 0.9), 2.0 + 0.3, top(rt3))

    # rowTimes rec arrow
    ax.annotate("", xy=(6.5, rt0), xytext=(6.5, rt2),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.2,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.3'),
                zorder=3)
    T(ax, 7.3, rt1, 'recursive\ncallsnd', fontsize=8, color='#0066CC')

    # ══════════════════════════════════════════════
    # DOT  (right, below transpose)
    # ══════════════════════════════════════════════
    func_box(ax, 9.0, 18.0, 12.0, 12.0, 'dot  (recursive)')

    dt0 = 29.0
    N(ax, 15.0, dt0, 'xs, ys', w=1.4, fs=11)
    A(ax, -0.5, bot(rt2, 0.8), 15.0, top(dt0) + 0.3, color='#0066CC', ls='--')

    dt1 = dt0 - DY
    N(ax, 12.5, dt1, 'case xs', w=1.4, fs=10)
    N(ax, 17.5, dt1, 'case ys', w=1.4, fs=10)
    A(ax, 15.0, bot(dt0), 12.5, top(dt1))
    A(ax, 15.0, bot(dt0), 17.5, top(dt1))
    T(ax, 10.5, dt1, r'[] $\rightarrow$ 0.0', fontsize=9, color='#006600')
    T(ax, 19.5, dt1, r'[] $\rightarrow$ 0.0', fontsize=9, color='#006600')

    dt2 = dt1 - 1.8
    N(ax, 11.0, dt2, 'head x', w=1.3, fs=10)
    N(ax, 14.0, dt2, r'tail $xs_1$', w=1.4, fs=10)
    N(ax, 16.5, dt2, 'head y', w=1.3, fs=10)
    N(ax, 19.0, dt2, r'tail $ys_1$', w=1.4, fs=10)
    A(ax, 12.5, bot(dt1), 11.0, top(dt2))
    A(ax, 12.5, bot(dt1), 14.0, top(dt2))
    A(ax, 17.5, bot(dt1), 16.5, top(dt2))
    A(ax, 17.5, bot(dt1), 19.0, top(dt2))

    dt3 = dt2 - 1.8
    N(ax, 12.5, dt3, r'mul ($x \times y$)', w=2.0, fs=10)
    N(ax, 17.5, dt3, 'callsnd\n' + r'dot($xs_1, ys_1$)', w=2.2, h=0.8, fs=9)
    A(ax, 11.0, bot(dt2), 12.5 - 0.3, top(dt3))
    A(ax, 16.5, bot(dt2), 12.5 + 0.3, top(dt3))
    A(ax, 14.0, bot(dt2), 17.5 - 0.3, top(dt3, 0.8))
    A(ax, 19.0, bot(dt2), 17.5 + 0.3, top(dt3, 0.8))

    dt4 = dt3 - 2.0
    N(ax, 15.0, dt4, r'add ($x{\times}y + rest$)', w=2.8, fs=10)
    A(ax, 12.5, bot(dt3), 15.0 - 0.5, top(dt4))
    A(ax, 17.5, bot(dt3, 0.8), 15.0 + 0.5, top(dt4))

    # return value from dot
    dt5 = dt4 - 1.8
    N(ax, 15.0, dt5, 'ret dot', w=1.5, fs=10)
    A(ax, 15.0, bot(dt4), 15.0, top(dt5))

    # dot rec arrow
    ax.annotate("", xy=(20.5, dt0), xytext=(20.5, dt3),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.2,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.3'),
                zorder=3)
    T(ax, 21.3, dt1, 'recursive\ncallsnd', fontsize=8, color='#0066CC')

    # ══════════════════════════════════════════════
    # HELPERS NOTE
    # ══════════════════════════════════════════════
    T(ax, 8.0, 16.5,
      'heads, tails:  recursive list traversal'
      r'  (case $\rightarrow$ head/tail $\rightarrow$ cons)',
      fontsize=9, color='#777777',
      bbox=dict(boxstyle='round,pad=0.4', facecolor='#F0F0F0',
                edgecolor='#CCCCCC', linewidth=0.6, alpha=1.0))

    # ══════════════════════════════════════════════
    # PRINT_FINAL  (bottom center, closer to dot)
    # ══════════════════════════════════════════════
    pf_top = 15.0
    T(ax, CX, pf_top + 1.0, 'callsnd print_final (from main)',
      fontsize=9, color='#0066CC')
    A(ax, CX, pf_top + 0.7, CX, top(pf_top - 0.8), color='#0066CC', ls='--')

    func_box(ax, 4.5, pf_top - 4.5, 7.0, 5.0, 'print_final')
    N(ax, CX, pf_top - 0.8, 'xs  (input)', w=1.6, fs=11)
    N(ax, CX, pf_top - 2.5, 'super #4\n(GHC print)', w=2.2, h=0.8, fs=10, fill=GRAY)
    A(ax, CX, bot(pf_top - 0.8), CX, top(pf_top - 2.5, 0.8))

    N(ax, CX, pf_top - 4.0, 'ret main', w=1.5, fs=10)
    A(ax, CX, bot(pf_top - 2.5, 0.8), CX, top(pf_top - 4.0))

    draw_io(ax, CX, pf_top - 5.5, 'OUT')
    A(ax, CX, bot(pf_top - 4.0), CX, pf_top - 5.5 + IO_R)

    # ── INSTRUCTION SUMMARY ──
    T(ax, CX, pf_top - 7.5,
      'matmul: main + recursive mmult, transpose, rowTimes, dot'
      '  +  1 GHC super (print)',
      fontsize=9, color='#888888',
      bbox=dict(boxstyle='round,pad=0.35', facecolor='#F5F5F5',
                edgecolor='#DDDDDD', linewidth=0.5, alpha=1.0))

    legend(ax, CX, pf_top - 9.5)
    save(fig, "fig_matmul_dfg")


# ═══════════════════════════════════════════════════════════════
# HELLO WORLD
# ═══════════════════════════════════════════════════════════════

def fig_helloworld():
    fig, ax = plt.subplots(figsize=(20, 22))
    ax.set_xlim(-5, 20)
    ax.set_ylim(-7.5, 24)
    ax.set_aspect('equal')
    ax.axis('off')

    draw_io(ax, 8.0, 23.0, 'IN')

    # 6 pairs, wide spacing
    px = [0.5, 4.0, 7.5, 11.0, 14.5, 18.0]
    pw = 2.5

    pairs = [
        ('[72,101]', '"He"'),
        ('[108,108]', '"ll"'),
        ('[111,32]', '"o "'),
        ('[87,111]', '"Wo"'),
        ('[114,108]', '"rl"'),
        ('[100,33]', '"d!"'),
    ]
    chars = ['H', 'e', 'l', 'l', 'o', ' ', 'W', 'o', 'r', 'l', 'd', '!']

    # ── STAGE LABELS ──
    sx = -4.0
    T(ax, sx, 21.5, 'build\nconstant\npairs', fontsize=10, color='#333333',
      fontweight='bold', bbox=dict(boxstyle='round,pad=0.25', facecolor='#E8E8E8',
                                    edgecolor='#999999', linewidth=0.7, alpha=1.0))
    T(ax, sx, 17.5, 'case\n(outer)\nisNil\nhead + tail', fontsize=9, color='#333333',
      fontweight='bold', bbox=dict(boxstyle='round,pad=0.25', facecolor='#E8E8E8',
                                    edgecolor='#999999', linewidth=0.7, alpha=1.0))
    T(ax, sx, 13.5, 'case\n(inner)\nisNil\nhead', fontsize=9, color='#333333',
      fontweight='bold', bbox=dict(boxstyle='round,pad=0.25', facecolor='#E8E8E8',
                                    edgecolor='#999999', linewidth=0.7, alpha=1.0))
    T(ax, sx, 10.5, 'guard\nbands', fontsize=10, color='#333333',
      fontweight='bold', bbox=dict(boxstyle='round,pad=0.25', facecolor='#E8E8E8',
                                    edgecolor='#999999', linewidth=0.7, alpha=1.0))
    T(ax, sx, 8.5, 'assemble\nlist', fontsize=10, color='#333333',
      fontweight='bold', bbox=dict(boxstyle='round,pad=0.25', facecolor='#E8E8E8',
                                    edgecolor='#999999', linewidth=0.7, alpha=1.0))

    # ── Layer 1: Constants ──
    for i, (vals, lbl) in enumerate(pairs):
        cx = px[i]
        N(ax, cx, 22.0, f'const\n{vals}', w=pw, h=0.8, fs=10)
        A(ax, 8.0, 23.0 - IO_R, cx, top(22.0, 0.8), lw=0.6, color='#AAAAAA')

    # ── Layer 2: Cons pairs ──
    for i, (vals, lbl) in enumerate(pairs):
        cx = px[i]
        N(ax, cx, 20.3, f'cons x2\n{lbl}', w=pw, h=0.8, fs=10)
        A(ax, cx, bot(22.0, 0.8), cx, top(20.3, 0.8))

    # ── Layer 3: Case outer (isNil + steer) ──
    for i in range(6):
        cx = px[i]
        N(ax, cx, 18.3, 'isNil + steer', w=pw * 0.85, h=0.65, fs=9.5)
        A(ax, cx, bot(20.3, 0.8), cx, top(18.3, 0.65))

    # ── Layer 4: head + tail ──
    for i in range(6):
        cx = px[i]
        N(ax, cx - 0.7, 16.5, 'head', w=1.1, h=0.55, fs=10)
        N(ax, cx + 0.7, 16.5, 'tail', w=1.1, h=0.55, fs=10)
        A(ax, cx - 0.2, bot(18.3, 0.65), cx - 0.7, top(16.5, 0.55))
        A(ax, cx + 0.2, bot(18.3, 0.65), cx + 0.7, top(16.5, 0.55))

    # char labels below head (outer)
    for i in range(6):
        cx = px[i]
        T(ax, cx - 0.7, 15.7, f'= {chars[2*i]}',
          fontsize=10, color='#006600', fontweight='bold')

    # ── Layer 5: Case inner (isNil + steer on tail) ──
    for i in range(6):
        cx = px[i]
        N(ax, cx + 0.7, 14.3, 'isNil + steer', w=pw * 0.7, h=0.6, fs=8.5)
        A(ax, cx + 0.7, bot(16.5, 0.55), cx + 0.7, top(14.3, 0.6))

    # ── Layer 6: head (inner) ──
    for i in range(6):
        cx = px[i]
        N(ax, cx + 0.7, 12.8, 'head', w=1.1, h=0.55, fs=10)
        A(ax, cx + 0.7, bot(14.3, 0.6), cx + 0.7, top(12.8, 0.55))

    # char labels below head (inner)
    for i in range(6):
        cx = px[i]
        T(ax, cx + 0.7, 12.0, f'= {chars[2*i+1]}',
          fontsize=10, color='#006600', fontweight='bold')

    # ── Layer 7: Band gates ──
    N(ax, 9.0, 10.5,
      r'band gates x94:  sequential guards verify each case matched',
      w=14.0, h=0.7, fs=10)
    for i in range(6):
        cx = px[i]
        A(ax, cx - 0.7, 15.7 - 0.35, 9.0 - 6.0, top(10.5, 0.7),
          lw=0.4, color='#CCCCCC')
        A(ax, cx + 0.7, 12.0 - 0.35, 9.0, top(10.5, 0.7),
          lw=0.4, color='#CCCCCC')

    # ── Layer 8: Assemble ──
    N(ax, 9.0, 8.5,
      'cons x12:   72 : 101 : 108 : 108 : 111 : 32 : 87 : 111 : 114 : 108 : 100 : 33 : []',
      w=16.0, h=0.7, fs=10)
    A(ax, 9.0, bot(10.5, 0.7), 9.0, top(8.5, 0.7))

    # ── callsnd print_final ──
    N(ax, 9.0, 6.0, 'callsnd print_final', w=2.6, fs=11)
    A(ax, 9.0, bot(8.5, 0.7), 9.0, top(6.0))

    # ── PRINT_FINAL BODY ──
    func_box(ax, 5.5, -1.0, 7.0, 3.5, 'print_final')
    N(ax, 9.0, 1.8, 'cs  (input)', w=1.5)
    N(ax, 9.0, 0.0, 'super #4\n(GHC: map toEnum,\nprint string)',
      w=3.0, h=1.0, fs=10, fill=GRAY)

    A(ax, 9.0, bot(6.0), 9.0, top(1.8) + 0.5, color='#0066CC', ls='--')
    A(ax, 9.0, bot(1.8), 9.0, top(0.0, 1.0))

    N(ax, 9.0, -2.0, 'ret main', w=1.5)
    A(ax, 9.0, bot(0.0, 1.0), 9.0, top(-2.0))
    draw_io(ax, 9.0, -3.5, 'OUT')
    A(ax, 9.0, bot(-2.0), 9.0, -3.5 + IO_R)

    # Instruction count
    T(ax, 9.0, -5.0,
      '947 TALM instructions:  48 cons  +  36 isNil  +  24 head  +  '
      '24 tail  +  94 band  +  48 steer  +  46 const  +  ...  +  1 GHC super',
      fontsize=8.5, color='#888888',
      bbox=dict(boxstyle='round,pad=0.35', facecolor='#F5F5F5',
                edgecolor='#DDDDDD', linewidth=0.5, alpha=1.0))

    legend(ax, 9.0, -6.5)
    save(fig, "fig_helloworld_dfg")


# ═══════════════════════════════════════════════════════════════
# MERGE SORT  (07_merge_sort.hsk)
# ═══════════════════════════════════════════════════════════════

def fig_mergesort():
    """Merge sort DFG — vertically stacked, split and merge fully below mergeSort."""
    fig, ax = plt.subplots(figsize=(22, 62))
    ax.set_xlim(-8, 26)
    ax.set_ylim(-10, 72)
    ax.set_aspect('equal')
    ax.axis('off')

    CX = 8.0

    # ── IN ──
    draw_io(ax, CX, 71.0, 'IN')

    # ══════════════════════════════════════════════
    # MAIN
    # ══════════════════════════════════════════════
    func_box(ax, 2.0, 62.5, 12.0, 7.5, 'main')

    y0 = 69.0
    N(ax, CX, y0, 'const 34, 7, 23, 32, 5, 62', w=3.5, fs=11)
    A(ax, CX, 71.0 - IO_R, CX, top(y0))

    y1 = y0 - 1.5
    N(ax, CX, y1, r'cons $\times$6 = [34,7,23,32,5,62]', w=3.8, h=0.7, fs=11)
    A(ax, CX, bot(y0), CX, top(y1, 0.7))

    y2 = y1 - 1.8
    N(ax, CX, y2, 'callsnd mergeSort', w=2.5, fs=11)
    A(ax, CX, bot(y1, 0.7), CX, top(y2))

    y3 = y2 - 1.8
    N(ax, CX, y3, 'retsnd', w=1.5, fs=11)
    A(ax, CX, bot(y2), CX, top(y3))

    y4 = y3 - 1.8
    N(ax, CX, y4, 'callsnd print_final', w=2.6, fs=11)
    A(ax, CX, bot(y3), CX, top(y4))

    # ══════════════════════════════════════════════
    # mergeSort (recursive template)
    # ══════════════════════════════════════════════
    ms_top = 61.0
    ms_bot = 43.0
    func_box(ax, -5.0, ms_bot, 26.0, ms_top - ms_bot, 'mergeSort   (recursive template)')

    ms0 = 60.0
    N(ax, CX, ms0, 'lst  (input)', w=1.8, fs=11)
    A(ax, CX, bot(y2), CX, top(ms0) + 0.3, color='#0066CC', ls='--')

    ms1 = ms0 - 1.8
    N(ax, CX, ms1, 'isNil lst', w=1.7, fs=11)
    A(ax, CX, bot(ms0), CX, top(ms1))

    ms2 = ms1 - 1.8
    N(ax, CX, ms2, 'steer', w=1.3, fs=11)
    A(ax, CX, bot(ms1), CX, top(ms2))

    # [] -> return []
    N(ax, -1.5, ms2 - 1.8, 'return []', w=1.6, fs=11)
    A(ax, CX - 1.3/2, bot(ms2) + 0.1, -1.5 + 1.6/2, top(ms2 - 1.8), color='#006600')
    T(ax, 2.5, ms2 - 0.3, '[]', fontsize=12, color='#006600', fontweight='bold')

    T(ax, 11.0, ms2 - 0.3, '(x:xt)', fontsize=11, color='#CC0000', fontweight='bold')

    ms3 = ms2 - 2.0
    N(ax, 6.0, ms3, 'head lst', w=1.6, fs=11)
    N(ax, 11.0, ms3, 'tail lst', w=1.6, fs=11)
    A(ax, CX + 0.3, bot(ms2), 6.0 + 0.3, top(ms3), color='#CC0000')
    A(ax, CX + 0.5, bot(ms2), 11.0, top(ms3), color='#CC0000')

    ms4 = ms3 - 1.8
    N(ax, 11.0, ms4, 'isNil (tail)', w=1.8, fs=11)
    A(ax, 11.0, bot(ms3), 11.0, top(ms4))

    ms5 = ms4 - 1.8
    N(ax, 11.0, ms5, 'steer', w=1.3, fs=11)
    A(ax, 11.0, bot(ms4), 11.0, top(ms5))

    N(ax, 6.0, ms5, r'return [head]', w=1.9, fs=10)
    A(ax, 11.0 - 1.3/2, bot(ms5) + 0.1, 6.0 + 1.9/2, top(ms5), color='#006600')
    T(ax, 8.0, ms5 + 0.65, '[x]', fontsize=11, color='#006600', fontweight='bold')
    A(ax, 6.0, bot(ms3), 6.0, top(ms5), color='#AAAAAA', lw=0.6)

    T(ax, 14.0, ms5 + 0.65, r'$\_$ (len$\geq$2)', fontsize=10, color='#CC0000', fontweight='bold')

    ms6 = ms5 - 2.0
    N(ax, 14.0, ms6, 'callsnd split', w=2.0, fs=11)
    A(ax, 11.0 + 1.3/2, bot(ms5) + 0.1, 14.0 - 0.3, top(ms6), color='#CC0000')

    ms7 = ms6 - 1.8
    N(ax, 14.0, ms7, 'retsnd\n(left, right)', w=2.2, h=0.8, fs=10)
    A(ax, 14.0, bot(ms6), 14.0, top(ms7, 0.8))

    ms8 = ms7 - 1.8
    N(ax, 10.5, ms8, 'fst = left', w=1.7, fs=10)
    N(ax, 17.0, ms8, 'snd = right', w=1.8, fs=10)
    A(ax, 14.0 - 0.5, bot(ms7, 0.8), 10.5, top(ms8))
    A(ax, 14.0 + 0.5, bot(ms7, 0.8), 17.0, top(ms8))

    ms9 = ms8 - 1.8
    N(ax, 10.5, ms9, 'callsnd\nmergeSort left', w=2.2, h=0.8, fs=10)
    N(ax, 17.0, ms9, 'callsnd\nmergeSort right', w=2.5, h=0.8, fs=10)
    A(ax, 10.5, bot(ms8), 10.5, top(ms9, 0.8))
    A(ax, 17.0, bot(ms8), 17.0, top(ms9, 0.8))

    ms10 = ms9 - 1.8
    N(ax, 10.5, ms10, 'retsnd\nleftSorted', w=1.9, h=0.8, fs=10)
    N(ax, 17.0, ms10, 'retsnd\nrightSorted', w=2.0, h=0.8, fs=10)
    A(ax, 10.5, bot(ms9, 0.8), 10.5, top(ms10, 0.8))
    A(ax, 17.0, bot(ms9, 0.8), 17.0, top(ms10, 0.8))

    ms11 = ms10 - 1.8
    N(ax, 14.0, ms11, 'callsnd merge\n(leftSorted, rightSorted)', w=3.5, h=0.8, fs=10)
    A(ax, 10.5, bot(ms10, 0.8), 14.0 - 0.8, top(ms11, 0.8))
    A(ax, 17.0, bot(ms10, 0.8), 14.0 + 0.8, top(ms11, 0.8))

    ms12 = ms11 - 1.8
    N(ax, 14.0, ms12, 'retsnd result', w=2.0, fs=10)
    A(ax, 14.0, bot(ms11, 0.8), 14.0, top(ms12))

    ms13 = ms12 - 1.8
    N(ax, CX, ms13, 'ret mergeSort', w=2.0, fs=11)
    A(ax, 14.0, bot(ms12), CX + 2.0/2, top(ms13))
    A(ax, -1.5, bot(ms2 - 1.8), CX - 2.0/2, top(ms13), color='#006600', lw=0.7)
    A(ax, 6.0 - 1.9/2, bot(ms5), CX - 0.5, top(ms13), color='#006600', lw=0.7)

    # recursive back-edge (left margin)
    ax.annotate("", xy=(-4.5, ms0), xytext=(-4.5, ms9),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.5,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=0.25'),
                zorder=3)
    T(ax, -6.0, (ms0 + ms9) / 2, 'recursive\ncallsnd', fontsize=10, color='#0066CC')

    # return to caller (right margin)
    ax.annotate("", xy=(CX + 3.0, y3), xytext=(20.5, ms13),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.0,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.1'),
                zorder=3)
    T(ax, 21.5, (y3 + ms13) / 2, 'return\nto caller', fontsize=9, color='#0066CC')

    # ══════════════════════════════════════════════
    # split (recursive) — LEFT side, well below mergeSort
    # ══════════════════════════════════════════════
    sp_top = 41.5
    sp_bot = 23.0
    func_box(ax, -6.0, sp_bot, 14.0, sp_top - sp_bot, 'split   (recursive template)')

    sp0 = 40.5
    N(ax, 2.0, sp0, 'lst  (input)', w=1.8, fs=11)
    A(ax, 14.0, bot(ms6), 2.0 + 1.0, top(sp0) + 0.3, color='#0066CC', ls='--')

    sp1 = sp0 - 1.8
    N(ax, 2.0, sp1, 'isNil lst', w=1.7, fs=11)
    A(ax, 2.0, bot(sp0), 2.0, top(sp1))

    sp2 = sp1 - 1.8
    N(ax, 2.0, sp2, 'steer', w=1.3, fs=11)
    A(ax, 2.0, bot(sp1), 2.0, top(sp2))

    N(ax, -2.5, sp2 - 1.8, 'return\n([], [])', w=1.7, h=0.8, fs=10)
    A(ax, 2.0 - 1.3/2, bot(sp2) + 0.1, -2.5 + 1.7/2, top(sp2 - 1.8, 0.8), color='#006600')
    T(ax, -0.5, sp2 - 0.3, '[]', fontsize=11, color='#006600', fontweight='bold')
    T(ax, 4.5, sp2 - 0.3, '(x:xt)', fontsize=11, color='#CC0000', fontweight='bold')

    sp3 = sp2 - 2.0
    N(ax, 1.0, sp3, 'head x', w=1.4, fs=11)
    N(ax, 4.0, sp3, 'tail', w=1.2, fs=11)
    A(ax, 2.0, bot(sp2), 1.0, top(sp3), color='#CC0000')
    A(ax, 2.0 + 0.5, bot(sp2), 4.0, top(sp3), color='#CC0000')

    sp4 = sp3 - 1.8
    N(ax, 2.5, sp4, 'head y', w=1.4, fs=11)
    N(ax, 5.5, sp4, 'tail zs', w=1.4, fs=11)
    A(ax, 4.0, bot(sp3), 2.5, top(sp4))
    A(ax, 4.0, bot(sp3), 5.5, top(sp4))

    sp4b = sp4 - 1.8
    N(ax, 5.5, sp4b, 'isNil zs\n+ steer', w=1.7, h=0.8, fs=10)
    A(ax, 5.5, bot(sp4), 5.5, top(sp4b, 0.8))

    N(ax, 1.5, sp4b, 'return\n([x], [])', w=1.7, h=0.8, fs=10)
    A(ax, 5.5 - 1.7/2, sp4b, 1.5 + 1.7/2, sp4b, color='#006600')
    T(ax, 3.5, sp4b + 0.8, '[x]', fontsize=10, color='#006600', fontweight='bold')
    T(ax, 7.0, sp4b + 0.8, 'x:y:zs', fontsize=10, color='#CC0000', fontweight='bold')

    sp5 = sp4b - 2.0
    N(ax, 5.5, sp5, 'callsnd split zs', w=2.2, fs=11)
    A(ax, 5.5 + 1.7/2, bot(sp4b, 0.8) + 0.2, 5.5, top(sp5), color='#CC0000')

    sp6 = sp5 - 1.8
    N(ax, 5.5, sp6, 'retsnd (xs, ys)', w=2.2, fs=10)
    A(ax, 5.5, bot(sp5), 5.5, top(sp6))

    sp7 = sp6 - 1.8
    N(ax, 3.0, sp7, 'cons\n(x:xs, y:ys)', w=2.0, h=0.8, fs=10)
    A(ax, 5.5, bot(sp6), 3.0 + 0.6, top(sp7, 0.8))
    A(ax, 1.0, bot(sp3), 3.0 - 0.6, top(sp7, 0.8), color='#AAAAAA', lw=0.6)
    A(ax, 2.5, bot(sp4), 3.0, top(sp7, 0.8), color='#AAAAAA', lw=0.6)

    sp8 = sp7 - 1.8
    N(ax, 2.0, sp8, 'ret split', w=1.6, fs=11)
    A(ax, 3.0, bot(sp7, 0.8), 2.0, top(sp8))
    A(ax, -2.5, bot(sp2 - 1.8, 0.8), 2.0 - 0.5, top(sp8), color='#006600', lw=0.6)
    A(ax, 1.5, bot(sp4b, 0.8), 2.0 + 0.3, top(sp8), color='#006600', lw=0.6)

    ax.annotate("", xy=(-5.5, sp0), xytext=(-5.5, sp5),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.2,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=0.25'),
                zorder=3)
    T(ax, -6.5, (sp0 + sp5) / 2, 'rec', fontsize=9, color='#0066CC')

    # ══════════════════════════════════════════════
    # merge (recursive, 2 inputs) — RIGHT side, well below mergeSort
    # ══════════════════════════════════════════════
    mg_top = 41.5
    mg_bot = 20.0
    func_box(ax, 9.5, mg_bot, 13.5, mg_top - mg_bot,
             'merge   (recursive, 2 inputs)')

    mg0 = 40.5
    N(ax, 13.0, mg0, 'xs  (input)', w=1.7, fs=11)
    N(ax, 19.0, mg0, 'ys  (input)', w=1.7, fs=11)
    A(ax, 14.0, bot(ms11, 0.8), 13.0, top(mg0) + 0.3, color='#0066CC', ls='--')
    A(ax, 14.0, bot(ms11, 0.8), 19.0, top(mg0) + 0.3, color='#0066CC', ls='--')

    mg1 = mg0 - 1.8
    N(ax, 13.0, mg1, 'isNil xs', w=1.7, fs=11)
    A(ax, 13.0, bot(mg0), 13.0, top(mg1))

    mg2 = mg1 - 1.8
    N(ax, 13.0, mg2, 'steer', w=1.3, fs=11)
    A(ax, 13.0, bot(mg1), 13.0, top(mg2))

    N(ax, 10.5, mg2 - 1.8, 'return ys', w=1.6, fs=11)
    A(ax, 13.0 - 1.3/2, bot(mg2) + 0.1, 10.5 + 0.5, top(mg2 - 1.8), color='#006600')
    T(ax, 11.3, mg2 - 0.3, '[]', fontsize=11, color='#006600', fontweight='bold')
    T(ax, 15.0, mg2 - 0.3, '(x:xt)', fontsize=11, color='#CC0000', fontweight='bold')

    mg3 = mg2 - 2.0
    N(ax, 12.0, mg3, 'head x', w=1.4, fs=11)
    N(ax, 15.0, mg3, 'tail xt', w=1.4, fs=11)
    A(ax, 13.0, bot(mg2), 12.0, top(mg3), color='#CC0000')
    A(ax, 13.0 + 0.3, bot(mg2), 15.0, top(mg3), color='#CC0000')

    mg4 = mg3 - 1.8
    N(ax, 19.0, mg4, 'isNil ys', w=1.7, fs=11)
    A(ax, 19.0, bot(mg0), 19.0, top(mg4))

    mg5 = mg4 - 1.8
    N(ax, 19.0, mg5, 'steer', w=1.3, fs=11)
    A(ax, 19.0, bot(mg4), 19.0, top(mg5))

    N(ax, 21.5, mg5, 'return xs', w=1.6, fs=11)
    A(ax, 19.0 + 1.3/2, bot(mg5) + 0.1, 21.5 - 0.5, top(mg5), color='#006600')
    T(ax, 20.8, mg5 + 0.65, '[]', fontsize=11, color='#006600', fontweight='bold')
    T(ax, 17.5, mg5 + 0.65, '(y:yt)', fontsize=11, color='#CC0000', fontweight='bold')

    mg6 = mg5 - 2.0
    N(ax, 17.0, mg6, 'head y', w=1.4, fs=11)
    N(ax, 20.0, mg6, 'tail yt', w=1.4, fs=11)
    A(ax, 19.0, bot(mg5), 17.0, top(mg6), color='#CC0000')
    A(ax, 19.0, bot(mg5), 20.0, top(mg6), color='#CC0000')

    mg7 = mg6 - 1.8
    N(ax, 15.0, mg7, r'lthan ($x \leq y$)', w=2.2, fs=11)
    A(ax, 12.0, bot(mg3), 15.0 - 0.5, top(mg7))
    A(ax, 17.0, bot(mg6), 15.0 + 0.5, top(mg7))

    mg8 = mg7 - 1.8
    N(ax, 15.0, mg8, 'steer', w=1.3, fs=11)
    A(ax, 15.0, bot(mg7), 15.0, top(mg8))

    T(ax, 12.0, mg8 - 0.3, 'true', fontsize=11, color='#006600', fontweight='bold')
    mg9t = mg8 - 2.0
    N(ax, 12.0, mg9t, 'callsnd\nmerge (xt, ys)', w=2.2, h=0.8, fs=10)
    A(ax, 15.0 - 1.3/2, bot(mg8) + 0.1, 12.0 + 0.5, top(mg9t, 0.8), color='#006600')
    mg10t = mg9t - 1.8
    N(ax, 12.0, mg10t, 'cons x : result', w=2.2, fs=10)
    A(ax, 12.0, bot(mg9t, 0.8), 12.0, top(mg10t))

    T(ax, 18.0, mg8 - 0.3, 'false', fontsize=11, color='#CC0000', fontweight='bold')
    mg9f = mg8 - 2.0
    N(ax, 18.0, mg9f, 'callsnd\nmerge (xs, yt)', w=2.2, h=0.8, fs=10)
    A(ax, 15.0 + 1.3/2, bot(mg8) + 0.1, 18.0 - 0.5, top(mg9f, 0.8), color='#CC0000')
    mg10f = mg9f - 1.8
    N(ax, 18.0, mg10f, 'cons y : result', w=2.2, fs=10)
    A(ax, 18.0, bot(mg9f, 0.8), 18.0, top(mg10f))

    mg11 = mg10t - 1.8
    N(ax, 15.0, mg11, 'ret merge', w=1.7, fs=11)
    A(ax, 12.0, bot(mg10t), 15.0 - 0.4, top(mg11))
    A(ax, 18.0, bot(mg10f), 15.0 + 0.4, top(mg11))
    A(ax, 10.5, bot(mg2 - 1.8), 15.0 - 1.7/2, top(mg11), color='#006600', lw=0.6)
    A(ax, 21.5, bot(mg5), 15.0 + 1.7/2, top(mg11), color='#006600', lw=0.6)

    ax.annotate("", xy=(22.5, mg0), xytext=(22.5, mg9t),
                arrowprops=dict(arrowstyle='->', color='#0066CC', lw=1.2,
                                linestyle='dashed',
                                connectionstyle='arc3,rad=-0.25'),
                zorder=3)
    T(ax, 23.0, (mg0 + mg9t) / 2, 'recursive\ncallsnd', fontsize=9, color='#0066CC')

    # ══════════════════════════════════════════════
    # PRINT_FINAL
    # ══════════════════════════════════════════════
    pf_y = 16.0
    func_box(ax, 4.5, pf_y - 3.8, 7.0, 3.8, 'print_final')
    N(ax, CX, pf_y - 0.8, 'xs  (input)', w=1.6, fs=11)
    N(ax, CX, pf_y - 2.5, 'super #4\n(GHC print)', w=2.2, h=0.8, fs=11, fill=GRAY)
    A(ax, CX, bot(y4), CX, top(pf_y - 0.8) + 0.5, color='#0066CC', ls='--')
    A(ax, CX, bot(pf_y - 0.8), CX, top(pf_y - 2.5, 0.8))

    draw_io(ax, CX, pf_y - 4.8, 'OUT')
    A(ax, CX, bot(pf_y - 2.5, 0.8), CX, pf_y - 4.8 + IO_R)

    T(ax, CX, pf_y - 6.5,
      '604 TALM instructions  (4 functions, 3 recursive)',
      fontsize=10, color='#888888',
      bbox=dict(boxstyle='round,pad=0.35', facecolor='#F5F5F5',
                edgecolor='#DDDDDD', linewidth=0.5, alpha=1.0))

    legend(ax, CX, pf_y - 8.0)
    save(fig, "fig_mergesort_dfg")


if __name__ == "__main__":
    print("Generating detailed dataflow graph figures...")
    fig_fibonacci()
    fig_vectorsum()
    fig_matmul()
    fig_helloworld()
    fig_mergesort()
    print(f"Done. Output in {OUTDIR}")
