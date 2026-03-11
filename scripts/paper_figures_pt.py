#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Gera todas as figuras do paper em português (mesmos nomes de arquivo)."""

import os, sys, argparse
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

# Reuse everything from the English version
sys.path.insert(0, os.path.dirname(__file__))
from paper_figures import (
    load_all, aggregate, best_config, per_P_data,
    plot_comp_errorbar, plot_fixed_P_errorbar, fmt_N_ticks_k,
    COMP_COLORS, COMP_MARKERS, COMP_LABELS,
    PER_P_STYLE, ALL_PS, _plot_per_P, _comp_label,
)

# Apply same rcParams
matplotlib.rcParams.update({
    'font.family': 'serif',
    'font.serif': ['Times New Roman', 'DejaVu Serif'],
    'font.size': 11,
    'axes.labelsize': 12,
    'legend.fontsize': 10,
    'xtick.labelsize': 10,
    'ytick.labelsize': 10,
    'figure.figsize': (6, 4),
    'figure.dpi': 300,
    'savefig.bbox': 'tight',
    'savefig.pad_inches': 0.05,
    'axes.grid': True,
    'grid.alpha': 0.3,
    'grid.linestyle': '--',
    'lines.linewidth': 1.8,
    'lines.markersize': 6,
})


def _save(outdir, name):
    os.makedirs(outdir, exist_ok=True)
    plt.savefig(os.path.join(outdir, name + '.pdf'))
    plt.savefig(os.path.join(outdir, name + '.png'), dpi=300)
    plt.close()
    print(f"  [ok] {name}.pdf/.png")


# ── Traduções dos rótulos de legenda ──
PT_LABELS = {
    'super':   'Ribault',
    'ghc':     'GHC Strategies',
    'parpseq': 'GHC par/pseq',
}

YLABEL_RUNTIME = "Tempo de execução (s)"


def _comp_label_pt(variant, best_ps):
    unique_ps = set(best_ps)
    if len(unique_ps) == 1:
        return f"{PT_LABELS[variant]}, $P$={best_ps[0]}"
    return f"{PT_LABELS[variant]} (melhor $P$)"


def plot_comp_errorbar_pt(ax, all_data, X_values, X_key='N',
                          variants=('super', 'ghc', 'parpseq'), **filters):
    results = {}
    for v in variants:
        bc = best_config(all_data, v, X_values, X_key=X_key, **filters)
        if not bc:
            continue
        results[v] = bc
        xs = sorted(bc.keys())
        meds = [bc[x][0] for x in xs]
        stds = [bc[x][1] for x in xs]
        best_ps = [bc[x][2] for x in xs]
        lbl = _comp_label_pt(v, best_ps)
        ax.errorbar(xs, meds, yerr=stds,
                    color=COMP_COLORS[v], marker=COMP_MARKERS[v],
                    label=lbl, capsize=2, elinewidth=0.8, alpha=0.5)
        ax.plot(xs, meds, color=COMP_COLORS[v], marker=COMP_MARKERS[v])
    return results


def plot_fixed_P_errorbar_pt(ax, all_data, X_values, fixed_P, X_key='N',
                              variants=('super', 'ghc', 'parpseq'), **filters):
    for v in variants:
        xs, meds, stds = [], [], []
        for x in X_values:
            times = [r['seconds'] for r in all_data
                     if r['variant'] == v and r[X_key] == x and r['P'] == fixed_P
                     and all(r.get(k) == val for k, val in filters.items())]
            if not times:
                continue
            med, sd = aggregate(times)
            xs.append(x)
            meds.append(med)
            stds.append(sd)
        if xs:
            lbl = f"{PT_LABELS[v]}, $P$={fixed_P}"
            ax.errorbar(xs, meds, yerr=stds,
                        color=COMP_COLORS[v], marker=COMP_MARKERS[v],
                        label=lbl, capsize=2, elinewidth=0.8, alpha=0.5)
            ax.plot(xs, meds, color=COMP_COLORS[v], marker=COMP_MARKERS[v])


# ── FIGURAS ──

def fig1(data, outdir):
    print("Fig 1: MatMul melhor tempo")
    mm = data['matmul']
    Ns = sorted({r['N'] for r in mm})
    fig, ax = plt.subplots()
    plot_comp_errorbar_pt(ax, mm, Ns)
    ax.set_title("Multiplicação de Matrizes: Melhor Tempo por Sistema", fontsize=13)
    ax.set_xlabel("Dimensão da matriz $N$")
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.set_ylim(bottom=0)
    ax.set_xticks([n for n in [100, 200, 400, 600, 800, 1000, 1200, 1500] if n <= max(Ns)])
    ax.legend()
    _save(outdir, "fig1_matmul_best_runtime")


def fig2(data, outdir):
    print("Fig 2: MatMul melhor speedup")
    mm = data['matmul']
    Ns = sorted({r['N'] for r in mm})
    fig, ax = plt.subplots()
    for v in ('super', 'ghc', 'parpseq'):
        bc = best_config(mm, v, Ns)
        p1 = {}
        for N in Ns:
            times = [r['seconds'] for r in mm
                     if r['variant'] == v and r['N'] == N and r['P'] == 1]
            if times:
                p1[N] = aggregate(times)[0]
        ns, sus, best_ps = [], [], []
        for N in Ns:
            if N in bc and N in p1 and bc[N][0] > 0:
                ns.append(N)
                sus.append(p1[N] / bc[N][0])
                best_ps.append(bc[N][2])
        if ns:
            lbl = _comp_label_pt(v, best_ps)
            ax.errorbar(ns, sus, color=COMP_COLORS[v], marker=COMP_MARKERS[v],
                        label=lbl, capsize=2, elinewidth=0.8, alpha=0.5)
            ax.plot(ns, sus, color=COMP_COLORS[v], marker=COMP_MARKERS[v])
    max_P = max(p for v in ('super', 'ghc', 'parpseq')
                for N in Ns if N in (best_config(mm, v, Ns) or {})
                for p in [best_config(mm, v, Ns).get(N, (0, 0, 1))[2]])
    ax.axhline(y=max_P, color='gray', ls='--', lw=1, alpha=0.6,
               label=f'Ideal ($P{{=}}{max_P}$)')
    ax.set_title("Multiplicação de Matrizes: Melhor Speedup por Sistema", fontsize=13)
    ax.set_xlabel("Dimensão da matriz $N$")
    ax.set_ylabel("Speedup vs. $P{=}1$")
    ax.set_ylim(0, max_P + 6)
    ax.legend()
    _save(outdir, "fig2_matmul_best_speedup")


def fig3(data, outdir):
    print("Fig 3: MergeSort melhor tempo")
    ms = data['mergesort']
    Ns = sorted({r['N'] for r in ms})
    fig, ax = plt.subplots()
    plot_comp_errorbar_pt(ax, ms, Ns)
    ax.set_yscale('log')
    ax.set_title("Merge Sort: Melhor Tempo por Sistema", fontsize=13)
    ax.set_xlabel("Tamanho da lista $N$")
    ax.set_ylabel(YLABEL_RUNTIME)
    fmt_N_ticks_k(ax)
    ax.legend()
    _save(outdir, "fig3_ms_best_runtime")


def fig4(data, outdir):
    print("Fig 4: MergeSort razão de vantagem")
    ms = data['mergesort']
    Ns = sorted({r['N'] for r in ms})
    bc_rib = best_config(ms, 'super', Ns)
    bc_ghc = best_config(ms, 'ghc', Ns)
    bc_pp = best_config(ms, 'parpseq', Ns)
    fig, ax = plt.subplots()
    ns1, r1 = [], []
    for N in Ns:
        if N in bc_rib and N in bc_ghc and bc_rib[N][0] > 0:
            ns1.append(N)
            r1.append(bc_ghc[N][0] / bc_rib[N][0])
    if ns1:
        ax.plot(ns1, r1, color=COMP_COLORS['ghc'], marker=COMP_MARKERS['ghc'],
                label='vs. GHC Strategies')
    ns2, r2 = [], []
    for N in Ns:
        if N in bc_rib and N in bc_pp and bc_rib[N][0] > 0:
            ns2.append(N)
            r2.append(bc_pp[N][0] / bc_rib[N][0])
    if ns2:
        ax.plot(ns2, r2, color=COMP_COLORS['parpseq'], marker=COMP_MARKERS['parpseq'],
                label='vs. GHC par/pseq')
    ax.axhline(y=1, color='gray', ls='--', lw=1, alpha=0.6)
    ax.set_title("Merge Sort: Razão de Vantagem do Ribault", fontsize=13)
    ax.set_xlabel("Tamanho da lista $N$")
    ax.set_ylabel(r"Razão de speedup do Ribault ($\times$ mais rápido)")
    fmt_N_ticks_k(ax)
    ax.legend()
    _save(outdir, "fig4_ms_advantage")


def fig5(data, outdir):
    print("Fig 5: Dyck tempo vs desbalanceamento")
    dk = data['dyck']
    imbs = sorted({r['imb'] for r in dk
                   if r.get('delta', 0) == 0 and r['N'] == 1000000})
    fig, ax = plt.subplots()
    plot_fixed_P_errorbar_pt(ax, dk, imbs, fixed_P=8, X_key='imb', N=1000000, delta=0)
    ax.set_yscale('log')
    ax.set_title("Caminhos de Dyck: Tempo vs. Desbalanceamento ($P{=}8$)", fontsize=13)
    ax.set_xlabel("Desbalanceamento de carga (%)")
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.set_xticks([0, 20, 40, 60, 80, 100])
    ax.legend()
    _save(outdir, "fig5_dyck_imbalance")


def fig6(data, outdir):
    print("Fig 6: Dyck escalabilidade pior caso")
    dk = data['dyck']
    Ns = sorted({r['N'] for r in dk if r.get('imb') == 100 and r.get('delta', 0) == 0})
    fig, ax = plt.subplots()
    plot_fixed_P_errorbar_pt(ax, dk, Ns, fixed_P=8, imb=100, delta=0)
    ax.set_yscale('log')
    ax.set_title("Caminhos de Dyck: Escalabilidade no Pior Caso\n($P{=}8$, 100% Desbalanceamento)",
                 fontsize=13)
    ax.set_xlabel("Comprimento da sequência $N$")
    ax.set_ylabel(YLABEL_RUNTIME)
    fmt_N_ticks_k(ax)
    ax.legend()
    _save(outdir, "fig6_dyck_collapse_scaling")


def fig7(data, outdir):
    print("Fig 7: Barplot resumo")
    scenarios = [
        ("MatMul", data['matmul'], dict(N=1000), None),
        ("MergeSort", data['mergesort'], dict(N=5000000), None),
        ("Dyck (bal.)", data['dyck'], dict(N=1000000, imb=0, delta=0), None),
        ("Dyck (desbal.)", data['dyck'], dict(N=1000000, imb=100, delta=0), 8),
    ]
    nq = data.get('nqueens', [])
    if nq:
        nq_Ns = sorted({r['N'] for r in nq})
        nq_N = nq_Ns[-1] if nq_Ns else 15
        scenarios.append(("N-Rainhas", nq, dict(N=nq_N), None))

    variants = ('super', 'ghc', 'parpseq')
    n_groups = len(scenarios)
    x = np.arange(n_groups)
    width = 0.25

    fig, ax = plt.subplots(figsize=(8, 4.5))
    for i, v in enumerate(variants):
        vals = []
        for label, dset, filt, force_P in scenarios:
            if force_P is not None:
                times = [r['seconds'] for r in dset
                         if r['variant'] == v and r['P'] == force_P
                         and all(r.get(k) == val for k, val in filt.items())]
                if times:
                    med, _ = aggregate(times)
                    vals.append(med)
                else:
                    vals.append(0)
            else:
                best_med = float('inf')
                has_cutoff = any('cutoff' in r for r in dset)
                cutoffs = sorted({r.get('cutoff', 0) for r in dset}) if has_cutoff else [0]
                for P in ALL_PS:
                    for c in cutoffs:
                        extra = dict(filt)
                        if has_cutoff and c > 0:
                            extra['cutoff'] = c
                        times = [r['seconds'] for r in dset
                                 if r['variant'] == v and r['P'] == P
                                 and all(r.get(k) == val for k, val in extra.items())]
                        if not times:
                            continue
                        med, _ = aggregate(times)
                        if med < best_med:
                            best_med = med
                vals.append(best_med if best_med < float('inf') else 0)
        bars = ax.bar(x + i * width, vals, width,
                      color=COMP_COLORS[v], label=PT_LABELS[v])
        for bar, val in zip(bars, vals):
            if val > 0:
                txt = f'{val:.4f}' if val < 0.01 else (f'{val:.3f}' if val < 1 else f'{val:.2f}')
                ax.annotate(txt,
                            xy=(bar.get_x() + bar.get_width() / 2, bar.get_height()),
                            xytext=(0, 4), textcoords='offset points',
                            ha='center', fontsize=8)
    ax.set_yscale('log')
    ax.set_title("Comparação de Tempo de Execução entre Benchmarks", fontsize=13)
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.set_xticks(x + width)
    ax.set_xticklabels([s[0] for s in scenarios], fontsize=9)
    dyck_imb_idx = 3
    ax.annotate("$P{=}8$", xy=(dyck_imb_idx + width, 0), xytext=(0, -18),
                textcoords='offset points', ha='center', fontsize=8, color='gray')
    ax.legend(loc='lower center', fontsize=8, framealpha=0.9,
              ncol=3, bbox_to_anchor=(0.5, -0.15))
    fig.subplots_adjust(bottom=0.18)
    _save(outdir, "fig7_summary_barplot")


def fig8(data, outdir):
    print("Fig 8: MatMul Ribault por P")
    mm = data['matmul']
    Ns = sorted({r['N'] for r in mm if r['variant'] == 'super'})
    ppd = per_P_data(mm, 'super', Ns)
    fig, ax = plt.subplots()
    _plot_per_P(ax, ppd, Ns)
    ax.set_title(r"Multiplicação de Matrizes: Escalabilidade do Ribault por $P$", fontsize=13)
    ax.set_xlabel("Dimensão da matriz $N$")
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.set_ylim(bottom=0)
    ax.legend()
    _save(outdir, "fig8_matmul_ribault_perP")


def fig9(data, outdir):
    print("Fig 9: MergeSort Ribault por P")
    ms = data['mergesort']
    Ns = sorted({r['N'] for r in ms if r['variant'] == 'super'})
    ppd = per_P_data(ms, 'super', Ns)
    fig, ax = plt.subplots()
    _plot_per_P(ax, ppd, Ns)
    ax.set_title(r"Merge Sort: Escalabilidade do Ribault por $P$", fontsize=13)
    ax.set_xlabel("Tamanho da lista $N$")
    ax.set_ylabel(YLABEL_RUNTIME)
    fmt_N_ticks_k(ax)
    ax.set_ylim(bottom=0)
    ax.legend()
    _save(outdir, "fig9_ms_ribault_perP")


def fig10(data, outdir):
    print("Fig 10: Dyck Ribault por P")
    dk = data['dyck']
    imbs = sorted({r['imb'] for r in dk
                   if r['variant'] == 'super' and r['N'] == 1000000
                   and r.get('delta', 0) == 0})
    ppd = per_P_data(dk, 'super', imbs, X_key='imb', N=1000000, delta=0)
    fig, ax = plt.subplots()
    _plot_per_P(ax, ppd, imbs)
    ax.set_title(r"Caminhos de Dyck: Escalabilidade do Ribault por $P$", fontsize=13)
    ax.set_xlabel("Desbalanceamento de carga (%)")
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.set_xticks([0, 20, 40, 60, 80, 100])
    ax.legend()
    _save(outdir, "fig10_dyck_ribault_perP")


def fig11(data, outdir):
    print("Fig 11: Fibonacci Ribault por P")
    fb = data['fibonacci']
    if not fb:
        print("  [skip] sem dados de fibonacci")
        return
    cutoffs = sorted({r['cutoff'] for r in fb
                      if r['variant'] == 'super' and r['N'] == 35})
    if not cutoffs:
        print("  [skip] sem dados Ribault de fibonacci para N=35")
        return
    ppd = per_P_data(fb, 'super', cutoffs, X_key='cutoff', N=35)
    fig, ax = plt.subplots()
    _plot_per_P(ax, ppd, cutoffs)
    ax.set_yscale('log')
    ax.set_title(r"Fibonacci: Escalabilidade do Ribault por $P$", fontsize=13)
    ax.set_xlabel("Limiar de corte")
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.legend()
    _save(outdir, "fig11_fib_ribault_perP")


def fig12(data, outdir):
    print("Fig 12: Fibonacci melhor tempo vs corte")
    fb = data['fibonacci']
    if not fb:
        print("  [skip] sem dados de fibonacci")
        return
    cutoffs = sorted({r['cutoff'] for r in fb if r['N'] == 35})
    if not cutoffs:
        print("  [skip] sem dados de fibonacci para N=35")
        return
    fig, ax = plt.subplots()
    plot_comp_errorbar_pt(ax, fb, cutoffs, X_key='cutoff', N=35)
    ax.set_yscale('log')
    ax.set_title("Fibonacci: Melhor Tempo vs. Limiar de Corte", fontsize=13)
    ax.set_xlabel("Limiar de corte")
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.legend()
    _save(outdir, "fig12_fib_best_runtime")


def fig13(data, outdir):
    print("Fig 13: N-Rainhas melhor tempo")
    nq = data.get('nqueens', [])
    if not nq:
        print("  [skip] sem dados de N-Rainhas")
        return
    Ns = sorted({r['N'] for r in nq})
    cutoffs = sorted({r.get('cutoff', 1) for r in nq})
    fig, ax = plt.subplots()
    for v in ('super', 'ghc', 'parpseq'):
        xs, meds, stds, best_ps = [], [], [], []
        for N in Ns:
            best_med, best_std, best_P = float('inf'), 0, 1
            for P in ALL_PS:
                for c in cutoffs:
                    times = [r['seconds'] for r in nq
                             if r['variant'] == v and r['N'] == N
                             and r['P'] == P and r.get('cutoff', 1) == c]
                    if not times:
                        continue
                    med, sd = aggregate(times)
                    if med < best_med:
                        best_med, best_std, best_P = med, sd, P
            if best_med < float('inf'):
                xs.append(N)
                meds.append(best_med)
                stds.append(best_std)
                best_ps.append(best_P)
        if xs:
            lbl = _comp_label_pt(v, best_ps)
            ax.errorbar(xs, meds, yerr=stds,
                        color=COMP_COLORS[v], marker=COMP_MARKERS[v],
                        label=lbl, capsize=2, elinewidth=0.8, alpha=0.5)
            ax.plot(xs, meds, color=COMP_COLORS[v], marker=COMP_MARKERS[v])
    ax.set_yscale('log')
    ax.set_title("N-Rainhas: Melhor Tempo por Sistema", fontsize=13)
    ax.set_xlabel("Tamanho do tabuleiro $N$")
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.set_xticks(Ns)
    ax.legend()
    _save(outdir, "fig13_nqueens_best_runtime")


def fig14(data, outdir):
    print("Fig 14: N-Rainhas Ribault por P")
    nq = data.get('nqueens', [])
    if not nq:
        print("  [skip] sem dados de N-Rainhas")
        return
    Ns = sorted({r['N'] for r in nq if r['variant'] == 'super'})
    cutoffs = sorted({r.get('cutoff', 1) for r in nq if r['variant'] == 'super'})
    all_Ps = sorted({r['P'] for r in nq if r['variant'] == 'super'})
    fig, ax = plt.subplots()
    for P in all_Ps:
        xs, meds_list = [], []
        for N in Ns:
            best_med = float('inf')
            for c in cutoffs:
                times = [r['seconds'] for r in nq
                         if r['variant'] == 'super' and r['N'] == N
                         and r['P'] == P and r.get('cutoff', 1) == c]
                if not times:
                    continue
                med, _ = aggregate(times)
                if med < best_med:
                    best_med = med
            if best_med < float('inf'):
                xs.append(N)
                meds_list.append(best_med)
        if xs:
            style = PER_P_STYLE.get(P, dict(color='#999999', ls='-', alpha=0.8, lw=1.5))
            ax.plot(xs, meds_list, marker='o', label=f"$P{{=}}{P}$", **style)
    ax.set_yscale('log')
    ax.set_title(r"N-Rainhas: Escalabilidade do Ribault por $P$", fontsize=13)
    ax.set_xlabel("Tamanho do tabuleiro $N$")
    ax.set_ylabel(YLABEL_RUNTIME)
    ax.set_xticks(Ns)
    ax.legend()
    _save(outdir, "fig14_nqueens_ribault_perP")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--results", default="RESULTS",
                    help="Diretório raiz de resultados")
    ap.add_argument("--outdir", default="RESULTS/paper_figures/pt",
                    help="Diretório de saída para figuras em PT")
    args = ap.parse_args()

    print("Carregando dados...")
    data = load_all(args.results)
    for k, v in data.items():
        print(f"  {k}: {len(v)} linhas")

    outdir = args.outdir
    os.makedirs(outdir, exist_ok=True)

    print("\n=== PARTE A: Figuras comparativas ===")
    fig1(data, outdir)
    fig2(data, outdir)
    fig3(data, outdir)
    fig4(data, outdir)
    fig5(data, outdir)
    fig6(data, outdir)
    fig7(data, outdir)

    print("\n=== PARTE B: Figuras de escalabilidade por P ===")
    fig8(data, outdir)
    fig9(data, outdir)
    fig10(data, outdir)
    fig11(data, outdir)

    print("\n=== PARTE C: Fibonacci comparativo ===")
    fig12(data, outdir)

    print("\n=== PARTE D: Figuras de N-Rainhas ===")
    fig13(data, outdir)
    fig14(data, outdir)

    print(f"\n=== Todas as figuras salvas em {outdir} ===")
    for f in sorted(os.listdir(outdir)):
        sz = os.path.getsize(os.path.join(outdir, f))
        print(f"  {f:45s} {sz//1024:>4d} KB")


if __name__ == "__main__":
    main()
