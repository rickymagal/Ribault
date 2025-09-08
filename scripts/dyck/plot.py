#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, csv, os, math
from collections import defaultdict
import statistics as stats
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# Lê CSV e retorna: data[(imb,delta)][P][N] = [secs...]
def read_metrics(path):
    data = defaultdict(lambda: defaultdict(lambda: defaultdict(list)))
    with open(path, newline="") as f:
        cr = csv.DictReader(f)
        for row in cr:
            if row.get("variant","") != "super":
                continue
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
            data[(imb,delta)][P][N].append(sec)
    # ordena N dentro de cada P
    for key in data:
        for P in data[key]:
            # nada a fazer agora; a ordenação será feita ao extrair
            pass
    return data

def series_mean_std(series_dict):
    # series_dict: N -> [secs...]
    Ns = sorted(series_dict.keys())
    means, stds = [], []
    for N in Ns:
        xs = series_dict[N]
        if len(xs) == 1:
            means.append(xs[0])
            stds.append(0.0)
        else:
            means.append(stats.mean(xs))
            # desvio-padrão amostral (ddof=1) se tiver >=2; senão 0
            stds.append(stats.pstdev(xs) if len(xs) < 2 else stats.stdev(xs))
    return Ns, means, stds

def plot_runtime(groups, outdir, tag):
    # Um gráfico por (imb,delta): N vs mean time, com barras de erro (std), curvas por P
    for (imb,delta), byP in sorted(groups.items()):
        plt.figure()
        for P in sorted(byP.keys()):
            Ns, mu, sd = series_mean_std(byP[P])
            if not Ns: 
                continue
            plt.errorbar(Ns, mu, yerr=sd, marker="o", capsize=3, label=f"P={P}")
        plt.title(f"Dyck Path – Runtime vs N (imb={imb}, delta={delta})", fontsize=12)
        plt.xlabel("Input size N", fontsize=11)
        plt.ylabel("Runtime (seconds) – mean ± std", fontsize=11)
        plt.grid(True, linestyle=":", linewidth=0.8)
        plt.legend(title="Threads", fontsize=9)
        plt.tight_layout()
        base = f"runtime_{tag}_imb{imb}_delta{delta}"
        plt.savefig(os.path.join(outdir, base + ".png"), dpi=180)
        plt.savefig(os.path.join(outdir, base + ".pdf"))
        plt.close()
        print(f"[plot] saved {base}.png/.pdf")

def plot_speedup(groups, outdir, tag):
    # Speedup vs N, baseline = menor P disponível
    for (imb,delta), byP in sorted(groups.items()):
        Ps = sorted(p for p in byP if byP[p])
        if not Ps: 
            continue
        baseP = Ps[0]
        Ns_b, mu_b, _ = series_mean_std(byP[baseP])
        base_map = dict(zip(Ns_b, mu_b))
        plt.figure()
        for P in Ps:
            Ns, mu, _ = series_mean_std(byP[P])
            Ns_sp, sp = [], []
            for N, tP in zip(Ns, mu):
                if N in base_map and tP > 0.0:
                    Ns_sp.append(N)
                    sp.append(base_map[N] / tP)
            if Ns_sp:
                plt.plot(Ns_sp, sp, marker="o", label=f"P={P}")
        plt.title(f"Dyck Path – Parallel Speedup (imb={imb}, delta={delta})", fontsize=12)
        plt.xlabel("Input size N", fontsize=11)
        plt.ylabel(f"Speedup vs P={baseP}", fontsize=11)
        plt.grid(True, linestyle=":", linewidth=0.8)
        plt.legend(title="Threads", fontsize=9)
        plt.tight_layout()
        base = f"speedup_{tag}_imb{imb}_delta{delta}"
        plt.savefig(os.path.join(outdir, base + ".png"), dpi=180)
        plt.savefig(os.path.join(outdir, base + ".pdf"))
        plt.close()
        print(f"[plot] saved {base}.png/.pdf")

def plot_efficiency(groups, outdir, tag):
    # Efficiency = speedup / P
    for (imb,delta), byP in sorted(groups.items()):
        Ps = sorted(p for p in byP if byP[p])
        if not Ps:
            continue
        baseP = Ps[0]
        Ns_b, mu_b, _ = series_mean_std(byP[baseP])
        base_map = dict(zip(Ns_b, mu_b))
        plt.figure()
        for P in Ps:
            Ns, mu, _ = series_mean_std(byP[P])
            Ns_eff, eff = [], []
            for N, tP in zip(Ns, mu):
                if N in base_map and tP > 0.0:
                    Ns_eff.append(N)
                    eff.append((base_map[N] / tP) / P)
            if Ns_eff:
                plt.plot(Ns_eff, eff, marker="o", label=f"P={P}")
        plt.title(f"Dyck Path – Parallel Efficiency (imb={imb}, delta={delta})", fontsize=12)
        plt.xlabel("Input size N", fontsize=11)
        plt.ylabel("Efficiency", fontsize=11)
        plt.grid(True, linestyle=":", linewidth=0.8)
        plt.legend(title="Threads", fontsize=9)
        plt.tight_layout()
        base = f"efficiency_{tag}_imb{imb}_delta{delta}"
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
    groups = read_metrics(args.metrics)
    if not groups:
        print("[plot] no data to plot")
        return
    plot_runtime(groups, args.outdir, args.tag)
    plot_speedup(groups, args.outdir, args.tag)
    plot_efficiency(groups, args.outdir, args.tag)

if __name__ == "__main__":
    main()
