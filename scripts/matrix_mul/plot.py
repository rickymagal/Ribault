#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os, csv, math
from collections import defaultdict
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# ---------- IO ----------

def load_metrics(path):
    # aceita cabeçalho com ou sem 'variant'
    rows = []
    with open(path, newline="") as f:
        rdr = csv.DictReader(f)
        for r in rdr:
            try:
                rc = int(r["rc"])
                if rc != 0:
                    continue
                rows.append({
                    "N": int(r["N"]),
                    "P": int(r["P"]),
                    "rep": int(r["rep"]),
                    "seconds": float(r["seconds"]),
                })
            except Exception:
                pass
    return rows

def write_aggregates_csv(path, agg_rows):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", newline="") as f:
      w = csv.writer(f)
      w.writerow(["N","P","mean_seconds","std_seconds","Pref","T_ref","speedup_rel","efficiency_rel"])
      for r in agg_rows:
        w.writerow([
          r["N"], r["P"],
          f"{r['t_mean']:.6f}", f"{r['t_std']:.6f}",
          r["P_ref"], f"{r['t_ref']:.6f}",
          f"{r['speedup']:.6f}", f"{r['eff']:.6f}"
        ])

# ---------- agregação ----------

def mean_std(xs):
    n = len(xs)
    if n == 0: return (float("nan"), float("nan"))
    m = sum(xs)/n
    if n == 1: return (m, 0.0)
    var = sum((x-m)*(x-m) for x in xs)/(n-1)
    return (m, math.sqrt(var))

def aggregate_by_NP(rows):
    acc = defaultdict(list)
    for r in rows:
        acc[(r["N"], r["P"])].append(r["seconds"])
    out = {}
    for (N,P), ts in acc.items():
        m, s = mean_std(ts)
        out[(N,P)] = (m, s)
    return out

def derive_speedup_eff(mean_by_NP):
    # Para cada N, P_ref = menor P medido
    byN = defaultdict(list)
    for (N,P), (tm,_ts) in mean_by_NP.items():
        byN[N].append((P, tm))
    agg_rows = []
    for N, pts in byN.items():
        pts.sort(key=lambda x: x[0])
        P_ref, T_ref = pts[0]
        for P, T in pts:
            speed = (T_ref / T) if T > 0 else float("nan")
            eff = speed / (P / P_ref) if P > 0 else float("nan")
            agg_rows.append({
              "N": N, "P": P,
              "t_mean": T, "t_std": mean_by_NP[(N,P)][1],
              "P_ref": P_ref, "t_ref": T_ref,
              "speedup": speed, "eff": eff
            })
    return agg_rows

# ---------- plots ----------

def plot_runtime_vs_N(derived, outdir, tag):
    # Séries = P ; X = N ; Y = T_mean ; barras = std
    series = defaultdict(list)  # P -> [(N, mean, std)]
    for r in derived:
        series[r["P"]].append((r["N"], r["t_mean"], r["t_std"]))
    for P in series:
        series[P].sort(key=lambda x: x[0])

    plt.figure()
    for P, pts in sorted(series.items()):
        Ns  = [n for n,_,_ in pts]
        Tm  = [t for _,t,_ in pts]
        Ts  = [s for _,_,s in pts]
        plt.errorbar(Ns, Tm, yerr=Ts, marker="o", capsize=3, label=f"P={P}")
    plt.title("Matrix Multiplication — Runtime vs Problem Size", fontsize=12)
    plt.xlabel("Problem size N (matrix N×N)", fontsize=11)
    plt.ylabel("Runtime (seconds)", fontsize=11)
    plt.grid(True, linestyle=":", linewidth=0.8)
    plt.legend(title="Processor count", fontsize=9)
    plt.tight_layout()
    os.makedirs(outdir, exist_ok=True)
    png = os.path.join(outdir, f"runtime_vs_N_{tag}.png")
    pdf = os.path.join(outdir, f"runtime_vs_N_{tag}.pdf")
    plt.savefig(png, dpi=180); plt.savefig(pdf)
    print(f"[plot] wrote {png} and {pdf}")

def plot_speedup_vs_P(derived, outdir, tag):
    # Séries = N ; X = P ; Y = speedup (sem barra por padrão)
    series = defaultdict(list); pref_for = {}
    for r in derived:
        series[r["N"]].append((r["P"], r["speedup"]))
        pref_for[r["N"]] = r["P_ref"]
    for N in series:
        series[N].sort(key=lambda x: x[0])

    plt.figure()
    for N, pts in sorted(series.items()):
        Ps = [p for p,_ in pts]
        Sp = [s for _,s in pts]
        plt.plot(Ps, Sp, marker="o", label=f"N={N} (Pref={pref_for[N]})")
    plt.title("Matrix Multiplication — Speedup vs Processor Count", fontsize=12)
    plt.xlabel("Processor count P", fontsize=11)
    plt.ylabel("Speedup (relative to minimal measured P)", fontsize=11)
    plt.grid(True, linestyle=":", linewidth=0.8)
    plt.legend(title="Problem size", fontsize=9)
    plt.tight_layout()
    os.makedirs(outdir, exist_ok=True)
    png = os.path.join(outdir, f"speedup_vs_P_{tag}.png")
    pdf = os.path.join(outdir, f"speedup_vs_P_{tag}.pdf")
    plt.savefig(png, dpi=180); plt.savefig(pdf)
    print(f"[plot] wrote {png} and {pdf}")

def plot_efficiency_vs_P(derived, outdir, tag):
    series = defaultdict(list); pref_for = {}
    for r in derived:
        series[r["N"]].append((r["P"], r["eff"]))
        pref_for[r["N"]] = r["P_ref"]
    for N in series:
        series[N].sort(key=lambda x: x[0])

    plt.figure()
    for N, pts in sorted(series.items()):
        Ps = [p for p,_ in pts]
        Ef = [e for _,e in pts]
        plt.plot(Ps, Ef, marker="o", label=f"N={N} (Pref={pref_for[N]})")
    plt.title("Matrix Multiplication — Efficiency vs Processor Count", fontsize=12)
    plt.xlabel("Processor count P", fontsize=11)
    plt.ylabel("Parallel efficiency", fontsize=11)
    plt.ylim(0.0, 1.05)
    plt.grid(True, linestyle=":", linewidth=0.8)
    plt.legend(title="Problem size", fontsize=9)
    plt.tight_layout()
    os.makedirs(outdir, exist_ok=True)
    png = os.path.join(outdir, f"efficiency_vs_P_{tag}.png")
    pdf = os.path.join(outdir, f"efficiency_vs_P_{tag}.pdf")
    plt.savefig(png, dpi=180); plt.savefig(pdf)
    print(f"[plot] wrote {png} and {pdf}")

# ---------- main ----------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--tag", required=True)
    args = ap.parse_args()

    rows = load_metrics(args.metrics)
    mean_std_by_NP = aggregate_by_NP(rows)
    derived = derive_speedup_eff(mean_std_by_NP)

    write_aggregates_csv(os.path.join(args.outdir, f"aggregates_{args.tag}.csv"), derived)
    plot_runtime_vs_N(derived, args.outdir, args.tag)
    plot_speedup_vs_P(derived, args.outdir, args.tag)
    plot_efficiency_vs_P(derived, args.outdir, args.tag)

if __name__ == "__main__":
    main()
