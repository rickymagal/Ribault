#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, os, csv
from collections import defaultdict
import matplotlib.pyplot as plt

# ---------- IO ----------

def load_metrics(path):
    # expects header: N,P,rep,seconds,rc
    rows = []
    with open(path, newline="") as f:
        rdr = csv.DictReader(f)
        for r in rdr:
            try:
                if int(r["rc"]) != 0:
                    continue
                rows.append({
                    "N": int(r["N"]),
                    "P": int(r["P"]),
                    "rep": int(r["rep"]),
                    "seconds": float(r["seconds"]),
                })
            except Exception:
                # ignora linhas quebradas
                pass
    return rows

def write_aggregates_csv(path, rows):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["N","P","mean_seconds","Pref","T_ref","speedup_rel","efficiency_rel"])
        for r in rows:
            w.writerow([r["N"], r["P"], f"{r['t_mean']:.6f}",
                        r["P_ref"], f"{r['t_ref']:.6f}",
                        f"{r['speedup']:.6f}", f"{r['eff']:.6f}"])

# ---------- agregação ----------

def aggregate_mean_time(rows):
    # média de tempo por (N,P)
    acc = defaultdict(list)
    for r in rows:
        acc[(r["N"], r["P"])].append(r["seconds"])
    mean = {k: sum(v)/len(v) for k,v in acc.items()}
    return mean

def derive_speedup_eff(mean_by_NP):
    # Para cada N, define P_ref = menor P medido (se houver P=1, melhor).
    byN = defaultdict(list)
    for (N,P), t in mean_by_NP.items():
        byN[N].append((P, t))
    out = []
    for N, pts in byN.items():
        pts.sort(key=lambda x: x[0])
        P_ref, T_ref = pts[0]
        for P, T in pts:
            speed = T_ref / T if T > 0 else float("nan")
            eff = speed / (P / P_ref) if P > 0 else float("nan")
            out.append({
                "N": N, "P": P,
                "t_mean": T,
                "P_ref": P_ref, "t_ref": T_ref,
                "speedup": speed, "eff": eff
            })
    return out

# ---------- plots ----------
def plot_runtime_vs_N(derived, outdir, tag):
    # Curvas de P (séries) no eixo X=N
    series = defaultdict(list)  # P -> [(N, t)]
    for r in derived:
        series[r["P"]].append((r["N"], r["t_mean"]))
    for P in series:
        series[P].sort(key=lambda x: x[0])

    plt.figure()
    for P, pts in sorted(series.items()):
        Ns = [n for n,_ in pts]
        Ts = [t for _,t in pts]
        plt.plot(Ns, Ts, marker="o", label=f"P={P}")
    plt.title("Matrix Multiplication — Runtime vs Problem Size")
    plt.xlabel("Problem size N (matrix N×N)")
    plt.ylabel("Runtime (seconds)")
    plt.grid(True, which="both", linestyle="-", alpha=0.35)
    plt.legend(title="Processor count", loc="best")
    os.makedirs(outdir, exist_ok=True)
    png = os.path.join(outdir, f"runtime_vs_N_{tag}.png")
    pdf = os.path.join(outdir, f"runtime_vs_N_{tag}.pdf")
    plt.tight_layout(); plt.savefig(png, dpi=160); plt.savefig(pdf)
    print(f"[plot] wrote {png} and {pdf}")

def plot_speedup_vs_P(derived, outdir, tag):
    # Curvas de N (séries) no eixo X=P
    series = defaultdict(list)  # N -> [(P, speed)]
    pref_for = {}
    for r in derived:
        series[r["N"]].append((r["P"], r["speedup"]))
        pref_for[r["N"]] = r["P_ref"]
    for N in series:
        series[N].sort(key=lambda x: x[0])

    plt.figure()
    for N, pts in sorted(series.items()):
        Ps = [p for p,_ in pts]
        Sp = [s for _,s in pts]
        plt.plot(Ps, Sp, marker="o", label=f"N={N} (P\u2091\u2090\u2097={pref_for[N]})")
    plt.title("Matrix Multiplication — Speedup vs Processor Count")
    plt.xlabel("Processor count (P)")
    plt.ylabel("Speedup (relative to minimal measured P)")
    plt.grid(True, which="both", linestyle="-", alpha=0.35)
    plt.legend(title="Problem size", loc="best")
    os.makedirs(outdir, exist_ok=True)
    png = os.path.join(outdir, f"speedup_vs_P_{tag}.png")
    pdf = os.path.join(outdir, f"speedup_vs_P_{tag}.pdf")
    plt.tight_layout(); plt.savefig(png, dpi=160); plt.savefig(pdf)
    print(f"[plot] wrote {png} and {pdf}")

def plot_efficiency_vs_P(derived, outdir, tag):
    # Curvas de N (séries) no eixo X=P
    series = defaultdict(list)  # N -> [(P, eff)]
    pref_for = {}
    for r in derived:
        series[r["N"]].append((r["P"], r["eff"]))
        pref_for[r["N"]] = r["P_ref"]
    for N in series:
        series[N].sort(key=lambda x: x[0])

    plt.figure()
    for N, pts in sorted(series.items()):
        Ps = [p for p,_ in pts]
        Ef = [e for _,e in pts]
        plt.plot(Ps, Ef, marker="o", label=f"N={N} (P\u2091\u2090\u2097={pref_for[N]})")
    plt.title("Matrix Multiplication — Efficiency vs Processor Count")
    plt.xlabel("Processor count (P)")
    plt.ylabel("Parallel efficiency (relative to minimal measured P)")
    plt.ylim(0.0, 1.05)
    plt.grid(True, which="both", linestyle="-", alpha=0.35)
    plt.legend(title="Problem size", loc="best")
    os.makedirs(outdir, exist_ok=True)
    png = os.path.join(outdir, f"efficiency_vs_P_{tag}.png")
    pdf = os.path.join(outdir, f"efficiency_vs_P_{tag}.pdf")
    plt.tight_layout(); plt.savefig(png, dpi=160); plt.savefig(pdf)
    print(f"[plot] wrote {png} and {pdf}")

# ---------- main ----------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--tag", required=True)
    args = ap.parse_args()

    rows = load_metrics(args.metrics)
    mean = aggregate_mean_time(rows)
    derived = derive_speedup_eff(mean)

    # CSV derivado com T_mean, speedup e efficiency
    write_aggregates_csv(os.path.join(args.outdir, f"aggregates_{args.tag}.csv"), derived)

    # Plots
    plot_runtime_vs_N(derived, args.outdir, args.tag)
    plot_speedup_vs_P(derived, args.outdir, args.tag)
    plot_efficiency_vs_P(derived, args.outdir, args.tag)

if __name__ == "__main__":
    main()
