#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse, os, sys, pathlib, math, time, json, csv, subprocess
from collections import defaultdict

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent

def run(cmd, timeout=None, cwd=None, env=None):
    t0 = time.time()
    try:
        p = subprocess.run(cmd, cwd=cwd, env=env, check=False, timeout=timeout)
        rc = p.returncode
    except subprocess.TimeoutExpired:
        rc = 124  # timeout
    dt_ms = round((time.time()-t0)*1000.0, 2)
    return rc, dt_ms

def ensure_dir(p):
    pathlib.Path(p).mkdir(parents=True, exist_ok=True)

def write_csv_row(csv_path, row, header):
    new = not pathlib.Path(csv_path).exists()
    with open(csv_path, "a", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=header)
        if new:
            w.writeheader()
        w.writerow(row)

def agg_metrics(rows):
    # rows: list of dicts (rc, elapsed_ms, variant, P, N, rep)
    by = defaultdict(list)
    for r in rows:
        key = (r["variant"], r["P"], r["N"])
        by[key].append(r)
    metrics = []
    for (variant,P,N), lst in sorted(by.items()):
        ok = [x for x in lst if x["rc"] == 0]
        n = len(lst)
        m = len(ok)
        succ = m / n if n else 0.0
        t_median = None
        t_mean = None
        t_std = None
        if m:
            vals = sorted([x["elapsed_ms"] for x in ok])
            mid = len(vals)//2
            t_median = (vals[mid] if len(vals)%2==1 else (vals[mid-1]+vals[mid])/2)
            t_mean = sum(vals)/len(vals)
            # std
            mu = t_mean
            t_std = (sum((v-mu)**2 for v in vals)/len(vals))**0.5 if len(vals) else None
        metrics.append({
            "variant": variant,
            "P": P,
            "N": N,
            "n_runs": n,
            "n_ok": m,
            "success": succ,
            "median_ms": t_median if t_median is not None else "",
            "mean_ms": t_mean if t_mean is not None else "",
            "std_ms": t_std if t_std is not None else "",
            "timeouts": sum(1 for x in lst if x["rc"]==124),
            "fail_rc_nonzero": sum(1 for x in lst if x["rc"] not in (0,124)),
        })
    # speedup/efficiency vs P (por variante e N), base P=1
    by_varN = defaultdict(list)
    for m in metrics:
        if m["median_ms"] != "":
            by_varN[(m["variant"], m["N"])].append(m)
    for (variant,N), lst in by_varN.items():
        base = next((x for x in lst if x["P"]==1 and x["median_ms"]!=""), None)
        if not base: 
            continue
        base_t = base["median_ms"]
        for m in lst:
            if m["median_ms"] == "":
                m["speedup"] = ""
                m["efficiency"] = ""
            else:
                sp = base_t / m["median_ms"] if m["median_ms"]>0 else ""
                ef = sp / m["P"] if sp != "" else ""
                m["speedup"] = sp
                m["efficiency"] = ef
    return metrics

def save_metrics_csv(outroot, metrics):
    p = pathlib.Path(outroot) / "metrics.csv"
    header = ["variant","N","P","n_runs","n_ok","success","median_ms","mean_ms","std_ms","timeouts","fail_rc_nonzero","speedup","efficiency"]
    new = True
    with open(p, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=header)
        w.writeheader()
        for m in sorted(metrics, key=lambda x:(x["variant"], x["N"], x["P"])):
            w.writerow({
                "variant": m["variant"],
                "N": m["N"],
                "P": m["P"],
                "n_runs": m["n_runs"],
                "n_ok": m["n_ok"],
                "success": m["success"],
                "median_ms": m.get("median_ms",""),
                "mean_ms": m.get("mean_ms",""),
                "std_ms": m.get("std_ms",""),
                "timeouts": m.get("timeouts",""),
                "fail_rc_nonzero": m.get("fail_rc_nonzero",""),
                "speedup": m.get("speedup",""),
                "efficiency": m.get("efficiency",""),
            })
    return str(p)

def plot_all(outroot):
    # plots simples e robustos sem dependências estranhas
    import csv, math
    import matplotlib.pyplot as plt

    mpath = pathlib.Path(outroot) / "metrics.csv"
    rows = []
    with open(mpath, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            # cast
            row["N"] = int(row["N"])
            row["P"] = int(row["P"])
            row["success"] = float(row["success"])
            for k in ("median_ms","speedup","efficiency"):
                row[k] = float(row[k]) if row[k] else math.nan
            rows.append(row)

    # 1) Tempo (mediana) vs N por (variant,P)
    variants = sorted(set(r["variant"] for r in rows))
    Ps = sorted(set(r["P"] for r in rows))
    for v in variants:
        for P in Ps:
            data = sorted([r for r in rows if r["variant"]==v and r["P"]==P], key=lambda x:x["N"])
            if not data:
                continue
            N = [d["N"] for d in data if not math.isnan(d["median_ms"])]
            T = [d["median_ms"] for d in data if not math.isnan(d["median_ms"])]
            if not N:
                continue
            plt.figure()
            plt.plot(N, T, marker="o")
            plt.xlabel("N")
            plt.ylabel("Tempo mediano (ms)")
            plt.title(f"Tempo vs N — {v}, P={P}")
            plt.grid(True, alpha=0.3)
            outpng = pathlib.Path(outroot)/f"plot_time_vs_n__{v}__p{P}.png"
            plt.tight_layout()
            plt.savefig(outpng)
            plt.close()

    # 2) Speedup e eficiência vs P no MAIOR N concluído por variante
    for v in variants:
        vr = [r for r in rows if r["variant"]==v and not math.isnan(r["speedup"])]
        if not vr:
            continue
        maxN = max(r["N"] for r in vr)
        data = sorted([r for r in vr if r["N"]==maxN], key=lambda x:x["P"])
        if not data:
            continue
        P = [d["P"] for d in data]
        SP = [d["speedup"] for d in data]
        EF = [d["efficiency"] for d in data]

        plt.figure()
        plt.plot(P, SP, marker="o")
        plt.xlabel("P")
        plt.ylabel("Speedup (mediana)")
        plt.title(f"Speedup vs P — {v}, N={maxN}")
        plt.grid(True, alpha=0.3)
        outpng = pathlib.Path(outroot)/f"plot_speedup_vs_p__{v}__N{maxN}.png"
        plt.tight_layout()
        plt.savefig(outpng)
        plt.close()

        plt.figure()
        plt.plot(P, EF, marker="o")
        plt.xlabel("P")
        plt.ylabel("Eficiência (Speedup/P)")
        plt.title(f"Eficiência vs P — {v}, N={maxN}")
        plt.grid(True, alpha=0.3)
        outpng = pathlib.Path(outroot)/f"plot_efficiency_vs_p__{v}__N{maxN}.png"
        plt.tight_layout()
        plt.savefig(outpng)
        plt.close()

def main():
    ap = argparse.ArgumentParser(description="Roda asm e super escalando N (fator>1) até primeira falha; então plota.")
    ap.add_argument("--interp", required=True, help="Caminho do binário interp")
    ap.add_argument("--talm-asm-dir", required=True, help="Diretório TALM/asm (para _assemble_fl.sh)")
    ap.add_argument("--threads", default="1,2,4,8", help="Lista de P (ex.: 1,2,4,8)")
    ap.add_argument("--reps", type=int, default=10, help="Repetições por (N,P,variant)")
    ap.add_argument("--timeout", type=float, default=1.0, help="Timeout (s) por execução do interp")
    ap.add_argument("--n0", type=int, default=500, help="N inicial")
    ap.add_argument("--growth", type=float, default=1.2, help="Fator de crescimento (>1)")
    ap.add_argument("--outroot", default=str(ROOT/"results/ms/growth_until_fail"), help="Raiz dos resultados")
    args = ap.parse_args()

    Ps = [int(x) for x in args.threads.split(",") if x.strip()]
    variants = ["asm","super"]
    outroot = pathlib.Path(args.outroot)
    ensure_dir(outroot)

    runs_csv = outroot / "runs_raw.csv"
    runs_header = ["ts","variant","N","P","rep","rc","elapsed_ms","outdir"]

    env_base = os.environ.copy()
    env_base["TALM_ASM_DIR"] = args.talm_asm_dir

    N = args.n0
    all_rows = []
    broke = False
    broke_info = None

    while True:
        for variant in variants:
            for P in Ps:
                # Caminhos por N
                NROOT = outroot/variant/f"N_{N}"
                ensure_dir(NROOT)

                # 1) build .hsk -> .fl
                HSK = NROOT/f"N_{N}_p{P}.hsk"
                FL  = NROOT/f"N_{N}_p{P}.fl"
                cmd_build = [str(HERE/"_build_fl_from_hsk.sh"), str(HSK), str(FL),
                             "--variant", variant, "--threads", str(P), "--force"]
                rc_b, _ = run(cmd_build, cwd=ROOT, env=env_base)

                # 2) assemble .fl -> .flb/.pla (alinha pla a P)
                cmd_asm = [str(HERE/"_assemble_fl.sh"), str(FL), str(NROOT), "--threads", str(P)]
                rc_a, _ = run(cmd_asm, cwd=ROOT, env=env_base)

                # 3) reps
                for rep in range(1, args.reps+1):
                    OUTDIR = NROOT/f"p_{P}"/f"rep_{rep}"
                    ensure_dir(OUTDIR)
                    cmd_run = [str(HERE/"_run_interp.py"),
                               "--interp", args.interp,
                               "--flb", str(NROOT/f"N_{N}_p{P}.flb"),
                               "--pla", str(NROOT/f"N_{N}_p{P}.pla"),
                               "--threads", str(P),
                               "--variant", variant,
                               "--outdir", str(OUTDIR)]
                    rc, dt_ms = run(cmd_run, timeout=args.timeout, cwd=ROOT, env=env_base)
                    row = {
                        "ts": int(time.time()),
                        "variant": variant,
                        "N": N,
                        "P": P,
                        "rep": rep,
                        "rc": rc,
                        "elapsed_ms": dt_ms,
                        "outdir": str(OUTDIR),
                    }
                    write_csv_row(runs_csv, row, runs_header)
                    all_rows.append(row)

                    print(f"[run] variant={variant} N={N} P={P} rep={rep} rc={rc} t={dt_ms}ms")

                    if rc != 0 and not broke:
                        broke = True
                        broke_info = (variant, N, P, rep, rc)
                        break
                if broke: break
            if broke: break
        if broke:
            v, n_b, p_b, r_b, rc_broke = broke_info
            print(f"[stop] primeira falha: variant={v} N={n_b} P={p_b} rep={r_b} rc={rc_broke}")
            break
        # próximo N
        N = int(math.ceil(N * args.growth))

    # métricas + plots
    metrics = agg_metrics(all_rows)
    mpath = save_metrics_csv(outroot, metrics)
    print(f"[metrics] salvo em: {mpath}")

    try:
        plot_all(outroot)
        print(f"[plots] gerados em: {outroot}")
    except Exception as e:
        print(f"[plots] falhou ao plotar: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
