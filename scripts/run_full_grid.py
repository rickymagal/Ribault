#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, os, sys, math, time, json, csv, subprocess
from statistics import median
from pathlib import Path

HERE = Path(__file__).resolve().parent

# ---------- util ----------
def sh(cmd, cwd=None, env=None, timeout=None):
    t0 = time.time()
    p = subprocess.run(cmd, cwd=cwd, env=env, text=True,
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    dt_ms = (time.time() - t0) * 1000.0
    return p.returncode, dt_ms, p.stdout, p.stderr

def ensure_dir(p): Path(p).mkdir(parents=True, exist_ok=True)

def write_csv_row(csv_path, header, row):
    is_new = not Path(csv_path).exists()
    with open(csv_path, "a", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=header)
        if is_new: w.writeheader()
        w.writerow(row)

# ---------- plotting (ingles) ----------
def plot_all(outroot: Path):
    try:
        import matplotlib.pyplot as plt
    except Exception as e:
        print(f"[plot] matplotlib unavailable: {e}", file=sys.stderr)
        return

    mpath = outroot / "metrics.csv"
    rows = []
    with open(mpath, "r", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            # casts
            row["N"] = int(row["N"])
            row["P"] = int(row["P"])
            for k in ("median_ms","speedup","efficiency"):
                row[k] = float(row[k]) if row[k] else float("nan")
            rows.append(row)

    variants = sorted(set(r["variant"] for r in rows))
    Ps = sorted(set(r["P"] for r in rows))

    # 1) Time vs N (one curve per P) for each variant
    for v in variants:
        Ns_all = sorted(set(r["N"] for r in rows if r["variant"]==v))
        plt.figure()
        plotted = False
        for P in Ps:
            data = [r for r in rows if r["variant"]==v and r["P"]==P and not math.isnan(r["median_ms"])]
            if not data: continue
            data.sort(key=lambda x:x["N"])
            xs = [d["N"] for d in data]
            ys = [d["median_ms"] for d in data]
            plt.plot(xs, ys, marker="o", label=f"P={P}")
            plotted = True
        if plotted:
            plt.xlabel("Array size (N)")
            plt.ylabel("Median time (ms)")
            plt.title(f"Time vs N — {v}")
            plt.grid(True, alpha=0.3); plt.legend()
            plt.tight_layout()
            plt.savefig(outroot / f"time_vs_N_{v}.png", dpi=140)
        plt.close()

    # 2) Speedup & 3) Efficiency vs P at largest common N per variant (baseline P=1)
    for v in variants:
        byP = {}
        Ns_sets = []
        for P in Ps:
            NsP = sorted(set(r["N"] for r in rows if r["variant"]==v and r["P"]==P and not math.isnan(r["median_ms"])))
            if NsP: Ns_sets.append(set(NsP))
        if not Ns_sets: continue
        Ns_common = set.intersection(*Ns_sets) if Ns_sets else set()
        if not Ns_common: continue
        Nstar = max(Ns_common)

        # median time per P @ Nstar
        base = None
        X, Sp, Ef = [], [], []
        for P in Ps:
            d = [r for r in rows if r["variant"]==v and r["P"]==P and r["N"]==Nstar and not math.isnan(r["median_ms"])]
            if not d: continue
            t = d[0]["median_ms"]
            if P==1: base = t
            X.append(P)
            Sp.append(float('nan'))  # fill now
            Ef.append(float('nan'))
        if base is None: continue
        for i,P in enumerate(X):
            d = [r for r in rows if r["variant"]==v and r["P"]==P and r["N"]==Nstar and not math.isnan(r["median_ms"])]
            t = d[0]["median_ms"]
            s = base/t if t>0 else float('nan')
            e = s/P
            Sp[i] = s; Ef[i] = e

        # speedup
        import matplotlib.pyplot as plt
        plt.figure()
        plt.plot(X, Sp, marker="o")
        plt.xlabel("Number of cores (P)")
        plt.ylabel("Speedup (vs P=1)")
        plt.title(f"Speedup @ N={Nstar} — {v}")
        plt.grid(True, alpha=0.3); plt.tight_layout()
        plt.savefig(outroot / f"speedup_{v}.png", dpi=140)
        plt.close()

        # efficiency
        plt.figure()
        plt.plot(X, Ef, marker="o")
        plt.xlabel("Number of cores (P)")
        plt.ylabel("Efficiency (Speedup / P)")
        plt.title(f"Efficiency @ N={Nstar} — {v}")
        plt.grid(True, alpha=0.3); plt.tight_layout()
        plt.savefig(outroot / f"efficiency_{v}.png", dpi=140)
        plt.close()

    # 4) For each P, compare variants over N (ASM vs SUPER)
    all_Ps = sorted(set(r["P"] for r in rows))
    for P in all_Ps:
        plt.figure()
        plotted_any = False
        Ns_union = sorted(set(r["N"] for r in rows if r["P"]==P))
        for v in variants:
            data = [r for r in rows if r["variant"]==v and r["P"]==P and not math.isnan(r["median_ms"])]
            if not data: continue
            data.sort(key=lambda x:x["N"])
            xs = [d["N"] for d in data]
            ys = [d["median_ms"] for d in data]
            if xs:
                plt.plot(xs, ys, marker="o", label=v.upper())
                plotted_any = True
        if plotted_any:
            plt.xlabel("Array size (N)")
            plt.ylabel("Median time (ms)")
            plt.title(f"ASM vs SUPER — comparison per core count (P={P})")
            plt.grid(True, alpha=0.3); plt.legend(title="Variant")
            plt.tight_layout()
            plt.savefig(outroot / f"variants_compare_P{P}.png", dpi=140)
        plt.close()

    print(f"[plot] figures saved to {outroot}")

# ---------- metrics ----------
def save_metrics(outroot: Path, rows):
    # rows: list of dicts {variant,N,P,rep,rc,elapsed_ms}
    # aggregate per (variant,N,P)
    agg = {}
    for r in rows:
        key = (r["variant"], r["N"], r["P"])
        agg.setdefault(key, []).append(r)
    outcsv = outroot / "metrics.csv"
    header = ["variant","N","P","n_runs","n_ok","median_ms","mean_ms","std_ms","speedup","efficiency"]
    with open(outcsv, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=header); w.writeheader()
        # group by variant,N to compute baseline P=1
        by_varN = {}
        for (v,N,P), lst in sorted(agg.items()):
            ok = [x["elapsed_ms"] for x in lst if x["rc"]==0]
            m = len(ok); n = len(lst)
            med = median(ok) if ok else None
            mean = sum(ok)/m if m else None
            std = (sum((t-mean)**2 for t in ok)/m)**0.5 if m else None
            by_varN.setdefault((v,N), {})[P] = med
            w.writerow({
                "variant": v, "N": N, "P": P,
                "n_runs": n, "n_ok": m,
                "median_ms": f"{med:.3f}" if med is not None else "",
                "mean_ms": f"{mean:.3f}" if mean is not None else "",
                "std_ms": f"{std:.3f}" if std is not None else "",
                "speedup": "", "efficiency": ""
            })
    # add speedup/efficiency columns (rewrite file)
    rows2 = []
    with open(outcsv, "r", encoding="utf-8") as f:
        r = list(csv.DictReader(f))
    # compute speedup/eff per variant,N using baseline P=1
    medmap = {(row["variant"], int(row["N"]), int(row["P"])): (float(row["median_ms"]) if row["median_ms"] else None)
              for row in r}
    for row in r:
        v,N,P = row["variant"], int(row["N"]), int(row["P"])
        base = medmap.get((v,N,1))
        med  = medmap.get((v,N,P))
        if base and med and med>0:
            s = base/med
            e = s/P
            row["speedup"]    = f"{s:.3f}"
            row["efficiency"] = f"{e:.3f}"
        rows2.append(row)
    with open(outcsv, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=r[0].keys()); w.writeheader(); w.writerows(rows2)
    print(f"[metrics] saved: {outcsv}")
    return outcsv

# ---------- main orchestrator ----------
def main():
    ap = argparse.ArgumentParser(description="Orchestrates full pipeline: .hsk -> .fl (+libsupers.so), .flb/.pla, runs, and plots.")
    ap.add_argument("--interp", required=True, help="Path to TALM/interp/interp")
    ap.add_argument("--asm-root", required=True, help="Path to TALM/asm (assembler & scheduler)")
    ap.add_argument("--threads", default="1,2,4,6,8,10,12", help="Comma list of P")
    ap.add_argument("--variants", default="asm,super", help="Comma list: asm,super")
    ap.add_argument("--reps", type=int, default=10)
    ap.add_argument("--timeout", type=float, default=2.0)
    ap.add_argument("--start-N", type=int, default=20000)
    ap.add_argument("--step", type=int, default=20000, help="Step for N")
    ap.add_argument("--n-max", type=int, default=200000)
    ap.add_argument("--outroot", default="results/ms/grid", help="Results root directory")
    # optional input shape hint to your _build_fl_from_hsk.sh / _ms_gen_input.py
    ap.add_argument("--input-shape", default="", help="optional: ones|descending (forwarded via env MS_INPUT_SHAPE)")
    args = ap.parse_args()

    Ps = [int(x) for x in args.threads.split(",") if x.strip()]
    variants = [v.strip() for v in args.variants.split(",") if v.strip()]
    outroot = Path(args.outroot); ensure_dir(outroot)

    # check helpers
    build_sh = HERE / "_build_fl_from_hsk.sh"
    asm_sh   = HERE / "_assemble_fl.sh"
    runner   = HERE / "_run_interp.py"
    for f in (build_sh, asm_sh, runner):
        if not f.exists():
            print(f"FATAL: missing helper {f}", file=sys.stderr); sys.exit(2)

    env = os.environ.copy()
    env["TALM_ASM_DIR"] = args.asm_root
    if args.input_shape:
        env["MS_INPUT_SHAPE"] = args.input_shape

    runs_csv = outroot / "runs_raw.csv"
    runs_header = ["ts","variant","N","P","rep","rc","elapsed_ms","outdir","stderr"]
    rows = []

    N = args.start_N
    while N <= args.n_max:
        for variant in variants:
            NROOT = outroot / variant / f"N_{N}"
            ensure_dir(NROOT)
            for P in Ps:
                base = f"N_{N}_p{P}"
                HSK = NROOT / f"{base}.hsk"
                FL  = NROOT / f"{base}.fl"

                # 1) build (.hsk -> .fl and, if super, libsupers.so + ghc-deps)
                rc_b, dt_b, so, se = sh([str(build_sh), str(HSK), str(FL),
                                          "--variant", variant, "--threads", str(P), "--force"],
                                         cwd=HERE.parent, env=env)
                if rc_b != 0:
                    print(f"[build FAIL] var={variant} N={N} P={P} rc={rc_b}\n{se}", file=sys.stderr)

                # 2) assemble (.fl -> .flb + .pla rr by P)
                rc_a, dt_a, so, se = sh([str(asm_sh), str(FL), str(NROOT), "--threads", str(P)],
                                         cwd=HERE.parent, env=env)
                if rc_a != 0:
                    print(f"[assemble FAIL] var={variant} N={N} P={P} rc={rc_a}\n{se}", file=sys.stderr)

                flb = NROOT / f"{base}.flb"
                pla = NROOT / f"{base}.pla"  # seu assemble gera .pla correto para P

                # 3) run reps
                for rep in range(1, args.reps+1):
                    OUTDIR = NROOT / f"p_{P}" / f"rep_{rep}"
                    ensure_dir(OUTDIR)
                    rc, dt_ms, so, se = sh([sys.executable, str(runner),
                                            "--interp", args.interp,
                                            "--flb", str(flb),
                                            "--pla", str(pla),
                                            "--threads", str(P),
                                            "--variant", variant,
                                            "--outdir", str(OUTDIR)],
                                           cwd=HERE.parent, env=env)
                    row = {
                        "ts": int(time.time()), "variant": variant, "N": N, "P": P, "rep": rep,
                        "rc": rc, "elapsed_ms": round(dt_ms,2), "outdir": str(OUTDIR), "stderr": se.strip()[:5000]
                    }
                    write_csv_row(runs_csv, runs_header, row)
                    rows.append(row)
                    print(f"[run] variant={variant} N={N} P={P} rep={rep} rc={rc} t={dt_ms:.2f}ms")
        N += args.step

    # metrics + plots
    mpath = save_metrics(outroot, rows)
    plot_all(outroot)
    print(f"[done] results in: {outroot}")

if __name__ == "__main__":
    main()
