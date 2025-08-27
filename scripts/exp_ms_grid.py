#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Roda MergeSort (ASM puro ou SUPER) em grade N×p com guards:
- Sequência densa de N (start..max_safe, step) + cap nos "sizes" pedidos
- p múltiplos: --threads 1 2 4 8 12
- timeout por execução e continuação mesmo se houver falhas
- coleta/plot depois com _collect_metrics.py e _plot_scaling.py
"""
import argparse, pathlib, subprocess, sys
from _ms_gen_input import make_list, format_hs_list, patch_main_line

ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"

def sh_ok(*args, **kw):
    print("[sh]", " ".join(str(a) for a in args))
    subprocess.run(list(args), check=True, **kw)

def run_cmd(*args, **kw):
    print("[run]", " ".join(str(a) for a in args))
    return subprocess.run(list(args), check=False, **kw).returncode

def seq_dense(start, step, max_safe):
    xs = []
    x = start
    while x <= max_safe:
        xs.append(x); x += step
    return xs

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--variant", choices=["asm","super"], required=True)
    ap.add_argument("--sizes", type=int, nargs="+", required=True,
                    help="tamanhos alvo (p.ex. 4096) — serão capados por --max-safe")
    ap.add_argument("--start", type=int, required=True)
    ap.add_argument("--step",  type=int, required=True)
    ap.add_argument("--max-safe", type=int, required=True)
    ap.add_argument("--threads", type=int, nargs="+", required=True)
    ap.add_argument("--reps", type=int, default=3)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--timeout", type=int, default=None, help="segundos por run")
    ap.add_argument("--affinity", default=None)
    ap.add_argument("--template", default=None, help="HSK de template")
    ap.add_argument("--libsupers", default=None)
    ap.add_argument("--results", default=None)
    args = ap.parse_args()

    # defaults por variante
    if args.variant == "asm":
        tpl_default = ROOT / "test/07_merge_sort.hsk"
        out_root = ROOT / "results/ms/asm_np"
    else:
        tpl_default = ROOT / "test/21_merge_sort_super.hsk"
        out_root = ROOT / "results/ms/super_np"

    template = pathlib.Path(args.template) if args.template else tpl_default
    results  = pathlib.Path(args.results) if args.results else out_root
    results.mkdir(parents=True, exist_ok=True)

    # gera sequência densa segura
    print(f"[{ap.prog}] tamanhos requisitados {args.sizes} excedem --max-safe={args.max_safe}.")
    Ns = seq_dense(args.start, args.step, args.max_safe)
    print(f"[{ap.prog}] usando sequência densa: {Ns}")

    # gera HSK/FL/FLB+PLA uma vez por N
    for n in Ns:
        base_dir = results / f"N_{n}"
        base_dir.mkdir(parents=True, exist_ok=True)
        tpl = template.read_text(encoding="utf-8")
        lst_src = format_hs_list(make_list(n, args.seed))
        hsk_text = patch_main_line(tpl, lst_src)
        hsk_out = base_dir / f"N_{n}.hsk"
        hsk_out.write_text(hsk_text, encoding="utf-8")

        fl_out = base_dir / f"N_{n}.fl"
        sh_ok(str(SCRIPTS / "_build_fl_from_hsk.sh"), str(hsk_out), str(fl_out))
        sh_ok(str(SCRIPTS / "_assemble_fl.sh"), str(fl_out), str(base_dir))

    # roda grade N×p×rep (continua em caso de erro/timeouts)
    interp = str((ROOT / "../TALM/interp/interp").resolve())
    for n in Ns:
        flb = results / f"N_{n}" / f"N_{n}.flb"
        pla = results / f"N_{n}" / f"N_{n}.pla"
        for p in args.threads:
            for r in range(1, args.reps + 1):
                outdir = results / f"N_{n}" / f"p_{p}" / f"rep_{r}"
                cmd = [str(SCRIPTS / "_run_interp.py"),
                       "--interp", interp,
                       "--flb", str(flb),
                       "--pla", str(pla),
                       "--threads", str(p),
                       "--variant", args.variant,
                       "--outdir", str(outdir)]
                if args.variant == "super":
                    if not args.libsupers:
                        print("[warn] --libsupers não fornecido para SUPER")
                    else:
                        cmd += ["--libsupers", str(args.libsupers)]
                if args.affinity:
                    cmd += ["--affinity", args.affinity]
                if args.timeout:
                    cmd += ["--timeout", str(args.timeout)]

                rc = run_cmd(*cmd)
                if rc != 0:
                    print(f"[warn] n={n} p={p} rep={r} falhou (rc={rc}). Continuando.")

    # coleta
    raw = results / "runs_raw.csv"
    agg = results / "metrics.csv"
    sh_ok(str(SCRIPTS / "_collect_metrics.py"), "--root", str(results), "--raw", str(raw), "--out", str(agg))

    # plots de scaling (runtime/speedup/efficiency vs p)
    sh_ok(str(SCRIPTS / "_plot_scaling.py"),
          "--metrics", str(agg),
          "--outdir", str(results),
          "--title", f"MergeSort ({args.variant.upper()}): scaling vs p")

if __name__ == "__main__":
    main()
