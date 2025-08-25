#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Experimento MS-ASM-N: tempo vs n para MergeSort **sem super**.
Gera HSK trocando só a lista do main, compila p/ .fl, monta e roda o interp.
"""
import argparse, pathlib, subprocess
from _ms_gen_input import make_list, format_hs_list, patch_main_line

ROOT = pathlib.Path(__file__).resolve().parents[1]   # HTC/
SCRIPTS = ROOT / "scripts"

def sh(*args, **kw):
    print("[sh]", " ".join(str(a) for a in args))
    subprocess.run(list(args), check=True, **kw)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sizes",   type=int, nargs="+", required=True)
    ap.add_argument("--threads", type=int, nargs="+", default=[1, 12])
    ap.add_argument("--reps",    type=int, default=10)
    ap.add_argument("--seed",    type=int, default=42)
    # baseline ASM usa o mergesort sem super por padrão
    ap.add_argument("--template", default=str(ROOT / "test/07_merge_sort.hsk"))
    ap.add_argument("--results",  default=str(ROOT / "results/ms/asm_n"))
    ap.add_argument("--affinity", default=None, help='ex: "0-11" usa taskset')
    args = ap.parse_args()

    results = pathlib.Path(args.results)
    results.mkdir(parents=True, exist_ok=True)

    for n in args.sizes:
        base_dir = results / f"N_{n}"
        base_dir.mkdir(parents=True, exist_ok=True)

        # 1) gera HSK (só troca lista do main)
        tpl = pathlib.Path(args.template).read_text(encoding="utf-8")
        lst_src = format_hs_list(make_list(n, args.seed))
        hsk_text = patch_main_line(tpl, lst_src)

        hsk_out = base_dir / f"N_{n}.hsk"
        hsk_out.write_text(hsk_text, encoding="utf-8")

        # 2) HSK -> FL
        fl_out = base_dir / f"N_{n}.fl"
        sh(str(SCRIPTS / "_build_fl_from_hsk.sh"), str(hsk_out), str(fl_out))

        # 3) FL -> FLB/PLA
        sh(str(SCRIPTS / "_assemble_fl.sh"), str(fl_out), str(base_dir))

        flb = base_dir / f"N_{n}.flb"
        pla = base_dir / f"N_{n}.pla"

        # 4) execuções com interp
        for p in args.threads:
            for r in range(1, args.reps + 1):
                outdir = base_dir / f"p_{p}" / f"rep_{r}"
                sh(str(SCRIPTS / "_run_interp.py"),
                   "--interp", str(pathlib.Path("../TALM/interp/interp")),
                   "--flb", str(flb),
                   "--pla", str(pla),
                   "--threads", str(p),
                   "--variant", "asm",
                   "--outdir", str(outdir),
                   *(["--affinity", args.affinity] if args.affinity else []))

    # 5) coleta
    raw = results / "runs_raw.csv"
    agg = results / "metrics.csv"
    sh(str(SCRIPTS / "_collect_metrics.py"), "--root", str(results), "--raw", str(raw), "--out", str(agg))

    # 6) gráfico
    png = results / "runtime_vs_n.png"
    try:
        sh(str(SCRIPTS / "_plot_ms.py"), "--metrics", str(agg), "--out", str(png),
           "--title", "MergeSort (ASM puro): Tempo vs n")
    except subprocess.CalledProcessError:
        print("[plot] matplotlib ausente? pulei o plot.")

if __name__ == "__main__":
    main()
