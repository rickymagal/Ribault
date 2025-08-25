#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Experimento MS-SUPER-N: runtime vs n para MergeSort com super.
Requer libsupers.so gerada para este caso.
"""
import argparse, pathlib, subprocess, sys
from _ms_gen_input import make_list, format_hs_list, patch_main_line

ROOT = pathlib.Path(__file__).resolve().parents[1]   # HTC/
SCRIPTS = ROOT / "scripts"

def sh(*args, **kw):
    print("[sh]", " ".join(str(a) for a in args))
    subprocess.run(list(args), check=True, **kw)

def auto_find_libsupers(base: pathlib.Path) -> pathlib.Path | None:
    # procura libsupers.so em test/supers/**/libsupers.so
    cands = list((base / "test" / "supers").rglob("libsupers.so"))
    if not cands:
        return None
    # preferir quem tem "merge_sort" ou "merge" no caminho
    pref = [p for p in cands if "merge_sort" in str(p).lower() or "merge" in str(p).lower()]
    return pref[0] if pref else cands[0]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sizes", type=int, nargs="+", required=True)
    ap.add_argument("--threads", type=int, nargs="+", default=[1, 12])
    ap.add_argument("--reps", type=int, default=10)
    ap.add_argument("--seed", type=int, default=42)
    ap.add_argument("--template", default=str(ROOT / "test/21_merge_sort_super.hsk"))
    ap.add_argument("--results",  default=str(ROOT / "results/ms/super_n"))
    # default agora no lugar certo; aceite "auto" para procurar sozinho
    ap.add_argument("--libsupers",
                    default=str(ROOT / "test/supers/21_merge_sort_super/libsupers.so"),
                    help='caminho do libsupers.so ou "auto" para auto-detecção')
    ap.add_argument("--affinity",  default=None, help='ex: "0-11" usa taskset')
    args = ap.parse_args()

    results = pathlib.Path(args.results)
    results.mkdir(parents=True, exist_ok=True)

    # resolve libsupers
    lib_arg = args.libsupers
    if lib_arg == "auto":
        auto = auto_find_libsupers(ROOT)
        if not auto:
            print("[ERRO] não achei libsupers.so em test/supers/**")
            sys.exit(2)
        libp = auto
    else:
        libp = pathlib.Path(lib_arg)

    if not libp.exists():
        print(f"[ERRO] libsupers não encontrada: {libp}")
        sys.exit(2)

    for n in args.sizes:
        base_dir = results / f"N_{n}"
        base_dir.mkdir(parents=True, exist_ok=True)

        # 1) gera HSK (só troca lista do main)
        hsk_out = base_dir / f"N_{n}.hsk"
        tpl = pathlib.Path(args.template).read_text(encoding="utf-8")
        lst = format_hs_list(make_list(n, args.seed))
        hsk_text = patch_main_line(tpl, lst)
        hsk_out.write_text(hsk_text, encoding="utf-8")

        # 2) HSK -> FL
        fl_out = base_dir / f"N_{n}.fl"
        sh(str(SCRIPTS / "_build_fl_from_hsk.sh"), str(hsk_out), str(fl_out))

        # 3) FL -> FLB/PLA
        sh(str(SCRIPTS / "_assemble_fl.sh"), str(fl_out), str(base_dir))

        flb = base_dir / f"N_{n}.flb"
        pla = base_dir / f"N_{n}.pla"

        # 4) execuções com interp + libsupers
        for p in args.threads:
            p_dir = base_dir / f"p_{p}"
            for r in range(1, args.reps + 1):
                outdir = p_dir / f"rep_{r}"
                sh(str(SCRIPTS / "_run_interp.py"),
                   "--interp", str(pathlib.Path("../TALM/interp/interp")),
                   "--flb", str(flb),
                   "--pla", str(pla),
                   "--threads", str(p),
                   "--variant", "super",
                   "--libsupers", str(libp),
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
           "--title", "MergeSort (com super): Tempo vs n")
    except subprocess.CalledProcessError:
        print("[plot] matplotlib ausente? pulei o plot.")

if __name__ == "__main__":
    main()
