#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Experimento MS-SUPER-N: tempo vs n para MergeSort **com super**.
Gera HSK trocando só a lista do main, compila p/ .fl, monta e roda o interp.
Se o usuário pedir tamanhos acima do limite seguro, o script reduz e
densifica automaticamente para produzir uma curva contínua até o máximo.
"""
import argparse, pathlib, subprocess, sys
from _ms_gen_input import make_list, format_hs_list, patch_main_line

ROOT = pathlib.Path(__file__).resolve().parents[1]   # HTC/
SCRIPTS = ROOT / "scripts"

def sh(*args, **kw):
    print("[sh]", " ".join(str(a) for a in args))
    subprocess.run(list(args), check=True, **kw)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--sizes",   type=int, nargs="+", required=True,
                    help="tamanhos-alvo; se houver algum > --max-safe, serão substituídos por uma sequência densa até o máximo seguro")
    ap.add_argument("--threads", type=int, nargs="+", default=[1, 4, 12])
    ap.add_argument("--reps",    type=int, default=10)
    ap.add_argument("--seed",    type=int, default=42)

    # super: usa a versão com instruções super + lib .so
    ap.add_argument("--template", default=str(ROOT / "test/21_merge_sort_super.hsk"))
    ap.add_argument("--libsupers", default=str(ROOT / "test/supers/21_merge_sort_super/libsupers.so"))
    ap.add_argument("--results",  default=str(ROOT / "results/ms/super_n"))
    ap.add_argument("--affinity", default=None, help='ex: "0-11" usa taskset')

    # guardas/controle da sequência densa
    ap.add_argument("--max-safe", type=int, default=512,
                    help="maior N seguro para a variante SUPER (padrão: 512)")
    ap.add_argument("--start", type=int, default=128,
                    help="início da sequência densa quando houver clamp (padrão: 128)")
    ap.add_argument("--step",  type=int, default=32,
                    help="passo da sequência densa quando houver clamp (padrão: 32)")
    args = ap.parse_args()

    if args.step <= 0:
        print("[exp_ms_super_n] --step deve ser > 0")
        sys.exit(2)

    # Se pediram tamanhos acima do seguro, clamp + densificação pra ter continuidade
    need_clamp = any(n > args.max_safe for n in args.sizes)
    if need_clamp:
        start = max(args.start, args.step)  # evita 0/negativo
        sizes = list(range(start, args.max_safe + 1, args.step))
        print(f"[exp_ms_super_n] tamanhos requisitados {args.sizes} excedem --max-safe={args.max_safe}.")
        print(f"[exp_ms_super_n] usando sequência densa: {sizes}")
    else:
        # mantém exatamente o que o usuário pediu
        sizes = args.sizes

    results = pathlib.Path(args.results)
    results.mkdir(parents=True, exist_ok=True)

    for n in sizes:
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
                   "--variant", "super",
                   "--libsupers", str(args.libsupers),
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
           "--title", "MergeSort (SUPER): Tempo vs n")
    except subprocess.CalledProcessError:
        print("[plot] matplotlib ausente? pulei o plot.")

if __name__ == "__main__":
    main()
