#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Experimento: n inicia em --start-n e é multiplicado por --growth (>1) a cada passo
(500, ceil(500*growth), ...), rodando p ∈ { --threads } e coletando métricas + gráficos.
Aborta a progressão se QUALQUER run ultrapassar --time-limit (segundos),
mas SEMPRE coleta o que já rodou e plota com dados parciais.

Para --variant=super, compila a HSK injetando cutoff = (N0 // p) no template.
"""

import argparse, pathlib, subprocess, sys, re, math

ROOT = pathlib.Path(__file__).resolve().parents[1]   # HTC/
SCRIPTS = ROOT / "scripts"

def sh(*args, timeout=None, **kw):
    print("[sh]", " ".join(str(a) for a in args))
    return subprocess.run(list(args), check=True, timeout=timeout, **kw)

# ---------------- HSK helpers (mínimo necessário) ----------------
def make_list(n, seed):
    import random
    rng = random.Random(seed)
    return [rng.randrange(0, 100000) for _ in range(n)]

def format_hs_list(xs):
    return "[" + ", ".join(str(x) for x in xs) + "]"

def patch_main_line(hsk_text, list_src):
    # tenta substituir linha com "main = ..."
    pat = re.compile(r'(?m)^\s*main\s*=\s*.*$')
    repl = f"main = mergeSort {list_src}"
    if pat.search(hsk_text):
        return pat.sub(repl, hsk_text, count=1)
    # fallback: injeta main no fim
    if "mergeSort" in hsk_text:
        return hsk_text + "\n\nmain = mergeSort " + list_src + "\n"
    raise RuntimeError("Não achei linha de main para patch.")

def patch_super_cutoff(hsk_text, cutoff):
    """
    Atualiza o cutoff da SUPER. Tenta, na ordem:
      1) 'super_cutoff = <n>'
      2) 'CUTOFF = <n>'
      3) marcador '@SUPER_CUTOFF@'
      4) padrão 'length xs <= <n>' (troca só o número)
      5) fallback: injeta 'super_cutoff = <n>' no topo
    """
    # 1)
    pat1 = re.compile(r'(?m)^(\s*super_cutoff\s*=\s*)(\d+)\s*$')
    if pat1.search(hsk_text):
        return pat1.sub(rf"\g<1>{cutoff}", hsk_text, count=1), "replaced super_cutoff ="

    # 2)
    pat2 = re.compile(r'(?m)^(\s*CUTOFF\s*=\s*)(\d+)\s*$')
    if pat2.search(hsk_text):
        return pat2.sub(rf"\g<1>{cutoff}", hsk_text, count=1), "replaced CUTOFF ="

    # 3)
    if "@SUPER_CUTOFF@" in hsk_text:
        return hsk_text.replace("@SUPER_CUTOFF@", str(cutoff)), "replaced @SUPER_CUTOFF@"

    # 4)
    pat3 = re.compile(r'(length\s+\w+\s*<=\s*)(\d+)')
    m = pat3.search(hsk_text)
    if m:
        s, e = m.span(2)
        return hsk_text[:s] + str(cutoff) + hsk_text[e:], "replaced length<=<n>"

    # 5)
    injected = f"super_cutoff = {cutoff}\n"
    return injected + hsk_text, "injected super_cutoff (fallback, ver template)"

# ---------------- coleta + plots ----------------
def collect_and_plot(root_dir: pathlib.Path, title: str):
    raw = root_dir / "runs_raw.csv"
    metrics = root_dir / "metrics.csv"

    # coleta (sempre)
    sh(str(SCRIPTS / "_collect_metrics.py"),
       "--root", str(root_dir),
       "--raw", str(raw),
       "--out", str(metrics))

    # plot tempo vs n
    try:
        sh(str(SCRIPTS / "_plot_ms.py"),
           "--metrics", str(metrics),
           "--out", str(root_dir / "runtime_vs_n.png"),
           "--title", title)
    except subprocess.CalledProcessError:
        print("[plot] _plot_ms.py: nada para plotar (ainda).")

    # plot scaling (speedup/efficiency)
    try:
        sh(str(SCRIPTS / "_plot_scaling.py"),
           "--metrics", str(metrics),
           "--out", str(root_dir / "scaling_speedup_efficiency.png"),
           "--title", title + " — Escalonamento")
    except subprocess.CalledProcessError:
        print("[plot] _plot_scaling.py: nada para plotar (ainda).")

# ---------------- util ----------------
def next_size(n: int, growth: float) -> int:
    nxt = int(math.ceil(n * growth))
    if nxt <= n:
        nxt = n + 1
    return nxt

# ---------------- main ----------------
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--variant", choices=["asm","super"], required=True)
    ap.add_argument("--libsupers", default=None,
                    help="obrigatório se --variant super")
    ap.add_argument("--start-n", type=int, required=True)
    ap.add_argument("--growth", type=float, default=2.0,
                    help="fator multiplicativo (>1). Ex: 2.0 (dobrar), 1.1, 1.25, ...")
    ap.add_argument("--threads", type=int, nargs="+", default=[1,2,4,8])
    ap.add_argument("--reps",    type=int, default=2)
    ap.add_argument("--seed",    type=int, default=42)
    ap.add_argument("--time-limit", type=int, default=2*60*60,
                    help="limite em segundos para CADA run; se exceder, aborta a progressão")
    ap.add_argument("--template-asm",
                    default=str(ROOT / "test/07_merge_sort.hsk"))
    ap.add_argument("--template-super",
                    default=str(ROOT / "test/21_merge_sort_super.hsk"))
    ap.add_argument("--results-root",
                    default=None,
                    help="raiz dos resultados; default: results/ms/growth_<variant>")
    args = ap.parse_args()

    if args.growth <= 1.0:
        print("Erro: --growth deve ser > 1.", file=sys.stderr)
        sys.exit(2)

    if args.variant == "super" and not args.libsupers:
        print("Erro: --libsupers é obrigatório para --variant super.", file=sys.stderr)
        sys.exit(2)

    results_root = (ROOT / f"results/ms/growth_{args.variant}") if not args.results_root \
                   else pathlib.Path(args.results_root)
    results_root.mkdir(parents=True, exist_ok=True)

    current_n = args.start_n
    keep = True

    while keep:
        print(f"\n=== Passo N={current_n} ({args.variant}, growth={args.growth}) ===")
        step_failed_by_timeout = False

        step_dir = results_root / f"N_{current_n}"
        step_dir.mkdir(parents=True, exist_ok=True)

        # Gera base HSK (lista do main) uma vez por N
        base_tpl_path = pathlib.Path(args.template_super if args.variant=="super" else args.template_asm)
        tpl = base_tpl_path.read_text(encoding="utf-8")
        lst_src = format_hs_list(make_list(current_n, args.seed))
        hsk_base = patch_main_line(tpl, lst_src)

        for p in args.threads:
            # Para SUPER, injeta cutoff = N0 // p
            hsk_text = hsk_base
            if args.variant == "super":
                cutoff = max(1, current_n // p)
                hsk_text, info = patch_super_cutoff(hsk_text, cutoff)
                print(f"[super] cutoff = N0//p = {current_n}//{p} = {cutoff}  ({info})")

            # Arquivos deste p
            hsk_out = step_dir / f"N_{current_n}_p{p}.hsk"
            fl_out  = step_dir / f"N_{current_n}_p{p}.fl"
            flb     = step_dir / f"N_{current_n}_p{p}.flb"
            pla     = step_dir / f"N_{current_n}_p{p}.pla"

            hsk_out.write_text(hsk_text, encoding="utf-8")

            # Compila HSK->FL
            sh(str(SCRIPTS / "_build_fl_from_hsk.sh"), str(hsk_out), str(fl_out))

            # Monta FL->FLB/PLA
            sh(str(SCRIPTS / "_assemble_fl.sh"), str(fl_out), str(step_dir))

            # Executa runs
            for r in range(1, args.reps + 1):
                outdir = step_dir / f"p_{p}" / f"rep_{r}"
                outdir.parent.mkdir(parents=True, exist_ok=True)

                cmd = [str(SCRIPTS / "_run_interp.py"),
                       "--interp", str(pathlib.Path("../TALM/interp/interp")),
                       "--flb", str(flb),
                       "--pla", str(pla),
                       "--threads", str(p),
                       "--variant", "super" if args.variant=="super" else "asm",
                       "--outdir", str(outdir)]
                if args.variant == "super":
                    cmd += ["--libsupers", str(args.libsupers)]
                try:
                    sh(*cmd, timeout=args.time_limit)
                except subprocess.TimeoutExpired:
                    print(f"[timeout] N={current_n} p={p} rep={r} excedeu {args.time_limit}s.")
                    step_failed_by_timeout = True
                    # coleta/parcial imediata
                    title = f"MergeSort ({'SUPER' if args.variant=='super' else 'ASM'}) — growth={args.growth}"
                    try:
                        collect_and_plot(results_root, title)
                    except Exception as e:
                        print(f"[collect/plot parcial] erro: {e}")
                    break  # interrompe reps

            if step_failed_by_timeout:
                break  # interrompe p

        # coleta/plot ao fim do passo (parcial ou completo)
        title = f"MergeSort ({'SUPER' if args.variant=='super' else 'ASM'}) — growth={args.growth}"
        try:
            collect_and_plot(results_root, title)
        except Exception as e:
            print(f"[collect/plot] erro: {e}")

        if step_failed_by_timeout:
            print(f"[stop] Abortando progressão após N={current_n} por timeout.")
            keep = False
        else:
            current_n = next_size(current_n, args.growth)

    print("\n[done] Experimento encerrado (completo ou por timeout).")

if __name__ == "__main__":
    main()
