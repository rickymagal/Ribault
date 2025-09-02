#!/usr/bin/env python3
import argparse, sys
from pathlib import Path

def pick_template(repo_root: Path) -> Path:
    test_dir = repo_root / "test"
    cands = sorted(test_dir.glob("*.hsk"))
    if not cands:
        sys.exit(f"[ms_gen_input] ERRO: nenhum .hsk encontrado em {test_dir}")
    prefer = [p for p in cands if any(k in p.name.lower() for k in ("merge", "sort"))]
    return (prefer or cands)[0]

def main():
    ap = argparse.ArgumentParser(description="Gera .hsk a partir de template válido.")
    ap.add_argument("--n", required=True, type=int)
    ap.add_argument("--P", type=int, default=1)
    ap.add_argument("--variant", choices=["asm","super"], default="asm")
    ap.add_argument("--vec", choices=["ones","desc"], default="ones")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    repo_root = Path(__file__).resolve().parents[1]
    tpl = pick_template(repo_root)

    txt = tpl.read_text(encoding="utf-8")
    # remove qualquer linha que o lexer não aceite (ex.: comentários iniciados em '#')
    txt = "\n".join(ln for ln in txt.splitlines() if not ln.lstrip().startswith("#")) + "\n"

    # substituições opcionais—se o template tiver placeholders, beleza; se não, fica igual
    repl = {
        "__N__": str(args.n), "__P__": str(args.P), "__VEC__": args.vec,
        "{{N}}": str(args.n), "{{P}}": str(args.P), "{{VEC}}": args.vec,
    }
    for k, v in repl.items():
        txt = txt.replace(k, v)

    out = Path(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(txt, encoding="utf-8")
    print(f"[ms_gen_input] wrote {out} (variant={args.variant} N={args.n} P={args.P} vec={args.vec})")

if __name__ == "__main__":
    main()
