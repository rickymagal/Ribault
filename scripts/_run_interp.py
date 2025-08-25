#!/usr/bin/env python3
import argparse, os, subprocess, sys
from pathlib import Path

def run_cmd_logged(cmd, outdir, env=None, cwd=None):
    outdir = Path(outdir); outdir.mkdir(parents=True, exist_ok=True)
    with open(outdir / "stdout.txt", "w") as so, open(outdir / "stderr.txt", "w") as se:
        so.write(f"CMD: {cmd!r}\n")
        if env and "LD_LIBRARY_PATH" in env:
            so.write(f"LD_LIBRARY_PATH: {env['LD_LIBRARY_PATH']}\n")
        so.flush()
        p = subprocess.run(cmd, stdout=so, stderr=se, env=env, cwd=cwd, check=False)
        return p.returncode

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--interp", required=True)
    ap.add_argument("--flb", required=True)
    ap.add_argument("--pla", required=True)
    ap.add_argument("--threads", type=int, required=True)
    ap.add_argument("--variant", choices=["asm","super"], required=True)
    ap.add_argument("--libsupers")
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--affinity")  # opcional
    args = ap.parse_args()

    interp_path = Path(args.interp).resolve()
    interp_dir  = interp_path.parent

    # Ambiente: rts-local SEMPRE; ghc-deps quando for SUPER
    ldparts = [str(interp_dir / "rts-local")]
    if args.variant == "super" and args.libsupers:
        ghcdeps = Path(args.libsupers).resolve().parent / "ghc-deps"
        ldparts.append(str(ghcdeps))
    # Preserva LD_LIBRARY_PATH do usuário
    if os.environ.get("LD_LIBRARY_PATH"):
        ldparts.append(os.environ["LD_LIBRARY_PATH"])
    env = dict(os.environ)
    env["LD_LIBRARY_PATH"] = ":".join(ldparts)

    # Comando exatamente como você roda à mão
    cmd = [str(interp_path), str(args.threads), str(Path(args.flb).resolve()), str(Path(args.pla).resolve())]
    if args.variant == "super":
        if not args.libsupers:
            print("Faltou --libsupers para variant=super", file=sys.stderr)
            sys.exit(2)
        cmd.append(str(Path(args.libsupers).resolve()))

    # Executa no diretório do interp (igual ao seu teste manual)
    rc = run_cmd_logged(cmd, args.outdir, env=env, cwd=str(interp_dir))

    if rc != 0:
        print(f"[run] retorno {rc}. Veja logs:\n  {args.outdir}/stderr.txt\n  {args.outdir}/stdout.txt")
        sys.exit(1)
    else:
        print(f"[run] ok. Logs em {args.outdir}")

if __name__ == "__main__":
    main()
