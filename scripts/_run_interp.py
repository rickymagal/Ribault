#!/usr/bin/env python3
import argparse, json, subprocess, time, pathlib, sys, os

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--interp",   required=True)
    ap.add_argument("--flb",      required=True)
    ap.add_argument("--pla",      required=True)
    ap.add_argument("--threads",  required=True, type=int)
    ap.add_argument("--variant",  required=True, choices=["asm","super"])
    ap.add_argument("--libsupers")
    ap.add_argument("--outdir",   required=True)
    args = ap.parse_args()

    outdir = pathlib.Path(args.outdir)
    outdir.mkdir(parents=True, exist_ok=True)

    # Usa caminho ABSOLUTO para o intérprete e NÃO muda o cwd.
    interp_path = pathlib.Path(args.interp).resolve()
    cmd = [str(interp_path), str(args.threads), args.flb, args.pla]
    if args.variant == "super" and args.libsupers:
        cmd.append(args.libsupers)

    so = open(outdir / "stdout.txt", "wb")
    se = open(outdir / "stderr.txt", "wb")

    t0 = time.time()
    try:
        p = subprocess.run(cmd, stdout=so, stderr=se, check=False)
        rc = p.returncode
    except Exception as e:
        rc = 127
        se.write(str(e).encode("utf-8", errors="ignore"))
    finally:
        so.flush(); se.flush(); so.close(); se.close()

    elapsed_ms = (time.time() - t0) * 1000.0

    meta = {
        "interp": str(interp_path),
        "threads": args.threads,
        "variant": args.variant,
        "flb": args.flb,
        "pla": args.pla,
        "libsupers": args.libsupers,
        "rc": rc,
        "elapsed_ms": elapsed_ms,
        "cmd": cmd,
        "env": {k:v for k,v in os.environ.items() if k.startswith("TREBUCHET_") or k in ("NUM_CORES",)}
    }
    with open(outdir / "run_meta.json", "w") as f:
        json.dump(meta, f, indent=2)

    # Não deixe este wrapper matar o experimento orquestrado:
    print(f"[run] rc={rc} elapsed_ms={elapsed_ms:.2f} outdir={outdir}")
    sys.exit(0)

if __name__ == "__main__":
    main()
