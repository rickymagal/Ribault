#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, subprocess, pathlib, os, json, time, sys

def run_cmd_logged(cmd, outdir, env=None, cwd=None):
    outdir = pathlib.Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    so = open(outdir/"stdout.txt", "w")
    se = open(outdir/"stderr.txt", "w")
    try:
        t0 = time.perf_counter()
        p = subprocess.run(cmd, stdout=so, stderr=se, env=env, cwd=cwd, check=False)
        dt_ms = (time.perf_counter() - t0) * 1000.0
    finally:
        so.flush(); se.flush()
        so.close(); se.close()
    meta = {"rc": p.returncode, "elapsed_ms": dt_ms, "cmd": cmd}
    (outdir/"run_meta.json").write_text(json.dumps(meta, indent=2), encoding="utf-8")
    return p.returncode, dt_ms, outdir

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--interp", required=True)
    ap.add_argument("--flb",    required=True)
    ap.add_argument("--pla",    required=True)
    ap.add_argument("--threads", required=True, type=int)
    ap.add_argument("--variant", required=True, choices=["asm","super"])
    ap.add_argument("--libsupers", default=None)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--affinity", default=None)
    args = ap.parse_args()

    # Caminho ABSOLUTO do interp e não mudamos o cwd
    interp = pathlib.Path(args.interp).resolve()

    cmd = [str(interp), str(args.threads), str(args.flb), str(args.pla)]
    if args.variant == "super" and args.libsupers:
        cmd.append(str(args.libsupers))
    if args.affinity:
        cmd = ["taskset", "-c", str(args.affinity)] + cmd

    rc, dt_ms, outdir = run_cmd_logged(cmd, args.outdir, cwd=None)

    if rc == 0:
        print(f"[run] ok. Logs em {outdir}")
        print(f"[run] elapsed_ms={dt_ms:.2f}")
        sys.exit(0)
    else:
        print(f"[run] retorno {rc}. Veja logs:")
        print(f"  {outdir/'stderr.txt'}")
        print(f"  {outdir/'stdout.txt'}")
        print(f"[run] elapsed_ms={dt_ms:.2f}")
        sys.exit(1)

if __name__ == "__main__":
    main()
