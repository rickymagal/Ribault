#!/usr/bin/env python3
import argparse, json, os, subprocess, time
from pathlib import Path
import re

def parse_triplet(outdir: Path):
    s = str(outdir)
    def grab(rx):
        m = re.search(rx, s)
        return int(m.group(1)) if m else None
    return grab(r"/N_(\d+)(/|$)"), grab(r"/p_(\d+)(/|$)"), grab(r"/rep_(\d+)(/|$)")

def write_meta(path, **kw):
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "w") as f:
        json.dump(kw, f)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--interp", required=True)
    ap.add_argument("--flb",    required=True)
    ap.add_argument("--pla",    required=True)
    ap.add_argument("--threads", type=int, required=True)
    ap.add_argument("--variant", choices=["asm","super"], required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--superlib", default=None)
    args = ap.parse_args()

    outdir = Path(args.outdir)
    stdout_path = outdir/"stdout.txt"
    stderr_path = outdir/"stderr.txt"
    meta_path   = outdir/"result.json"
    N,P,rep     = parse_triplet(outdir)

    cmd = [args.interp, str(args.threads), args.flb, args.pla]

    # Para super: se tiver lib, usamos; se não tiver, seguimos sem ela.
    sl = None
    if args.variant == "super":
        cand = (args.superlib or os.environ.get("SUPERLIB") or "").strip()
        if cand and Path(cand).is_file():
            sl = cand
            cmd.append(sl)

    t0 = time.monotonic()
    outdir.mkdir(parents=True, exist_ok=True)
    with open(stdout_path, "wb") as so, open(stderr_path, "wb") as se:
        proc = subprocess.run(cmd, stdout=so, stderr=se)
    dt = (time.monotonic() - t0) * 1000.0
    rc = proc.returncode

    meta = dict(variant=args.variant, N=N, P=P, rep=rep, rc=rc, elapsed_ms=dt, superlib=sl or "")
    write_meta(meta_path, **meta)
    print(f"[run] variant={args.variant} N={N} P={P} rep={rep} rc={rc} t={dt:.2f}ms")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
