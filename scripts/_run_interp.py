#!/usr/bin/env python3
import argparse, os, sys, subprocess, shlex, time

def main():
    ap = argparse.ArgumentParser(description="Run interp with proper LD_LIBRARY_PATH.")
    ap.add_argument("--interp", required=True)
    ap.add_argument("--flb",    required=True)
    ap.add_argument("--pla",    required=True)
    ap.add_argument("--P",      required=True, type=int)
    ap.add_argument("--lib",    default="")
    ap.add_argument("--cwd",    default="")
    ap.add_argument("--extra-ld", default="")
    args = ap.parse_args()

    interp = os.path.abspath(args.interp)
    flb    = os.path.abspath(args.flb)
    pla    = os.path.abspath(args.pla)
    lib    = os.path.abspath(args.lib) if args.lib else ""
    workdir = os.path.abspath(args.cwd) if args.cwd else os.getcwd()

    interp_dir = os.path.dirname(interp)
    ghc_deps   = os.path.join(interp_dir, "ghc-deps")
    rts_local  = os.path.join(interp_dir, "rts-local")
    libdir     = os.path.dirname(lib) if lib else ""

    pieces = []
    if libdir:    pieces.append(libdir)
    if os.path.isdir(ghc_deps):  pieces.append(ghc_deps)
    if os.path.isdir(rts_local): pieces.append(rts_local)
    if args.extra_ld: pieces.extend([p for p in args.extra_ld.split(":") if p])

    env = os.environ.copy()
    env["LD_LIBRARY_PATH"] = ":".join([p for p in pieces if p] + [env.get("LD_LIBRARY_PATH","")])

    cmd = [interp, str(args.P), flb, pla] + ([lib] if lib else [])
    sys.stderr.write(f"[run] exec: {' '.join(shlex.quote(c) for c in cmd)}\n")
    sys.stderr.write(f"[run] LD_LIBRARY_PATH={env['LD_LIBRARY_PATH']}\n")

    t0 = time.perf_counter()
    proc = subprocess.Popen(cmd, cwd=workdir, env=env,
                            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                            text=True, bufsize=1)
    try:
        for line in proc.stdout:
            sys.stdout.write(line)
    finally:
        rc = proc.wait()
    ms = (time.perf_counter() - t0) * 1000.0
    # Não muda o formato dos logs que você já está usando:
    print(f"[rc] {rc}")
    print(f"[ms] {ms:.2f}")
    sys.exit(rc)

if __name__ == "__main__":
    main()
