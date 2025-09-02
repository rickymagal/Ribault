#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, os, re, csv, sys, pathlib

def parse_workload(repdir):
    sfile = os.path.join(repdir, "strace.txt")
    total_calls, total_ms, threads = "?", "?", "?"
    if os.path.isfile(sfile):
        total_ms_acc = 0.0
        calls = 0
        rx = re.compile(r"<([\d\.]+)>$")
        with open(sfile, 'r', errors='replace') as f:
            for ln in f:
                m = rx.search(ln.strip())
                if m:
                    calls += 1
                    total_ms_acc += float(m.group(1))*1000.0
        total_calls = calls
        total_ms = round(total_ms_acc, 3)
    # threads heurístico: conta de "worker start id="
    wstart = 0
    rxd = re.compile(r"\[debug\] worker start id=")
    log = os.path.join(repdir, "log.txt")  # caso você redirecione stdout
    if os.path.isfile(log):
        with open(log, 'r', errors='replace') as f:
            for ln in f:
                if rxd.search(ln): wstart += 1
    return str(total_calls), str(total_ms), str(wstart+1 if wstart else "?")

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repdir", required=True)
    ap.add_argument("--variant", required=True)
    ap.add_argument("--n", type=int, required=True)
    ap.add_argument("--P", type=int, required=True)
    ap.add_argument("--rep", type=int, required=True)
    ap.add_argument("--out-tsv", required=True)
    args = ap.parse_args()

    calls, tms, nthr = parse_workload(args.repdir)
    row = dict(variant=args.variant, N=args.n, P=args.P, rep=args.rep,
               syscalls=calls, t_sys_ms=tms, threads=nthr)
    out = pathlib.Path(args.out_tsv)
    newfile = not out.exists()
    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("a", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["variant","N","P","rep","syscalls","t_sys_ms","threads"], delimiter='\t')
        if newfile: w.writeheader()
        w.writerow(row)
    print(f"[metrics] append -> {out} :: {row}")

if __name__ == "__main__":
    main()
