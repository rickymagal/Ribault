#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, pathlib, re, json, csv

def parse_triplet(path: pathlib.Path):
    # .../N_32/p_4/rep_2/ -> (n=32, p=4, rep=2)
    mN   = re.search(r"N_(\d+)", str(path))
    mP   = re.search(r"/p_(\d+)/", str(path))
    mRep = re.search(r"/rep_(\d+)/", str(path))
    if not (mN and mP and mRep): return None
    return int(mN.group(1)), int(mP.group(1)), int(mRep.group(1))

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", required=True)
    ap.add_argument("--raw",  required=True)
    ap.add_argument("--out",  required=True)
    args = ap.parse_args()

    root = pathlib.Path(args.root)
    rows = []
    for meta in root.rglob("run_meta.json"):
        trip = parse_triplet(meta)
        if not trip: continue
        n,p,rep = trip
        try:
            data = json.loads(meta.read_text(encoding="utf-8"))
            ms = float(data.get("elapsed_ms", "nan"))
        except Exception:
            continue
        rows.append({"n": n, "p": p, "rep": rep, "elapsed_ms": ms, "dir": str(meta.parent)})

    rows.sort(key=lambda r: (r["p"], r["n"], r["rep"]))

    # RAW
    rawp = pathlib.Path(args.raw)
    rawp.parent.mkdir(parents=True, exist_ok=True)
    with rawp.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["n","p","rep","elapsed_ms","dir"])
        w.writeheader()
        w.writerows(rows)

    # MÉTRICAS agregadas por (n, p) -> média
    outp = pathlib.Path(args.out)
    outp.parent.mkdir(parents=True, exist_ok=True)
    agg = {}
    for r in rows:
        k = (r["n"], r["p"])
        agg.setdefault(k, []).append(r["elapsed_ms"])
    aggr = []
    for (n,p), vals in sorted(agg.items()):
        if not vals: continue
        mean = sum(vals)/len(vals)
        aggr.append({"n": n, "p": p, "mean_ms": mean, "samples": len(vals)})

    with outp.open("w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["n","p","mean_ms","samples"])
        w.writeheader()
        w.writerows(aggr)

    print(f"[collect] OK: {outp}")

if __name__ == "__main__":
    main()
