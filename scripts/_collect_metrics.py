#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, csv, math, pathlib, re, statistics as stats

TIME_RE = re.compile(r"wall=(\d+(?:\.\d+)?)\s+rss=(\d+)")

def scan_runs(root):
    rows = []
    for p in pathlib.Path(root).rglob("time.log"):
        d = p.parent
        # tenta inferir n, p, rep dos nomes dos diretórios
        # .../N_<n>/p_<p>/rep_<r>/time.log
        parts = d.parts
        n = pth = rep = None
        for i, seg in enumerate(parts):
            if seg.startswith("N_"):
                n = int(seg[2:])
            if seg.startswith("p_"):
                pth = int(seg[2:])
            if seg.startswith("rep_"):
                rep = int(seg[4:])
        wall = rss = None
        for line in p.read_text().splitlines():
            m = TIME_RE.search(line)
            if m:
                wall = float(m.group(1)); rss = int(m.group(2))
        if wall is None:
            continue
        rows.append({"n": n, "p": pth, "rep": rep, "wall_s": wall, "rss_kb": rss, "path": str(d)})
    return rows

def iqr_filter(values):
    if len(values) < 4:  # nada agressivo com poucas reps
        return values, []
    q1 = stats.quantiles(values, n=4)[0]
    q3 = stats.quantiles(values, n=4)[2]
    iqr = q3 - q1
    lo = q1 - 1.5*iqr
    hi = q3 + 1.5*iqr
    keep = [v for v in values if lo <= v <= hi]
    drop = [v for v in values if not (lo <= v <= hi)]
    return keep, drop

def mean_ci95(xs):
    if not xs:
        return (math.nan, math.nan)
    m = stats.mean(xs)
    sd = stats.pstdev(xs) if len(xs) < 2 else stats.stdev(xs)
    # aproximação 95% ~ 1.96*SE (ok para n>=8; sem scipy)
    se = sd / math.sqrt(len(xs)) if len(xs) > 0 else math.nan
    ci = 1.96 * se
    return (m, ci)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--root", required=True, help="diretório do experimento (ex.: results/ms/asm_n)")
    ap.add_argument("--out",  required=True, help="CSV final agregado")
    ap.add_argument("--raw",  required=False, help="CSV com execuções individuais")
    args = ap.parse_args()

    rows = scan_runs(args.root)
    if args.raw:
        with open(args.raw, "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=["n","p","rep","wall_s","rss_kb","path"])
            w.writeheader(); w.writerows(rows)

    # agrega por (n,p)
    agg = {}
    for r in rows:
        agg.setdefault((r["n"], r["p"]), []).append(r["wall_s"])

    out_rows = []
    for (n,p), vals in sorted(agg.items()):
        kept, dropped = iqr_filter(vals)
        m, ci = mean_ci95(kept)
        out_rows.append({
            "n": n, "p": p,
            "runs": len(vals), "kept": len(kept), "dropped": len(dropped),
            "T_mean_s": f"{m:.6f}", "CI95_s": f"{ci:.6f}"
        })

    with open(args.out, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=["n","p","runs","kept","dropped","T_mean_s","CI95_s"])
        w.writeheader(); w.writerows(out_rows)
    print(f"[collect] OK: {args.out}")

if __name__ == "__main__":
    main()
