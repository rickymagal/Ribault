#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import argparse, csv, os
from collections import defaultdict

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

def load_metrics(path):
    rows=[]
    with open(path) as f:
        r=csv.DictReader(f)
        for row in r:
            rows.append({
                "variant": row["variant"],
                "N": int(row["N"]),
                "P": int(row["P"]),
                "rep": int(row["rep"]),
                "secs": float(row["seconds"]),
            })
    return rows

def summarize(rows):
    g=defaultdict(list)
    for r in rows:
        g[(r["variant"],r["N"],r["P"])].append(r["secs"])
    out=[]
    for (v,N,P),vals in sorted(g.items()):
        mean=sum(vals)/len(vals)
        std=(sum((x-mean)**2 for x in vals)/(len(vals)-1))**0.5 if len(vals)>1 else 0.0
        out.append({"variant":v,"N":N,"P":P,"mean":mean,"std":std,"reps":len(vals)})
    return out

def plot_time_vs_N(summary, outdir):
    byv=defaultdict(list)
    for r in summary: byv[r["variant"]].append(r)
    for v,rows in byv.items():
        byP=defaultdict(list)
        for r in rows: byP[r["P"]].append(r)
        plt.figure()
        for P,its in sorted(byP.items()):
            its=sorted(its,key=lambda x:x["N"])
            xs=[i["N"] for i in its]; ys=[i["mean"] for i in its]; yerr=[i["std"] for i in its]
            plt.errorbar(xs,ys,yerr=yerr,label=f"P={P}")
        plt.xlabel("N"); plt.ylabel("Tempo (s)")
        plt.title(f"Tempo vs N ({v})"); plt.grid(True,alpha=.3); plt.legend(); plt.tight_layout()
        plt.savefig(os.path.join(outdir,f"time_vs_N_{v}.png")); plt.close()

def plot_speedup_eff(summary, outdir):
    bykey=defaultdict(list)
    for r in summary: bykey[(r["variant"],r["N"])].append(r)
    for (v,N),its in sorted(bykey.items()):
        its=sorted(its,key=lambda x:x["P"])
        base=its[0]["mean"]
        xs=[i["P"] for i in its]
        su=[base/i["mean"] for i in its]
        ef=[(base/i["mean"])/i["P"] for i in its]
        plt.figure(); plt.plot(xs,su,marker="o"); plt.xlabel("P"); plt.ylabel("Speedup")
        plt.title(f"Speedup ({v}, N={N})"); plt.grid(True,alpha=.3); plt.tight_layout()
        plt.savefig(os.path.join(outdir,f"speedup_{v}_N{N}.png")); plt.close()
        plt.figure(); plt.plot(xs,ef,marker="o"); plt.xlabel("P"); plt.ylabel("Efficiency")
        plt.title(f"Efficiency ({v}, N={N})"); plt.grid(True,alpha=.3); plt.tight_layout()
        plt.savefig(os.path.join(outdir,f"efficiency_{v}_N{N}.png")); plt.close()

def plot_best_vs_best(summary, outdir):
    from collections import defaultdict
    byv=defaultdict(list)
    for r in summary: byv[r["variant"]].append(r)
    commons=None
    for v in byv:
        s=set(r["P"] for r in byv[v])
        commons = s if commons is None else commons & s
    if not commons: return
    Pstar=max(commons)
    plt.figure()
    for v,rs in byv.items():
        its=[x for x in rs if x["P"]==Pstar]
        its=sorted(its,key=lambda x:x["N"])
        xs=[i["N"] for i in its]; ys=[i["mean"] for i in its]; yerr=[i["std"] for i in its]
        plt.errorbar(xs,ys,yerr=yerr,label=f"{v} (P={Pstar})")
    plt.xlabel("N"); plt.ylabel("Tempo (s)")
    plt.title(f"Melhor vs Melhor (P={Pstar})"); plt.grid(True,alpha=.3); plt.legend(); plt.tight_layout()
    plt.savefig(os.path.join(outdir,f"best_vs_best_P{Pstar}.png")); plt.close()

def main():
    ap=argparse.ArgumentParser()
    ap.add_argument("--metrics", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--tag", default="exp")
    args=ap.parse_args()

    rows=load_metrics(args.metrics)
    summary=summarize(rows)
    with open(os.path.join(args.outdir, f"{args.tag}_summary.csv"),"w",newline="") as f:
        w=csv.writer(f); w.writerow(["variant","N","P","mean_secs","stdev_secs","reps"])
        for r in summary: w.writerow([r["variant"],r["N"],r["P"],r["mean"],r["std"],r["reps"]])

    plot_time_vs_N(summary,args.outdir)
    plot_speedup_eff(summary,args.outdir)
    plot_best_vs_best(summary,args.outdir)
    print("[plots] ok")

if __name__=="__main__":
    main()
