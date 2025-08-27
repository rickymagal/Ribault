#!/usr/bin/env python3
import argparse, json
from pathlib import Path
from statistics import median

# tenta importar matplotlib; se não tiver, só gera CSV
HAS_MPL = True
try:
    import matplotlib.pyplot as plt
except Exception:
    HAS_MPL = False

def scan(base: Path, maxN: int):
    data = {}  # {variant:{P:{N:[ms...]}}}
    for var_dir in base.iterdir():
        if not var_dir.is_dir(): continue
        var = var_dir.name
        data.setdefault(var, {})
        for Nd in sorted(var_dir.glob("N_*"), key=lambda p:int(p.name.split("_")[1])):
            N = int(Nd.name.split("_")[1])
            if N>maxN: continue
            for Pd in Nd.glob("p_*"):
                P = int(Pd.name.split("_")[1])
                data[var].setdefault(P, {}).setdefault(N, [])
                for repd in Pd.glob("rep_*"):
                    jf = repd/"result.json"
                    if jf.exists():
                        meta = json.load(open(jf))
                        if meta.get("rc",1)==0:
                            data[var][P][N].append(meta["elapsed_ms"])
    return data

def plot_time_vs_N(data, var, outdir: Path):
    if not HAS_MPL: return
    if var not in data: return
    import matplotlib.pyplot as plt
    plt.figure()
    Ps = sorted(data[var].keys())
    Ns = sorted({n for p in Ps for n in data[var][p].keys()})
    for P in Ps:
        xs, ys = [], []
        for N in Ns:
            ts = data[var][P].get(N, [])
            if ts: xs.append(N); ys.append(median(ts))
        if xs:
            plt.plot(xs, ys, label=f"P={P}")
    plt.xlabel("N"); plt.ylabel("tempo mediano (ms)")
    plt.title(f"Tempo vs N — {var}")
    plt.grid(True, alpha=0.3); plt.legend(); plt.tight_layout()
    plt.savefig(outdir/f"time_vs_N_{var}.png", dpi=140); plt.close()

def plot_speedup_eff(data, var, outdir: Path):
    if not HAS_MPL: return
    if var not in data or 1 not in data[var]: return
    import matplotlib.pyplot as plt
    Ps = sorted(data[var].keys())
    Ns = set(data[var][1].keys())
    for P in Ps: Ns &= set(data[var][P].keys())
    if not Ns: return
    Nstar = max(Ns)
    base_t = median(data[var][1][Nstar])
    X, S, E = [], [], []
    for P in Ps:
        ts = data[var][P].get(Nstar, [])
        if ts:
            t = median(ts); s = base_t/t if t>0 else float("nan"); e = s/P
            X.append(P); S.append(s); E.append(e)
    # speedup
    plt.figure(); plt.plot(X,S,marker="o")
    plt.xlabel("P"); plt.ylabel("speedup"); plt.title(f"Speedup @ N={Nstar} — {var}")
    plt.grid(True,alpha=0.3); plt.tight_layout()
    plt.savefig(outdir/f"speedup_{var}.png", dpi=140); plt.close()
    # eficiência
    plt.figure(); plt.plot(X,E,marker="o")
    plt.xlabel("P"); plt.ylabel("eficiência"); plt.title(f"Eficiência @ N={Nstar} — {var}")
    plt.grid(True,alpha=0.3); plt.tight_layout()
    plt.savefig(outdir/f"efficiency_{var}.png", dpi=140); plt.close()

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--base", required=True)
    ap.add_argument("--maxN", type=int, required=True)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    base = Path(args.base); out = Path(args.out); out.mkdir(parents=True, exist_ok=True)
    data = scan(base, args.maxN)

    # CSV sempre
    csv = out/"summary.csv"
    with open(csv,"w") as f:
        f.write("variant,P,N,median_ms\n")
        for var in sorted(data.keys()):
            for P in sorted(data[var].keys()):
                for N in sorted(data[var][P].keys()):
                    arr = data[var][P][N]
                    if arr: f.write(f"{var},{P},{N},{median(arr):.3f}\n")

    if HAS_MPL:
        for var in sorted(data.keys()):
            plot_time_vs_N(data, var, out)
            plot_speedup_eff(data, var, out)
        print(f"[plot] figuras em {out} e resumo em {csv}")
    else:
        print(f"[plot] matplotlib indisponível. Gerei só {csv}.")

if __name__ == "__main__":
    main()
