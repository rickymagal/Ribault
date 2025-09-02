#!/usr/bin/env python3
# Executa o interp e, se STRACE estiver definido/fornecido, mostra workload por thread (syscalls) + tempo.
import argparse, os, subprocess, re, time
from collections import Counter

def run_cmd(cmd, cwd=None, env=None):
    print("[run] exec: " + " ".join("'" + c + "'" if " " in c else c for c in cmd), flush=True)
    t0 = time.perf_counter()
    cp = subprocess.run(cmd, cwd=cwd, env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    dt_ms = (time.perf_counter() - t0)*1000.0
    return cp.returncode, cp.stdout, dt_ms

def parse_worker_map(interp_stdout):
    """
    Lê linhas do interp do tipo:
      [debug] worker start id=0 TID=21448 CPU=9
    Retorna:
      workers: dict[id]->tid
    """
    workers = {}
    for ln in interp_stdout.splitlines():
        m = re.search(r"worker start id=(\d+)\s+TID=(\d+)", ln)
        if m:
            wid = int(m.group(1)); tid = int(m.group(2))
            workers[wid] = tid
    return workers

def parse_strace_counts(path, known_tids=None):
    """
    Conta syscalls por TID a partir do arquivo gerado pelo strace.
    Aceita dois formatos:
      "[pid 12345] ..."  (prefixo explícito)
      "12345  ... "      (TID no início da linha)
    Se known_tids for fornecido, prioriza contabilizar só esses TIDs.
    """
    counts = Counter()
    total_time_ms = 0.0

    re_bracket = re.compile(r"^\[pid\s+(\d+)\]")
    re_leading = re.compile(r"^\s*(\d+)\s")  # ex: "21448 12:34:56 ..."

    with open(path, 'r', errors='ignore') as f:
        for ln in f:
            ln = ln.rstrip("\n")
            tid = None

            m = re_bracket.match(ln)
            if m:
                tid = int(m.group(1))
            else:
                m = re_leading.match(ln)
                if m:
                    try:
                        tid = int(m.group(1))
                    except:
                        tid = None

            if tid is None:
                continue

            if known_tids is None or tid in known_tids:
                counts[tid] += 1

            # tempo da syscall (strace -T)
            mt = re.search(r"<([0-9]+\.[0-9]+)>$", ln)
            if mt:
                try:
                    total_time_ms += float(mt.group(1)) * 1000.0
                except:
                    pass

    return counts, total_time_ms

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--interp", required=True)
    ap.add_argument("--threads", type=int, required=True)
    ap.add_argument("--flb", required=True)
    ap.add_argument("--pla", required=True)
    ap.add_argument("--outdir", required=True)
    ap.add_argument("--variant", default="")
    ap.add_argument("--strace-bin", default=os.environ.get("STRACE",""))
    ap.add_argument("--N", type=int, required=True, help="tamanho do problema para log")
    args = ap.parse_args()

    os.makedirs(args.outdir, exist_ok=True)
    strace_txt = os.path.join(args.outdir, "strace.txt")

    print(f"[run] P={args.threads} (default=1)")
    print(f"[run] CWD={args.outdir}")
    print(f"[run] strace_bin={args.strace_bin}")

    cmd = [args.interp, str(args.threads), args.flb, args.pla]
    use_strace = bool(args.strace_bin) and os.path.exists(args.strace_bin)
    if use_strace:
        cmd = [args.strace_bin, "-f", "-tt", "-T", "-qq", "-o", strace_txt, "-E", "LD_LIBRARY_PATH=", "--"] + cmd
    elif args.strace_bin:
        print("[run][WARN] strace não encontrado — seguindo sem trace")

    rc, out, t_ms = run_cmd(cmd, cwd=args.outdir)

    # Ecoa stdout do interp
    if out:
        for ln in out.splitlines():
            if ln.strip():
                print(ln)

    # Mapeia worker id -> TID (a partir do stdout do interp)
    worker_map = parse_worker_map(out)  # ex: {0:21448, 1:21449, ...}

    # Workload por thread (strace)
    if use_strace and os.path.exists(strace_txt):
        known_tids = set(worker_map.values()) if worker_map else None
        counts, t_sys_ms = parse_strace_counts(strace_txt, known_tids=known_tids)

        total = sum(counts.values())
        n_threads = len(counts) if counts else (len(worker_map) if worker_map else 1)
        print(f"[workload] TOTAL: syscalls={total} t_sys_ms={t_sys_ms:.3f} threads={n_threads}")

        # Se temos o mapeamento, imprime por worker id (na ordem do id)
        if worker_map:
            for wid in sorted(worker_map):
                tid = worker_map[wid]
                c = counts.get(tid, 0)
                print(f"[workload] worker[{wid}] TID={tid} syscalls={c}")
            # Se aparecer algum TID no strace que não está no mapa, lista como 'extra'
            extras = [tid for tid in counts.keys() if tid not in worker_map.values()]
            for tid in sorted(extras):
                print(f"[workload] extra TID={tid} syscalls={counts[tid]}")
        else:
            # Sem mapa, imprime por TID puro
            for tid, c in counts.most_common():
                print(f"[workload] TID={tid} syscalls={c}")
    else:
        print("[workload] TOTAL: syscalls=? t_sys_ms=? threads=1")
        if worker_map:
            for wid in sorted(worker_map):
                print(f"[workload] worker[{wid}] TID={worker_map[wid]} syscalls=?")

    print(f"[run] variant={args.variant or 'asm'} N={args.N} P={args.threads} rep=1 rc={rc} t={t_ms:.2f}ms")
    raise SystemExit(rc)

if __name__ == "__main__":
    main()
