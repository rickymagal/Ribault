#!/usr/bin/env python3
# Balanceia o arquivo .pla gerado pelo assembler mantendo o número de mapeamentos.
# Formato esperado do .pla:
#   <N>
#   <node_id> <pe_id>
#   ...
# onde N = número de linhas de mapeamento (não é P).
import argparse

def ler_pla(path):
    with open(path, "r") as f:
        lines = [ln.strip() for ln in f if ln.strip()]
    if not lines:
        raise SystemExit(f"[balance_pla][ERRO] .pla vazio: {path}")
    try:
        N_hdr = int(lines[0])
    except:
        raise SystemExit(f"[balance_pla][ERRO] header inválido no .pla: {lines[0]!r}")
    rows = []
    for ln in lines[1:]:
        parts = ln.split()
        if len(parts) < 2:  # tolera linhas curtas
            continue
        try:
            node = int(parts[0])
            pe   = int(parts[1])
        except:
            continue
        rows.append((node, pe))
    # se header divergir do real, usa o real
    if N_hdr != len(rows):
        N_hdr = len(rows)
    return N_hdr, rows

def escrever_pla(path, N, rows):
    with open(path, "w") as f:
        f.write(f"{N}\n")
        for node, pe in rows:
            f.write(f"{node} {pe}\n")

def balance(rows, P):
    # round-robin pelos P PEs
    balanced = []
    dist = [0]*P
    for i, (node, _) in enumerate(rows):
        pe = i % P
        balanced.append((node, pe))
        dist[pe] += 1
    return balanced, dist

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--flb", required=True, help="(ignoramos o conteúdo; só para compat)")
    ap.add_argument("--pla", required=True, help="caminho do .pla a reescrever")
    ap.add_argument("-P", "--threads", type=int, required=True)
    ap.add_argument("--print-summary", action="store_true")
    args = ap.parse_args()

    P = args.threads
    if P <= 0:
        raise SystemExit("[balance_pla][ERRO] P inválido")

    N, rows = ler_pla(args.pla)
    rows_bal, dist = balance(rows, P)
    escrever_pla(args.pla, N, rows_bal)

    if args.print_summary:
        print(f"[debug] pla: tarefas={N} P={P} dist={dist}")

if __name__ == "__main__":
    main()
