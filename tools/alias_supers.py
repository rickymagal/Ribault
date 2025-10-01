#!/usr/bin/env python3
import re, sys

if len(sys.argv) != 1 and len(sys.argv) != 2:
    print("uso: alias_supers.py <Supers.hs>")
    sys.exit(1)

path = sys.argv[1] if len(sys.argv)==2 else "Supers.hs"
txt  = open(path, 'r', encoding='utf-8').read()

# 1) Duplicar cada 'foreign export ... "sN"' com alias 'super(N-1)'
def add_alias(m):
    line = m.group(0)
    n = int(m.group(1))
    alias = line.replace(f'"s{n}"', f'"super{n-1}"', 1)
    return line + alias

txt = re.sub(
    r'(?m)^\s*foreign\s+export\s+ccall\s+"s(\d+)"[^\n]*\n',
    add_alias,
    txt
)

# 2) Fortalecer toList para não travar em 0 e colocar limite de passos
pat = re.compile(
    r'(?ms)^toList\s*::\s*Int64\s*->\s*\[Int64\]\s*.*?^fromList\s*::',
    re.M
)
toList_safe = (
    "toList :: Int64 -> [Int64]\n"
    "toList n = go 0 n\n"
    "  where\n"
    "    go k m\n"
    "      | m == nil  = []\n"
    "      | m == 0    = []  -- trata entrada não inicializada/lixo\n"
    "      | k > 2000000 = [] -- trava-dura anti-loop\n"
    "      | otherwise = let h = fstDec m; t = sndDec m in h : go (k+1) t\n\n"
    "fromList ::"
)
txt = pat.sub(toList_safe, txt)

open(path, 'w', encoding='utf-8').write(txt)
