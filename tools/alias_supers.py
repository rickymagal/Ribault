#!/usr/bin/env python3
import re, sys, io

if len(sys.argv) != 2:
    print("uso: alias_supers.py <Supers.hs>")
    sys.exit(1)

path = sys.argv[1]
with io.open(path, 'r', encoding='utf-8') as f:
    src = f.read().splitlines()

out = []
pat = re.compile(r'^\s*foreign\s+export\s+ccall\s+"s(\d+)"\s')

for line in src:
    out.append(line)
    m = pat.match(line)
    if m:
        n = int(m.group(1))
        alias = line.replace(f'"s{n}"', f'"super{n-1}"', 1)
        out.append(alias)

text = "\n".join(out)
if not text.endswith("\n"):
    text += "\n"

with io.open(path, 'w', encoding='utf-8') as f:
    f.write(text)
