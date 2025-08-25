#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import random, re

def make_list(n: int, seed: int = 42):
    rng = random.Random(seed)
    return [rng.randrange(0, 10**6) for _ in range(n)]

def format_hs_list(xs):
    return "[" + ",".join(str(x) for x in xs) + "]"

def patch_main_line(template_text: str, new_list_src: str) -> str:
    """
    Troca **apenas** a lista do main, preservando o nome da função chamada.
    Aceita espaços/; no fim da linha.
    Exemplos que casa:
      main = mergeSort0 [1,2,3];
      main    =   merge (foo)   [9,8]   ;
    """
    # pega a linha de main com a lista entre [ ... ] e o ; final
    pat = re.compile(
        r'(?m)^(?P<indent>\s*)main\s*=\s*(?P<head>.*?\[)'
        r'(?P<list>[^\]]*)'
        r'(?P<tail>\]\s*;)\s*$'
    )
    m = pat.search(template_text)
    if not m:
        # fallback: se o ; não está na mesma linha (raro), tente sem o ;
        pat2 = re.compile(
            r'(?m)^(?P<indent>\s*)main\s*=\s*(?P<head>.*?\[)'
            r'(?P<list>[^\]]*)'
            r'(?P<tail>\])\s*$'
        )
        m = pat2.search(template_text)
        if not m:
            raise SystemExit("ERRO: não achei a linha do `main = ...` com lista no template .hsk")

    repl = f"{m.group('indent')}main = {m.group('head')}{new_list_src}{m.group('tail')}"
    return template_text[:m.start()] + repl + template_text[m.end():]
