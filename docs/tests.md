# Correctness Tests

**Inputs:** `test/*.hsk` (e.g., `01_literals.hsk`, `21_merge_sort_super.hsk`, …).

**Goldens (expected outputs):**
- `test/golden-ast/` — baseline AST DOT.
- `test/golden-df/` — baseline Dataflow DOT.
- `test/golden-talm/` — baseline TALM assembly.

**Generated outputs** go into:
- `test/ast-output/` (+ PNGs in `test/ast-images/`)
- `test/df-output/` (+ PNGs in `test/df-images/`)
- `test/code-output/`

Typical workflow:

```bash
make clean && make            # regenerate everything
# visually diff DOT/PNG vs golden
# or script-driven diffs in scripts/
```
