#!/usr/bin/env bash
# Native sequential baselines (C, Rust) for LCS. Monolithic 2-row DP,
# identical structure to gen_hs_sequential.py but in the respective native
# language with the leaf-side optimizations (gcc -O3 -march=native; rustc
# release with raw pointers + unsafe, no bounds checks). REPS=7 medians.
#
# Output CSV columns: variant,seq_len,P,rep,seconds,result
#   variant in {seq_c, seq_rust}
#   P always 1 (these are sequential baselines)
#
# Usage:
#   bash run_lcs_seq_native.sh OUTROOT
# Environment:
#   NS    (default: "100000 200000 300000 400000 500000")
#   REPS  (default: 7)
set -euo pipefail

export LANG="${LANG:-C.utf8}"
export LC_ALL="${LC_ALL:-C.utf8}"

SRCDIR="$(cd "$(dirname "$0")" && pwd)"
OUTROOT="${1:-$HOME/results/lcs_seq_native}"
mkdir -p "$OUTROOT"
OUTROOT="$(cd "$OUTROOT" && pwd)"

NS="${NS:-100000 200000 300000 400000 500000}"
REPS="${REPS:-7}"

CSV="$OUTROOT/metrics.csv"
echo "variant,seq_len,P,rep,seconds,result" > "$CSV"

# ---- Build C ----
gcc -O3 -march=native -o "$OUTROOT/lcs_seq_c" "$SRCDIR/lcs_seq.c"
echo "[seq_c] built: $OUTROOT/lcs_seq_c"

# ---- Build Rust ----
mkdir -p "$OUTROOT/rust_build/src"
cp "$SRCDIR/Cargo.toml" "$OUTROOT/rust_build/Cargo.toml"
cp "$SRCDIR/lcs_seq.rs" "$OUTROOT/rust_build/lcs_seq.rs"
(cd "$OUTROOT/rust_build" && cargo build --release --quiet)
echo "[seq_rust] built: $OUTROOT/rust_build/target/release/lcs_seq_rust"

RUST_BIN="$OUTROOT/rust_build/target/release/lcs_seq_rust"
C_BIN="$OUTROOT/lcs_seq_c"

for N in $NS; do
    echo ""
    echo "======== N=$N ========"
    for ((rep=1; rep<=REPS; rep++)); do
        # seq_c (pinned to core 0)
        out="$(taskset -c 0 "$C_BIN" "$N")"
        result_c="$(awk -F= '/^RESULT=/{print $2}' <<< "$out")"
        secs_c="$(awk -F= '/^RUNTIME_SEC=/{print $2}' <<< "$out")"
        echo "  seq_c    rep=$rep -> ${secs_c}s (RESULT=$result_c)"
        echo "seq_c,$N,1,$rep,$secs_c,$result_c" >> "$CSV"

        # seq_rust (pinned to core 0)
        out="$(taskset -c 0 "$RUST_BIN" "$N")"
        result_r="$(awk -F= '/^RESULT=/{print $2}' <<< "$out")"
        secs_r="$(awk -F= '/^RUNTIME_SEC=/{print $2}' <<< "$out")"
        echo "  seq_rust rep=$rep -> ${secs_r}s (RESULT=$result_r)"
        echo "seq_rust,$N,1,$rep,$secs_r,$result_r" >> "$CSV"
    done
done

echo ""
echo "=========================================="
echo " Native sequential sweep complete"
echo " CSV: $CSV"
echo "=========================================="
