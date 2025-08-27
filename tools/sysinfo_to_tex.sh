#!/usr/bin/env bash
set -euo pipefail
IN="${1:-machine_info.txt}"
OUT="${2:-experimental_setup.tex}"

grab()   { grep -m1 -F "$1" "$IN" 2>/dev/null | sed -E "s/.*$1[[:space:]]*:?[[:space:]]*//" || true; }
grabre() { grep -m1 -E "$1"  "$IN" 2>/dev/null | sed -E "s/.*$1[[:space:]]*:?[[:space:]]*//" || true; }

cpu_model="$(grab 'Model name')"
cpu_family="$(grab 'CPU family')"
cpu_modelnum="$(grab 'Model:')"
cpu_stepping="$(grab 'Stepping')"
sockets="$(grab 'Socket(s)')"
cores_per_socket="$(grab 'Core(s) per socket')"
threads_per_core="$(grab 'Thread(s) per core')"
cpus_logical="$(grab 'CPU(s):')"
l1d="$(grab 'L1d cache')"; l1i="$(grab 'L1i cache')"
l2="$(grab 'L2 cache')";  l3="$(grab 'L3 cache')"
isa_flags="$(grab 'Flags:')"
min_mhz="$(grab 'CPU min MHz')"; max_mhz="$(grab 'CPU max MHz')"

numa_nodes="$(grab 'NUMA node(s)')"
mem_total="$(awk '/^Mem:/ {print $2}' "$IN" 2>/dev/null | head -n1)"
ram_type="$(grep -m1 -i '^\s*Type:' "$IN" 2>/dev/null | awk '{print $2}')"
ram_speeds="$(grep -i 'Configured Memory Speed' "$IN" 2>/dev/null | sed -E 's/.*: *//;' | sort -u | tr '\n' ' ' )"
[ -z "${ram_speeds:-}" ] && ram_speeds="$(grep -i '^ *Speed:' "$IN" 2>/dev/null | sed -E 's/.*: *//;' | sort -u | tr '\n' ' ')"

os_name="$(grep -m1 '^PRETTY_NAME=' "$IN" 2>/dev/null | sed -E 's/^PRETTY_NAME="?([^"]*)"?/\1/')"
kernel="$(grep -m1 '^Linux ' "$IN" 2>/dev/null | awk '{print $3}')"

gov_driver="$(grab 'driver:')"
gov_mode="$(grab 'governor:')"
no_turbo="$(grab 'intel_pstate.no_turbo:')"
boost="$(grab 'cpufreq.boost:')"
turbo_state="unknown"
[ "${no_turbo:-}" = "1" ] && turbo_state="off"
[ "${no_turbo:-}" = "0" ] && turbo_state="on"
[ "${boost:-}" = "0" ] && turbo_state="off"
[ "${boost:-}" = "1" ] && turbo_state="on"

gcc_ver="$(grep -m1 -E '^gcc ' "$IN" 2>/dev/null | sed 's/ (.*//')"
clang_ver="$(grep -m1 -E '^clang version' "$IN" 2>/dev/null)"
ld_ver="$(grep -m1 -E '^GNU ld' "$IN" 2>/dev/null)"
glibc_ver="$(grep -m1 -E '^ldd ' "$IN" 2>/dev/null)"
ghc_ver="$(grep -m1 -E 'Glasgow Haskell|^ghc ' "$IN" 2>/dev/null || true)"

# Derivados
phys_cores="NA"
[[ "${sockets:-}" =~ ^[0-9]+$ && "${cores_per_socket:-}" =~ ^[0-9]+$ ]] && phys_cores=$(( sockets * cores_per_socket ))
smt_state="off"; [[ "${threads_per_core:-1}" -gt 1 ]] && smt_state="on"

# ISA resumo
isa_summary=""
echo "$isa_flags" | grep -qw avx512  && isa_summary="AVX-512"
echo "$isa_flags" | grep -qw avx2    && isa_summary="${isa_summary:+$isa_summary, }AVX2"
echo "$isa_flags" | grep -qw sse4_2  && isa_summary="${isa_summary:+$isa_summary, }SSE4.2"
[ -z "$isa_summary" ] && isa_summary="(see full flags)"

repos="$(grep -E '^Current repo:' "$IN" 2>/dev/null | sed 's/^Current repo: //')"
[ -z "$repos" ] && repos="(provide Ribault and Trebuchet commit hashes)"

cat > "$OUT" <<EOF
\paragraph{Hardware.}
CPU: \emph{${cpu_model:-N/A}} (family ${cpu_family:-N/A}, model ${cpu_modelnum:-N/A}, stepping ${cpu_stepping:-N/A}); ${sockets:-N/A} socket(s),
${phys_cores} physical cores, ${cpus_logical:-N/A} threads (SMT ${smt_state}, ${threads_per_core:-N/A} thread(s)/core).
Base/boost: min ${min_mhz:-N/A}~MHz, max ${max_mhz:-N/A}~MHz.
Caches: L1d ${l1d:-N/A}, L1i ${l1i:-N/A}, L2 ${l2:-N/A}, L3 ${l3:-N/A}.
ISA: ${isa_summary}.
Memory: ${mem_total:-N/A} total; type ${ram_type:-N/A}; configured speed(s): ${ram_speeds:-N/A}
Topologia NUMA: ${numa_nodes:-N/A} node(s).

\paragraph{Software.}
${os_name:-N/A}, kernel \texttt{${kernel:-N/A}}.
Toolchain: \texttt{${gcc_ver:-N/A}}; \texttt{${clang_ver:-N/A}}; \texttt{${ld_ver:-N/A}}; \texttt{${glibc_ver:-N/A}}.
$([ -n "${ghc_ver:-}" ] && printf 'GHC: \\texttt{%s}.\n' "$ghc_ver")
CPUfreq: driver \texttt{${gov_driver:-N/A}}, governor \texttt{${gov_mode:-N/A}}, turbo ${turbo_state}.

\paragraph{Runtime / Artifacts.}
Repos: ${repos}.
EOF

printf 'Wrote %s\n' "$OUT"
