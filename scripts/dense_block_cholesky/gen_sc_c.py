#!/usr/bin/env python3
"""Generate Ribault dense block Cholesky files with C-implemented supers.

Dense block Cholesky DAG with chained serial updates per block. Each op
in dag.bin becomes a super in the .fl, parameterised by op_idx; the
super body reads (target_i, target_j, src1, src2) from the in-memory
DAG loaded at init.

Super types (kind -> super number):
  init   (s13)
  potrf  (s12)
  trsm   (s11)
  syrk   (s14)
  gemm   (s15)
  output (s10)

Each op super takes its dependencies as sync inputs plus a const op_idx.
"""

import argparse, os, struct


SUPER_OUTPUT = 10
SUPER_TRSM   = 11
SUPER_POTRF  = 12
SUPER_INIT   = 13
SUPER_SYRK   = 14
SUPER_GEMM   = 15

OP_KIND_TO_NAME = {0: 'potrf', 1: 'trsm', 2: 'syrk', 3: 'gemm'}
OP_KIND_TO_NUM  = {0: SUPER_POTRF, 1: SUPER_TRSM, 2: SUPER_SYRK, 3: SUPER_GEMM}


def read_dag(data_dir):
    """Returns list of ops: each is dict {id, kind, ti, tj, s1i, s1j, s2i, s2j, level, deps}."""
    path = os.path.join(data_dir, "dag.bin")
    ops = []
    with open(path, "rb") as f:
        (n_ops,) = struct.unpack("<i", f.read(4))
        for _ in range(n_ops):
            kind, ti, tj, s1i, s1j, s2i, s2j, level, n_deps = struct.unpack("<9i", f.read(36))
            deps = list(struct.unpack(f"<{n_deps}i", f.read(4 * n_deps))) if n_deps else []
            ops.append({'id': len(ops), 'kind': kind, 'ti': ti, 'tj': tj,
                        's1i': s1i, 's1j': s1j, 's2i': s2i, 's2j': s2j,
                        'level': level, 'deps': deps})
    return ops


C_TEMPLATE = r"""/* Auto-generated: Ribault dense block Cholesky C supers. */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define NB        __NB__
#define B         __B__
#define N_OPS     __N_OPS__

static const char *DATA_DIR = "__DATA_DIR__";

static double *A;          /* block storage [n_blocks * B * B] */
static int   *op_kind;     /* [N_OPS] */
static int   *op_ti;
static int   *op_tj;
static int   *op_s1i;
static int   *op_s1j;
static int   *op_s2i;
static int   *op_s2j;

static void *xmalloc(size_t n) { void *p = malloc(n); if (!p) { fprintf(stderr, "OOM %zu\n", n); exit(1); } return p; }
static inline int block_idx(int i, int j) { return i * (i + 1) / 2 + j; }
static inline double *block_ptr(int i, int j) { return A + (size_t)block_idx(i, j) * B * B; }

static void load_dag(void) {
    char path[1024]; snprintf(path, sizeof path, "%s/dag.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    int n_ops;
    if (fread(&n_ops, sizeof(int), 1, f) != 1) { fprintf(stderr, "short read dag.bin hdr\n"); exit(1); }
    if (n_ops != N_OPS) { fprintf(stderr, "dag.bin n_ops mismatch %d vs %d\n", n_ops, N_OPS); exit(1); }
    op_kind = xmalloc(N_OPS * sizeof(int));
    op_ti   = xmalloc(N_OPS * sizeof(int));
    op_tj   = xmalloc(N_OPS * sizeof(int));
    op_s1i  = xmalloc(N_OPS * sizeof(int));
    op_s1j  = xmalloc(N_OPS * sizeof(int));
    op_s2i  = xmalloc(N_OPS * sizeof(int));
    op_s2j  = xmalloc(N_OPS * sizeof(int));
    for (int i = 0; i < N_OPS; i++) {
        int hdr[9];
        if (fread(hdr, sizeof(int), 9, f) != 9) { fprintf(stderr, "short read op %d\n", i); exit(1); }
        op_kind[i] = hdr[0]; op_ti[i] = hdr[1]; op_tj[i] = hdr[2];
        op_s1i[i] = hdr[3]; op_s1j[i] = hdr[4]; op_s2i[i] = hdr[5]; op_s2j[i] = hdr[6];
        int n_deps = hdr[8];
        if (n_deps > 0) {
            int skip[128];
            if (n_deps > 128) { fprintf(stderr, "deps overflow\n"); exit(1); }
            if (fread(skip, sizeof(int), n_deps, f) != (size_t)n_deps) { fprintf(stderr, "short read deps\n"); exit(1); }
        }
    }
    fclose(f);
}

static int64_t s_init(void) {
    int n_blocks = NB * (NB + 1) / 2;
    A = xmalloc((size_t)n_blocks * B * B * sizeof(double));
    char path[1024]; snprintf(path, sizeof path, "%s/A.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    if (fread(A, sizeof(double), (size_t)n_blocks * B * B, f) != (size_t)n_blocks * B * B) {
        fprintf(stderr, "short read A.bin\n"); exit(1);
    }
    fclose(f);
    load_dag();
    return 0;
}

static inline void potrf_block(double *D) {
    for (int j = 0; j < B; j++) {
        double s = D[j*B + j];
        for (int kk = 0; kk < j; kk++) s -= D[j*B + kk] * D[j*B + kk];
        D[j*B + j] = sqrt(s);
        double inv = 1.0 / D[j*B + j];
        for (int i = j + 1; i < B; i++) {
            double t = D[i*B + j];
            for (int kk = 0; kk < j; kk++) t -= D[i*B + kk] * D[j*B + kk];
            D[i*B + j] = t * inv;
        }
    }
    for (int i = 0; i < B; i++)
        for (int j = i + 1; j < B; j++)
            D[i*B + j] = 0.0;
}

static inline void trsm_block(double *X, const double *L) {
    for (int i = 0; i < B; i++) {
        for (int j = 0; j < B; j++) {
            double s = X[i*B + j];
            for (int kk = 0; kk < j; kk++) s -= X[i*B + kk] * L[j*B + kk];
            X[i*B + j] = s / L[j*B + j];
        }
    }
}

static inline void syrk_block(double *C, const double *A_) {
    for (int i = 0; i < B; i++) {
        for (int j = 0; j <= i; j++) {
            double s = 0.0;
            for (int kk = 0; kk < B; kk++) s += A_[i*B + kk] * A_[j*B + kk];
            C[i*B + j] -= s;
        }
    }
}

static inline void gemm_block(double *C, const double *A_, const double *B_) {
    for (int i = 0; i < B; i++) {
        for (int j = 0; j < B; j++) {
            double s = 0.0;
            for (int kk = 0; kk < B; kk++) s += A_[i*B + kk] * B_[j*B + kk];
            C[i*B + j] -= s;
        }
    }
}

static int64_t s_potrf(int64_t op_idx) {
    int idx = (int)op_idx;
    potrf_block(block_ptr(op_ti[idx], op_tj[idx]));
    return 0;
}
static int64_t s_trsm(int64_t op_idx) {
    int idx = (int)op_idx;
    trsm_block(block_ptr(op_ti[idx], op_tj[idx]), block_ptr(op_s1i[idx], op_s1j[idx]));
    return 0;
}
static int64_t s_syrk(int64_t op_idx) {
    int idx = (int)op_idx;
    syrk_block(block_ptr(op_ti[idx], op_tj[idx]), block_ptr(op_s1i[idx], op_s1j[idx]));
    return 0;
}
static int64_t s_gemm(int64_t op_idx) {
    int idx = (int)op_idx;
    gemm_block(block_ptr(op_ti[idx], op_tj[idx]),
               block_ptr(op_s1i[idx], op_s1j[idx]),
               block_ptr(op_s2i[idx], op_s2j[idx]));
    return 0;
}

static int64_t s_output(void) {
    int n_blocks = NB * (NB + 1) / 2;
    uint64_t cs = 0;
    for (size_t k = 0; k < (size_t)n_blocks * B * B; k++) {
        int64_t fixed = (int64_t)(A[k] * 1e6);
        cs = (cs + (uint64_t)((uint32_t)fixed)) & 0xFFFFFFFFu;
    }
    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    fflush(stdout);
    return (int64_t)cs;
}

void s__SUPER_INIT__(int64_t *in, int64_t *out)  { (void)in;  out[0] = s_init(); }
void s__SUPER_POTRF__(int64_t *in, int64_t *out) { /* deps + const at the end */
    int n = 0; while (in[n] != INT64_MIN && n < 30) n++;  /* unused: not robust */
    /* The last input is the const (op_idx). The runtime passes inputs in
       declaration order; we read in[n-1] as the op_idx. But we don't know n.
       So: we encode op_idx as the LAST input always. The .fl declares
       deps first then const last. */
}
/* The above won't work directly. Simpler: the .fl declares the const LAST,
   and the super body assumes the op_idx is at a known position. We pick:
   position = 0 (first input is always the op_idx const), deps follow.
   But .fl conventions in attention/mergesort had const at index 1 (after
   one sync input). For multi-dep supers we need a different scheme.

   Simplest fix: declare op_idx as the LAST input by reading from in[arity-1].
   But we don't know arity at runtime without passing it. Easier: declare
   const as input 0 (first), then deps follow as in[1..n].

   Actually since we know the op kind, we know the dep count is variable but
   the const is always the LAST in the .fl declaration. The Trebuchet runtime
   passes inputs in declaration order. So we just need the BODY to know the
   index of the const. Since arity varies (1..3 deps + 1 const), we'd need
   the runtime to tell us arity, or we use a fixed position. */
"""

# Given the complexity above, let me use a simpler approach: emit each op
# with EXACTLY 3 deps (padding with -1 const if fewer) + 1 const (op_idx).
# Then super body always has 4 inputs: in[0..2] are deps (ignored values),
# in[3] is op_idx. This makes the .fl uniform.

# But the assembler infers arity from CALL site. If we vary call-site arity,
# the body's `in` array also varies. We need consistency.

# Easiest: each op super takes ALL its deps explicitly + op_idx as last input.
# The body just reads in[deps_count] = op_idx. But body doesn't know deps_count.

# CORRECT SOLUTION: encode op_idx as the FIRST input (a const). Then deps follow.
# Body reads in[0] = op_idx. Arity is whatever the .fl call site says.

# Looking at attention's super body:
#   void s__SUPER_PHASE_A__(int64_t *in, int64_t *out) {
#     out[0] = s_phaseA(in[1]);  // in[0]=trigger, in[1]=block_idx
#   }
# So they have trigger first, block_idx second. The body knows the position.

# For Cholesky I'll do: each op super has VARIABLE arity, with op_idx as the
# LAST input. Body reads in[i] where i = arity - 1. To know arity at runtime,
# the .fl could pass arity as another const. OR, simpler: each super body
# loops to find the non-zero terminator.

# Alternative SIMPLEST: have only ONE op super type that handles ALL ops via
# a runtime switch on op_kind. This collapses 4 super types to 1, but the
# variable deps issue remains.

# Pragmatic solution: emit each op as a super with FIXED 4 inputs (always
# pass exactly 3 deps, padding with the previous op id if fewer). Plus op_idx.
# Body reads in[3] always.

# I'll go with this: pad deps to exactly 3 by repeating an existing dep if
# needed. The chain-serialization in build_dag means we have at least 1 dep
# for everything except INIT. We can pad shorter dep lists with the FIRST dep.


def node_var(op_id):
    return f"op_{op_id}"


def emit_fl(out_dir, ops):
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('potrf',  {SUPER_POTRF},  1, False, False)",
        f"superinst('trsm',   {SUPER_TRSM},   1, False, False)",
        f"superinst('syrk',   {SUPER_SYRK},   1, False, False)",
        f"superinst('gemm',   {SUPER_GEMM},   1, False, False)",
        f"superinst('output', {SUPER_OUTPUT}, 1, False, False)",
        "avgtime('potrf', 100)",
        "avgtime('trsm',  500)",
        "avgtime('syrk',  500)",
        "avgtime('gemm',  500)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]

    # For each op: emit its const + super invocation. Deps come from ops[i].deps.
    # If no deps, depend on ini. Op super arity = max(1, len(deps)) + 1 (op_idx const).
    # Body reads the LAST input as op_idx; arity inferred via convention.
    # For simplicity we ALWAYS put op_idx as the LAST input.

    for op in ops:
        op_id   = op['id']
        op_kind = op['kind']
        op_name = OP_KIND_TO_NAME[op_kind]
        # Pick dep sync inputs: explicit deps, or ini if none.
        dep_vars = [node_var(d) for d in op['deps']] if op['deps'] else ['ini']
        # const for op_idx
        fl_lines.append(f"const k_{op_id}, {op_id}")
        fl_lines.append(f"{op_name} {node_var(op_id)}, " + ", ".join(dep_vars + [f"k_{op_id}"]))

    # Output: depends on the last op (which is the root TRSM/POTRF — actually
    # the last op in topological order is the final operation that completes
    # the factorization).
    fl_lines.append(f"output out, {node_var(len(ops) - 1)}")

    path = os.path.join(out_dir, "attn.fl")  # name kept for runner compatibility
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


# Updated C template: super body knows op_idx is the LAST input. We rely on
# the Trebuchet runtime passing the actual arity-matched in[] array. The
# super body reads in[arity-1]. We hardcode arity per op_type via separate
# super functions... no, that's not feasible either.
#
# Actual workable solution: encode op_idx as a GLOBAL counter incremented by
# each super invocation. Each super reads the global counter as its op_idx.
# But ordering isn't guaranteed (parallel firing).
#
# CORRECT solution: each super takes its DEPS as in[0..n-2] and op_idx as
# in[n-1]. The Trebuchet runtime's super dispatcher knows n (the assembler
# encodes it). The body can be passed n via a separate arg.
#
# Hmm. Looking at the existing s_phaseA / s_phaseB calls in attention,
# the body just reads `in[1]` knowing the convention is "in[0]=trigger,
# in[1]=block_idx". For Cholesky with variable dep count, we'd need to
# pass arity somehow.
#
# WORKAROUND: emit ONE super per op (each op_id is a unique super name).
# Body for op_X is hardcoded with op_X's parameters. Very explicit.
# But that means N_OPS super entries in the .fl — at NB=50 = 22k entries.
# Each entry has its own .c function. Source bloat.
#
# Alternative WORKAROUND: use a SINGLE global "op_counter" advanced by
# init(). Each op invocation uses the global counter as op_idx. But this
# requires the runtime to fire ops in order, which we lose if firing
# in parallel.
#
# REAL solution: pass op_idx as a GLOBAL LOOKUP. The .fl emits each op
# super with the dep sync inputs (no const). The super body uses a
# per-thread-local op_idx state? No, too complex.
#
# Let me revisit: what if I emit each op with a FIXED arity? Specifically:
#  - Each op super takes EXACTLY 2 sync inputs + 1 const (op_idx). 3 total.
#  - For ops with 1 dep: pad with same dep (in[0] = in[1]).
#  - For ops with 3 deps: chain them via intermediate barrier (or just allow
#    arity=4 only for those ops).
#
# Variable arity at runtime IS supported by Trebuchet (attention's barrier
# has K-way fan-in). The trick is the body knows which index is the const.
#
# CONVENTION I'll use: const op_idx is ALWAYS in[0] (first). Sync deps follow
# as in[1..]. Body just reads in[0].

# Now I realize I shouldn't have put it last. Let me put const FIRST.
# This is cleaner: in[0] = op_idx always.

# REGENERATING the .fl emit with const FIRST:

C_TEMPLATE_FINAL = r"""/* Auto-generated: Ribault dense block Cholesky C supers. */
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define NB        __NB__
#define B         __B__
#define N_OPS     __N_OPS__

static const char *DATA_DIR = "__DATA_DIR__";

static double *A;
static int *op_ti, *op_tj, *op_s1i, *op_s1j, *op_s2i, *op_s2j;

static void *xmalloc(size_t n) { void *p = malloc(n); if (!p) { fprintf(stderr, "OOM %zu\n", n); exit(1); } return p; }
static inline int block_idx(int i, int j) { return i * (i + 1) / 2 + j; }
static inline double *block_ptr(int i, int j) { return A + (size_t)block_idx(i, j) * B * B; }

static void load_dag(void) {
    char path[1024]; snprintf(path, sizeof path, "%s/dag.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    int n_ops;
    if (fread(&n_ops, sizeof(int), 1, f) != 1) { fprintf(stderr, "short read dag.bin hdr\n"); exit(1); }
    if (n_ops != N_OPS) { fprintf(stderr, "dag.bin n_ops mismatch %d vs %d\n", n_ops, N_OPS); exit(1); }
    op_ti  = xmalloc(N_OPS * sizeof(int));
    op_tj  = xmalloc(N_OPS * sizeof(int));
    op_s1i = xmalloc(N_OPS * sizeof(int));
    op_s1j = xmalloc(N_OPS * sizeof(int));
    op_s2i = xmalloc(N_OPS * sizeof(int));
    op_s2j = xmalloc(N_OPS * sizeof(int));
    for (int i = 0; i < N_OPS; i++) {
        int hdr[9];
        if (fread(hdr, sizeof(int), 9, f) != 9) { fprintf(stderr, "short read op %d\n", i); exit(1); }
        op_ti[i]  = hdr[1]; op_tj[i]  = hdr[2];
        op_s1i[i] = hdr[3]; op_s1j[i] = hdr[4];
        op_s2i[i] = hdr[5]; op_s2j[i] = hdr[6];
        int n_deps = hdr[8];
        if (n_deps > 0) {
            int skip[128];
            if (fread(skip, sizeof(int), n_deps, f) != (size_t)n_deps) { fprintf(stderr, "short read\n"); exit(1); }
        }
    }
    fclose(f);
}

static int64_t s_init(void) {
    int n_blocks = NB * (NB + 1) / 2;
    A = xmalloc((size_t)n_blocks * B * B * sizeof(double));
    char path[1024]; snprintf(path, sizeof path, "%s/A.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    if (fread(A, sizeof(double), (size_t)n_blocks * B * B, f) != (size_t)n_blocks * B * B) {
        fprintf(stderr, "short read A.bin\n"); exit(1);
    }
    fclose(f);
    load_dag();
    return 0;
}

static inline void potrf_block(double *D) {
    for (int j = 0; j < B; j++) {
        double s = D[j*B + j];
        for (int kk = 0; kk < j; kk++) s -= D[j*B + kk] * D[j*B + kk];
        D[j*B + j] = sqrt(s);
        double inv = 1.0 / D[j*B + j];
        for (int i = j + 1; i < B; i++) {
            double t = D[i*B + j];
            for (int kk = 0; kk < j; kk++) t -= D[i*B + kk] * D[j*B + kk];
            D[i*B + j] = t * inv;
        }
    }
    for (int i = 0; i < B; i++)
        for (int j = i + 1; j < B; j++)
            D[i*B + j] = 0.0;
}

static inline void trsm_block(double *X, const double *L) {
    for (int i = 0; i < B; i++)
        for (int j = 0; j < B; j++) {
            double s = X[i*B + j];
            for (int kk = 0; kk < j; kk++) s -= X[i*B + kk] * L[j*B + kk];
            X[i*B + j] = s / L[j*B + j];
        }
}

static inline void syrk_block(double *C, const double *A_) {
    for (int i = 0; i < B; i++)
        for (int j = 0; j <= i; j++) {
            double s = 0.0;
            for (int kk = 0; kk < B; kk++) s += A_[i*B + kk] * A_[j*B + kk];
            C[i*B + j] -= s;
        }
}

static inline void gemm_block(double *C, const double *A_, const double *B_) {
    for (int i = 0; i < B; i++)
        for (int j = 0; j < B; j++) {
            double s = 0.0;
            for (int kk = 0; kk < B; kk++) s += A_[i*B + kk] * B_[j*B + kk];
            C[i*B + j] -= s;
        }
}

static int64_t s_potrf(int64_t op_idx) {
    int idx = (int)op_idx;
    potrf_block(block_ptr(op_ti[idx], op_tj[idx]));
    return 0;
}
static int64_t s_trsm(int64_t op_idx) {
    int idx = (int)op_idx;
    trsm_block(block_ptr(op_ti[idx], op_tj[idx]), block_ptr(op_s1i[idx], op_s1j[idx]));
    return 0;
}
static int64_t s_syrk(int64_t op_idx) {
    int idx = (int)op_idx;
    syrk_block(block_ptr(op_ti[idx], op_tj[idx]), block_ptr(op_s1i[idx], op_s1j[idx]));
    return 0;
}
static int64_t s_gemm(int64_t op_idx) {
    int idx = (int)op_idx;
    gemm_block(block_ptr(op_ti[idx], op_tj[idx]),
               block_ptr(op_s1i[idx], op_s1j[idx]),
               block_ptr(op_s2i[idx], op_s2j[idx]));
    return 0;
}

static int64_t s_output(void) {
    int n_blocks = NB * (NB + 1) / 2;
    uint64_t cs = 0;
    for (size_t k = 0; k < (size_t)n_blocks * B * B; k++) {
        int64_t fixed = (int64_t)(A[k] * 1e6);
        cs = (cs + (uint64_t)((uint32_t)fixed)) & 0xFFFFFFFFu;
    }
    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    fflush(stdout);
    return (int64_t)cs;
}

/* Trebuchet bindings. Convention: in[0] = op_idx const; in[1..] = sync deps. */
void s__SUPER_INIT__(int64_t *in, int64_t *out)   { (void)in; out[0] = s_init(); }
void s__SUPER_POTRF__(int64_t *in, int64_t *out)  { out[0] = s_potrf(in[0]); }
void s__SUPER_TRSM__(int64_t *in, int64_t *out)   { out[0] = s_trsm(in[0]); }
void s__SUPER_SYRK__(int64_t *in, int64_t *out)   { out[0] = s_syrk(in[0]); }
void s__SUPER_GEMM__(int64_t *in, int64_t *out)   { out[0] = s_gemm(in[0]); }
void s__SUPER_RESULT__(int64_t *in, int64_t *out) { (void)in; out[0] = s_output(); }

__attribute__((visibility("default"))) void supers_hs_init(void) { }
__attribute__((visibility("default"))) void supers_hs_exit(void) { }
__attribute__((visibility("default"))) void supers_hs_init_thread(void) { }
__attribute__((visibility("default"))) void supers_hs_thread_done(void) { }
"""


def emit_fl_v2(out_dir, ops):
    """Emits .fl with const FIRST input (in[0]), deps after."""
    fl_lines = [
        f"superinst('init',   {SUPER_INIT},   1, False, False)",
        f"superinst('potrf',  {SUPER_POTRF},  1, False, False)",
        f"superinst('trsm',   {SUPER_TRSM},   1, False, False)",
        f"superinst('syrk',   {SUPER_SYRK},   1, False, False)",
        f"superinst('gemm',   {SUPER_GEMM},   1, False, False)",
        f"superinst('output', {SUPER_OUTPUT}, 1, False, False)",
        "avgtime('potrf', 100)",
        "avgtime('trsm',  500)",
        "avgtime('syrk',  500)",
        "avgtime('gemm',  500)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    for op in ops:
        op_id   = op['id']
        op_name = OP_KIND_TO_NAME[op['kind']]
        dep_vars = [node_var(d) for d in op['deps']] if op['deps'] else ['ini']
        fl_lines.append(f"const k_{op_id}, {op_id}")
        # const FIRST, then deps
        fl_lines.append(f"{op_name} {node_var(op_id)}, k_{op_id}, " + ", ".join(dep_vars))
    fl_lines.append(f"output out, {node_var(len(ops) - 1)}")
    path = os.path.join(out_dir, "attn.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


def emit(out_dir, data_dir, NB, B):
    os.makedirs(out_dir, exist_ok=True)
    ops = read_dag(data_dir)
    emit_fl_v2(out_dir, ops)
    print(f"[gen_sc_c] wrote {out_dir}/attn.fl  (n_ops={len(ops)})")

    src = (C_TEMPLATE_FINAL
           .replace("__NB__", str(NB))
           .replace("__B__", str(B))
           .replace("__N_OPS__", str(len(ops)))
           .replace("__DATA_DIR__", data_dir)
           .replace("__SUPER_INIT__",   str(SUPER_INIT))
           .replace("__SUPER_POTRF__",  str(SUPER_POTRF))
           .replace("__SUPER_TRSM__",   str(SUPER_TRSM))
           .replace("__SUPER_SYRK__",   str(SUPER_SYRK))
           .replace("__SUPER_GEMM__",   str(SUPER_GEMM))
           .replace("__SUPER_RESULT__", str(SUPER_OUTPUT)))
    c_path = os.path.join(out_dir, "attn_c_supers.c")
    with open(c_path, "w") as f:
        f.write(src)
    print(f"[gen_sc_c] wrote {c_path}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            k, v = line.split()
            cfg[k] = int(v)
    emit(args.out_dir, os.path.abspath(args.data_dir), cfg["NB"], cfg["B"])


if __name__ == "__main__":
    main()
