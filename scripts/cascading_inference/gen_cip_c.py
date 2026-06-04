#!/usr/bin/env python3
"""Generate Ribault Cascading Inference Pipeline files with C-implemented supers.

The DAG emitted into the .fl is:
   - one super per (chunk, stage) — 4 supers per chunk
   - linear dependency within a chunk: stage1_k -> stage2_k -> stage3_k -> stage4_k
   - sync tree above the stage4 ops to fan-in n_chunks results into a single
     token before the output super (TALM has a 5-bit src-count limit so we
     can't have a single super with more than 31 inputs; we use a 31-wide
     tree of sync supers).
   - output super reads the global decisions[N] array and prints the checksum.

Super numbering (these are the only numbers that need to be stable; the
names are arbitrary but must match between superinst declarations and
super C functions):
   init     -> s10
   stage1   -> s11
   stage2   -> s12
   stage3   -> s13
   stage4   -> s14
   sync     -> s15
   output   -> s16
"""

import argparse, os, struct


SUPER_INIT   = 10
SUPER_STAGE1 = 11
SUPER_STAGE2 = 12
SUPER_STAGE3 = 13
SUPER_STAGE4 = 14
SUPER_SYNC   = 15
SUPER_OUTPUT = 16

MAX_FANIN    = 30   # 5-bit src-count limit in TALM


C_TEMPLATE = r"""/* Auto-generated: Ribault Cascading Inference Pipeline C supers. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define DIM_D       256
#define B2_SLOTS    256
#define E_DIM        64
#define K3            8
#define H_DIM       128
#define C_CLS        16
#define ACCEPT_BITMAP_BYTES 8192
#define N_CLASSES            4

#define ACCEPT_S1        ((int32_t)1)
#define REJECT_S2        ((int32_t)2)
#define ACCEPT_S3_BASE   ((int32_t)0x40)
#define CLASS_BASE       ((int32_t)0x80)

#define N           __N__
#define CHUNK_SIZE  __CHUNK_SIZE__

static const char *DATA_DIR = "__DATA_DIR__";

static uint8_t  *items;          /* [N * DIM_D] */
static int32_t  *decisions;      /* [N] */
static double   *emb_all;        /* [N * E_DIM] */
static uint8_t  *accept_bitmap_cls[N_CLASSES];   /* [N_CLASSES][ACCEPT_BITMAP_BYTES] */
static int32_t  *chunk_class;                    /* [n_chunks] loaded from chunk_class.bin */
static int16_t  *reject_weights; /* [B2_SLOTS] */
static double   *ref_vec;        /* [K3 * E_DIM] */
static double   *W1_mat;         /* [H_DIM * E_DIM] */
static double   *b1_vec;
static double   *W2_mat;         /* [C_CLS * H_DIM] */
static double   *b2_vec;
static double   *cos_table;      /* [E_DIM * DIM_D] */
static int32_t   T2_cls[N_CLASSES];
static double    T3_cls[N_CLASSES];

static void *xmalloc(size_t n) { void *p = malloc(n); if (!p) { fprintf(stderr,"OOM %zu\n",n); exit(1); } return p; }

static void load_config(void) {
    char path[1024]; snprintf(path, sizeof path, "%s/config.txt", DATA_DIR);
    FILE *f = fopen(path, "r"); if (!f) { perror(path); exit(1); }
    char k[64], v[64];
    while (fscanf(f, "%63s %63s", k, v) == 2) {
        if      (!strncmp(k, "T2_CLASS_", 9)) T2_cls[atoi(k + 9)] = (int32_t)atoll(v);
        else if (!strncmp(k, "T3_CLASS_", 9)) T3_cls[atoi(k + 9)] = strtod(v, NULL);
    }
    fclose(f);
}

static void load_weights(void) {
    char path[1024]; snprintf(path, sizeof path, "%s/weights.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    for (int c = 0; c < N_CLASSES; c++) accept_bitmap_cls[c] = xmalloc(ACCEPT_BITMAP_BYTES);
    reject_weights = xmalloc(B2_SLOTS * sizeof(int16_t));
    ref_vec        = xmalloc(K3 * E_DIM * sizeof(double));
    W1_mat         = xmalloc(H_DIM * E_DIM * sizeof(double));
    b1_vec         = xmalloc(H_DIM * sizeof(double));
    W2_mat         = xmalloc(C_CLS * H_DIM * sizeof(double));
    b2_vec         = xmalloc(C_CLS * sizeof(double));
    cos_table      = xmalloc(E_DIM * DIM_D * sizeof(double));
    for (int c = 0; c < N_CLASSES; c++) {
        if (fread(accept_bitmap_cls[c], 1, ACCEPT_BITMAP_BYTES, f) != ACCEPT_BITMAP_BYTES) goto bad;
    }
    if (fread(reject_weights, sizeof(int16_t),  B2_SLOTS,      f) != B2_SLOTS) goto bad;
    if (fread(ref_vec,        sizeof(double),   K3 * E_DIM,    f) != (size_t)(K3*E_DIM)) goto bad;
    if (fread(W1_mat,         sizeof(double),   H_DIM * E_DIM, f) != (size_t)(H_DIM*E_DIM)) goto bad;
    if (fread(b1_vec,         sizeof(double),   H_DIM,         f) != H_DIM) goto bad;
    if (fread(W2_mat,         sizeof(double),   C_CLS * H_DIM, f) != (size_t)(C_CLS*H_DIM)) goto bad;
    if (fread(b2_vec,         sizeof(double),   C_CLS,         f) != C_CLS) goto bad;
    if (fread(cos_table,      sizeof(double),   E_DIM * DIM_D, f) != (size_t)(E_DIM*DIM_D)) goto bad;
    fclose(f); return;
bad: fprintf(stderr, "short read weights.bin\n"); exit(1);
}

static void load_input(void) {
    char path[1024]; snprintf(path, sizeof path, "%s/input.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    items = xmalloc((size_t)N * DIM_D);
    if (fread(items, 1, (size_t)N * DIM_D, f) != (size_t)N * DIM_D) {
        fprintf(stderr, "short read input.bin\n"); exit(1);
    }
    fclose(f);
}

static void load_chunk_class(void) {
    char path[1024]; snprintf(path, sizeof path, "%s/chunk_class.bin", DATA_DIR);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    int n_chunks = (N + CHUNK_SIZE - 1) / CHUNK_SIZE;
    chunk_class = xmalloc(n_chunks * sizeof(int32_t));
    if (fread(chunk_class, sizeof(int32_t), n_chunks, f) != (size_t)n_chunks) {
        fprintf(stderr, "short read chunk_class.bin\n"); exit(1);
    }
    fclose(f);
}

static int64_t s_init(void) {
    load_config(); load_weights(); load_input(); load_chunk_class();
    decisions = xmalloc(N * sizeof(int32_t));
    memset(decisions, 0, N * sizeof(int32_t));
    emb_all = xmalloc((size_t)N * E_DIM * sizeof(double));
    return 0;
}


/* ---------- Stage kernels ---------- */

static inline int stage1_decide(const uint8_t *it, const uint8_t *bm) {
    uint32_t sig = 0;
    for (int i = 0; i < DIM_D; i++) sig += it[i];
    sig &= 0xFFFF;
    return (bm[sig >> 3] >> (sig & 7)) & 1;
}
static inline int32_t stage2_score(const uint8_t *it) {
    int32_t hist[B2_SLOTS] = {0};
    for (int i = 0; i < DIM_D - 1; i++) {
        int b = ((int)it[i] * 7 + (int)it[i+1]) & 0xFF;
        hist[b]++;
    }
    int32_t s = 0;
    for (int k = 0; k < B2_SLOTS; k++) s += hist[k] * (int32_t)reject_weights[k];
    return s;
}
static inline void stage3_embed(const uint8_t *it, double *emb) {
    for (int j = 0; j < E_DIM; j++) {
        double s = 0.0;
        const double *row = cos_table + (size_t)j * DIM_D;
        for (int i = 0; i < DIM_D; i++) s += row[i] * ((double)it[i] / 255.0);
        emb[j] = s;
    }
    double n2 = 0.0;
    for (int j = 0; j < E_DIM; j++) n2 += emb[j] * emb[j];
    if (n2 > 0.0) {
        double inv = 1.0 / sqrt(n2);
        for (int j = 0; j < E_DIM; j++) emb[j] *= inv;
    }
}
static inline int stage3_best(const double *emb, double *bs_out) {
    int best = 0; double bs = -1e300;
    for (int k = 0; k < K3; k++) {
        double s = 0.0;
        const double *r = ref_vec + (size_t)k * E_DIM;
        for (int j = 0; j < E_DIM; j++) s += emb[j] * r[j];
        if (s > bs) { bs = s; best = k; }
    }
    *bs_out = bs; return best;
}
static inline int stage4_classify(const double *emb) {
    double hidden[H_DIM];
    for (int h = 0; h < H_DIM; h++) {
        double s = b1_vec[h];
        const double *row = W1_mat + (size_t)h * E_DIM;
        for (int j = 0; j < E_DIM; j++) s += row[j] * emb[j];
        hidden[h] = s > 0.0 ? s : 0.0;
    }
    int best = 0; double bs = -1e300;
    for (int c = 0; c < C_CLS; c++) {
        double s = b2_vec[c];
        const double *row = W2_mat + (size_t)c * H_DIM;
        for (int h = 0; h < H_DIM; h++) s += row[h] * hidden[h];
        if (s > bs) { bs = s; best = c; }
    }
    return best;
}


/* ---------- Per-chunk per-stage supers ----------
 * Each super takes chunk_id as in[0] and operates over items[lo..hi)
 * (with lo = chunk_id * CHUNK_SIZE, hi = min(lo+CHUNK_SIZE, N)).
 * Other inputs in in[1..] are sync deps from previous stages. */

static int64_t s_stage1(int64_t chunk_id) {
    int k = (int)chunk_id;
    int cls = chunk_class[k];
    const uint8_t *bm = accept_bitmap_cls[cls];
    int lo = k * CHUNK_SIZE;
    int hi = lo + CHUNK_SIZE > N ? N : lo + CHUNK_SIZE;
    for (int i = lo; i < hi; i++) {
        if (stage1_decide(items + (size_t)i * DIM_D, bm)) decisions[i] = ACCEPT_S1;
    }
    return 0;
}
static int64_t s_stage2(int64_t chunk_id) {
    int k = (int)chunk_id;
    int cls = chunk_class[k];
    int32_t t2 = T2_cls[cls];
    int lo = k * CHUNK_SIZE;
    int hi = lo + CHUNK_SIZE > N ? N : lo + CHUNK_SIZE;
    for (int i = lo; i < hi; i++) {
        if (decisions[i] != 0) continue;
        int32_t s = stage2_score(items + (size_t)i * DIM_D);
        if (s > t2) decisions[i] = REJECT_S2;
    }
    return 0;
}
static int64_t s_stage3(int64_t chunk_id) {
    int k = (int)chunk_id;
    int cls = chunk_class[k];
    double t3 = T3_cls[cls];
    int lo = k * CHUNK_SIZE;
    int hi = lo + CHUNK_SIZE > N ? N : lo + CHUNK_SIZE;
    for (int i = lo; i < hi; i++) {
        if (decisions[i] != 0) continue;
        double *emb = emb_all + (size_t)i * E_DIM;
        stage3_embed(items + (size_t)i * DIM_D, emb);
        double bs; int best = stage3_best(emb, &bs);
        if (bs > t3) decisions[i] = ACCEPT_S3_BASE | best;
    }
    return 0;
}
static int64_t s_stage4(int64_t chunk_id) {
    int k = (int)chunk_id;
    int lo = k * CHUNK_SIZE;
    int hi = lo + CHUNK_SIZE > N ? N : lo + CHUNK_SIZE;
    for (int i = lo; i < hi; i++) {
        if (decisions[i] != 0) continue;
        double *emb = emb_all + (size_t)i * E_DIM;
        int cls = stage4_classify(emb);
        decisions[i] = CLASS_BASE | cls;
    }
    return 0;
}
static int64_t s_sync(void) { return 0; }
static int64_t s_output(void) {
    uint64_t cs = 0;
    for (int i = 0; i < N; i++) cs = (cs + (uint32_t)decisions[i]) & 0xFFFFFFFFu;
    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    fflush(stdout);
    return (int64_t)cs;
}


/* ---------- Trebuchet glue.  Convention: in[0] = const chunk_id (stage
 * supers) or unused (init / sync / output); in[1..] = sync deps. */
void s__SUPER_INIT__(int64_t *in, int64_t *out)   { (void)in; out[0] = s_init(); }
void s__SUPER_STAGE1__(int64_t *in, int64_t *out) { out[0] = s_stage1(in[0]); }
void s__SUPER_STAGE2__(int64_t *in, int64_t *out) { out[0] = s_stage2(in[0]); }
void s__SUPER_STAGE3__(int64_t *in, int64_t *out) { out[0] = s_stage3(in[0]); }
void s__SUPER_STAGE4__(int64_t *in, int64_t *out) { out[0] = s_stage4(in[0]); }
void s__SUPER_SYNC__(int64_t *in, int64_t *out)   { (void)in; out[0] = s_sync(); }
void s__SUPER_OUTPUT__(int64_t *in, int64_t *out) { (void)in; out[0] = s_output(); }

__attribute__((visibility("default"))) void supers_hs_init(void) {}
__attribute__((visibility("default"))) void supers_hs_exit(void) {}
__attribute__((visibility("default"))) void supers_hs_init_thread(void) {}
__attribute__((visibility("default"))) void supers_hs_thread_done(void) {}
"""


def emit_fl(out_dir, n_chunks):
    fl_lines = [
        f"superinst('init',     {SUPER_INIT},     1, False, False)",
        f"superinst('stage1',   {SUPER_STAGE1},   1, False, False)",
        f"superinst('stage2',   {SUPER_STAGE2},   1, False, False)",
        f"superinst('stage3',   {SUPER_STAGE3},   1, False, False)",
        f"superinst('stage4',   {SUPER_STAGE4},   1, False, False)",
        f"superinst('sync',     {SUPER_SYNC},     1, False, False)",
        f"superinst('output',   {SUPER_OUTPUT},   1, False, False)",
        "avgtime('stage1',  100)",
        "avgtime('stage2',  500)",
        "avgtime('stage3', 5000)",
        "avgtime('stage4', 5000)",
        "",
        "const c0, 0",
        "init ini, c0",
    ]
    # Stage ops per chunk.
    for k in range(n_chunks):
        fl_lines.append(f"const cid_{k}, {k}")
        fl_lines.append(f"stage1 s1_{k}, cid_{k}, ini")
        fl_lines.append(f"stage2 s2_{k}, cid_{k}, s1_{k}")
        fl_lines.append(f"stage3 s3_{k}, cid_{k}, s2_{k}")
        fl_lines.append(f"stage4 s4_{k}, cid_{k}, s3_{k}")
    # Sync tree fan-in (MAX_FANIN = 31).
    current = [f"s4_{k}" for k in range(n_chunks)]
    sync_id = 0
    if len(current) == 1:
        root_var = current[0]
    else:
        while len(current) > 1:
            next_level = []
            for i in range(0, len(current), MAX_FANIN):
                batch = current[i:i + MAX_FANIN]
                cname = f"k_sync_{sync_id}"
                sname = f"sync_{sync_id}"
                fl_lines.append(f"const {cname}, 0")
                fl_lines.append(f"sync {sname}, {cname}, " + ", ".join(batch))
                next_level.append(sname)
                sync_id += 1
            current = next_level
        root_var = current[0]
    fl_lines.append(f"output out, {root_var}")
    path = os.path.join(out_dir, "attn.fl")
    with open(path, "w") as f:
        f.write("\n".join(fl_lines) + "\n")
    return path


def emit(out_dir, data_dir, N, CHUNK_SIZE):
    os.makedirs(out_dir, exist_ok=True)
    n_chunks = (N + CHUNK_SIZE - 1) // CHUNK_SIZE
    emit_fl(out_dir, n_chunks)
    print(f"[gen_cip_c] wrote {out_dir}/attn.fl  (n_chunks={n_chunks})")

    src = (C_TEMPLATE
           .replace("__N__", str(N))
           .replace("__CHUNK_SIZE__", str(CHUNK_SIZE))
           .replace("__DATA_DIR__", data_dir)
           .replace("__SUPER_INIT__",   str(SUPER_INIT))
           .replace("__SUPER_STAGE1__", str(SUPER_STAGE1))
           .replace("__SUPER_STAGE2__", str(SUPER_STAGE2))
           .replace("__SUPER_STAGE3__", str(SUPER_STAGE3))
           .replace("__SUPER_STAGE4__", str(SUPER_STAGE4))
           .replace("__SUPER_SYNC__",   str(SUPER_SYNC))
           .replace("__SUPER_OUTPUT__", str(SUPER_OUTPUT)))
    cpath = os.path.join(out_dir, "attn_c_supers.c")
    with open(cpath, "w") as f:
        f.write(src)
    print(f"[gen_cip_c] wrote {cpath}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--data-dir", required=True)
    args = ap.parse_args()
    cfg = {}
    with open(os.path.join(args.data_dir, "config.txt")) as f:
        for line in f:
            ws = line.split()
            if len(ws) >= 2: cfg[ws[0]] = ws[1]
    emit(args.out_dir, os.path.abspath(args.data_dir),
         int(cfg["N"]), int(cfg["CHUNK_SIZE"]))


if __name__ == "__main__":
    main()
