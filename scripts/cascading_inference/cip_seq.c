/* Cascading Inference Pipeline — canonical sequential baseline in C.
 *
 * Algorithm: see gen_input.py.  This file is the matheamtical reference;
 * cip_seq.rs and cip_seq.hs must produce the same per-item decision and
 * thus the same final checksum bit-for-bit on the same input.
 *
 * Fairness note: the parallel Rust variants (ribault_rust / cip_timely
 * / sucuri) read these same kernels through `*mut`/`*const` raw pointers
 * with `unsafe` inner loops.  cip_seq.c is C so already pointer-native —
 * no extra effort needed here.  The fairness equivalent on the Rust side
 * lives in cip_seq.rs.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <time.h>

#define D          256
#define B2_SLOTS   256
#define E_DIM       64
#define K3           8
#define H_DIM      128
#define C_CLS       16
#define ACCEPT_BITMAP_BYTES 8192
#define N_CLASSES            4

#define ACCEPT_S1        ((int32_t)1)
#define REJECT_S2        ((int32_t)2)
#define ACCEPT_S3_BASE   ((int32_t)0x40)
#define CLASS_BASE       ((int32_t)0x80)

static int     N;
static int     CHUNK_SIZE;
static int32_t T2_cls[N_CLASSES];
static double  T3_cls[N_CLASSES];

static uint8_t  *accept_bitmap_cls[N_CLASSES];  /* [N_CLASSES][ACCEPT_BITMAP_BYTES] */
static int32_t  *chunk_class;                   /* [n_chunks] */
static int       n_chunks;
static int16_t  *reject_weights; /* [B2_SLOTS] */
static double   *ref_vec;        /* [K3 * E_DIM] */
static double   *W1_mat;         /* [H_DIM * E_DIM] */
static double   *b1_vec;         /* [H_DIM] */
static double   *W2_mat;         /* [C_CLS * H_DIM] */
static double   *b2_vec;         /* [C_CLS] */
static double   *cos_table;      /* [E_DIM * D] */
static uint8_t  *items;          /* [N * D] */

static void *xmalloc(size_t n) {
    void *p = malloc(n);
    if (!p) { fprintf(stderr, "OOM %zu\n", n); exit(1); }
    return p;
}

static void load_config(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/config.txt", dir);
    FILE *f = fopen(path, "r"); if (!f) { perror(path); exit(1); }
    char k[64]; char v[64];
    while (fscanf(f, "%63s %63s", k, v) == 2) {
        if      (!strcmp(k, "N"))           N          = atoi(v);
        else if (!strcmp(k, "CHUNK_SIZE"))  CHUNK_SIZE = atoi(v);
        else if (!strncmp(k, "T2_CLASS_", 9)) T2_cls[atoi(k + 9)] = (int32_t)atoll(v);
        else if (!strncmp(k, "T3_CLASS_", 9)) T3_cls[atoi(k + 9)] = strtod(v, NULL);
    }
    fclose(f);
    n_chunks = (N + CHUNK_SIZE - 1) / CHUNK_SIZE;
}

static void load_weights(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/weights.bin", dir);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    for (int c = 0; c < N_CLASSES; c++) accept_bitmap_cls[c] = xmalloc(ACCEPT_BITMAP_BYTES);
    reject_weights = xmalloc(B2_SLOTS * sizeof(int16_t));
    ref_vec        = xmalloc(K3 * E_DIM * sizeof(double));
    W1_mat         = xmalloc(H_DIM * E_DIM * sizeof(double));
    b1_vec         = xmalloc(H_DIM * sizeof(double));
    W2_mat         = xmalloc(C_CLS * H_DIM * sizeof(double));
    b2_vec         = xmalloc(C_CLS * sizeof(double));
    cos_table      = xmalloc(E_DIM * D * sizeof(double));

    for (int c = 0; c < N_CLASSES; c++) {
        if (fread(accept_bitmap_cls[c], 1, ACCEPT_BITMAP_BYTES, f) != (size_t)ACCEPT_BITMAP_BYTES) goto bad;
    }
    if (fread(reject_weights, sizeof(int16_t),  B2_SLOTS,      f) != (size_t)B2_SLOTS) goto bad;
    if (fread(ref_vec,        sizeof(double),   K3 * E_DIM,    f) != (size_t)(K3 * E_DIM)) goto bad;
    if (fread(W1_mat,         sizeof(double),   H_DIM * E_DIM, f) != (size_t)(H_DIM * E_DIM)) goto bad;
    if (fread(b1_vec,         sizeof(double),   H_DIM,         f) != (size_t)H_DIM) goto bad;
    if (fread(W2_mat,         sizeof(double),   C_CLS * H_DIM, f) != (size_t)(C_CLS * H_DIM)) goto bad;
    if (fread(b2_vec,         sizeof(double),   C_CLS,         f) != (size_t)C_CLS) goto bad;
    if (fread(cos_table,      sizeof(double),   E_DIM * D,     f) != (size_t)(E_DIM * D)) goto bad;
    fclose(f);
    return;
bad:
    fprintf(stderr, "short read on weights.bin\n");
    exit(1);
}

static void load_input(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/input.bin", dir);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    items = xmalloc((size_t)N * D);
    if (fread(items, 1, (size_t)N * D, f) != (size_t)N * D) {
        fprintf(stderr, "short read on input.bin\n"); exit(1);
    }
    fclose(f);
}


/* ---------- Stage kernels ---------- */

static inline int stage1_decide(const uint8_t *it, const uint8_t *bm) {
    uint32_t sig = 0;
    for (int i = 0; i < D; i++) sig += it[i];
    sig &= 0xFFFF;
    return (bm[sig >> 3] >> (sig & 7)) & 1;
}

static inline int32_t stage2_score(const uint8_t *it) {
    int32_t hist[B2_SLOTS] = {0};
    for (int i = 0; i < D - 1; i++) {
        int b = ((int)it[i] * 7 + (int)it[i + 1]) & 0xFF;
        hist[b]++;
    }
    int32_t s = 0;
    for (int k = 0; k < B2_SLOTS; k++) s += hist[k] * (int32_t)reject_weights[k];
    return s;
}

static inline void stage3_embed(const uint8_t *it, double *emb_out) {
    for (int j = 0; j < E_DIM; j++) {
        double s = 0.0;
        const double *row = cos_table + (size_t)j * D;
        for (int i = 0; i < D; i++) s += row[i] * ((double)it[i] / 255.0);
        emb_out[j] = s;
    }
    double n2 = 0.0;
    for (int j = 0; j < E_DIM; j++) n2 += emb_out[j] * emb_out[j];
    if (n2 > 0.0) {
        double inv = 1.0 / sqrt(n2);
        for (int j = 0; j < E_DIM; j++) emb_out[j] *= inv;
    }
}

static inline int stage3_best(const double *emb, double *best_sim_out) {
    int best = 0;
    double bs = -1e300;
    for (int k = 0; k < K3; k++) {
        double s = 0.0;
        const double *r = ref_vec + (size_t)k * E_DIM;
        for (int j = 0; j < E_DIM; j++) s += emb[j] * r[j];
        if (s > bs) { bs = s; best = k; }
    }
    *best_sim_out = bs;
    return best;
}

static inline int stage4_classify(const double *emb) {
    double hidden[H_DIM];
    for (int h = 0; h < H_DIM; h++) {
        double s = b1_vec[h];
        const double *row = W1_mat + (size_t)h * E_DIM;
        for (int j = 0; j < E_DIM; j++) s += row[j] * emb[j];
        hidden[h] = s > 0.0 ? s : 0.0;
    }
    double out[C_CLS];
    int best = 0; double bs = -1e300;
    for (int c = 0; c < C_CLS; c++) {
        double s = b2_vec[c];
        const double *row = W2_mat + (size_t)c * H_DIM;
        for (int h = 0; h < H_DIM; h++) s += row[h] * hidden[h];
        out[c] = s;
        if (s > bs) { bs = s; best = c; }
    }
    (void)out;
    return best;
}


/* ---------- Per-item decision (matches gen_input.py exactly) ---------- */

static inline int32_t decide_item(const uint8_t *it, int class_id) {
    const uint8_t *bm = accept_bitmap_cls[class_id];
    int32_t t2 = T2_cls[class_id];
    double  t3 = T3_cls[class_id];
    if (stage1_decide(it, bm)) return ACCEPT_S1;
    int32_t score = stage2_score(it);
    if (score > t2) return REJECT_S2;
    double emb[E_DIM];
    stage3_embed(it, emb);
    double best_sim;
    int best = stage3_best(emb, &best_sim);
    if (best_sim > t3) return ACCEPT_S3_BASE | best;
    int cls = stage4_classify(emb);
    return CLASS_BASE | cls;
}


static void load_chunk_class(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/chunk_class.bin", dir);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    chunk_class = xmalloc(n_chunks * sizeof(int32_t));
    if (fread(chunk_class, sizeof(int32_t), n_chunks, f) != (size_t)n_chunks) {
        fprintf(stderr, "short read chunk_class.bin\n"); exit(1);
    }
    fclose(f);
}


int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s DATA_DIR\n", argv[0]); return 2; }
    const char *dir = argv[1];
    load_config(dir);
    load_weights(dir);
    load_input(dir);
    load_chunk_class(dir);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    uint32_t cs = 0;
    for (int chunk_id = 0; chunk_id < n_chunks; chunk_id++) {
        int class_id = chunk_class[chunk_id];
        int lo = chunk_id * CHUNK_SIZE;
        int hi = lo + CHUNK_SIZE > N ? N : lo + CHUNK_SIZE;
        for (int idx = lo; idx < hi; idx++) {
            int32_t d = decide_item(items + (size_t)idx * D, class_id);
            cs = (cs + (uint32_t)d) & 0xFFFFFFFFu;
        }
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double secs = (double)(t1.tv_sec - t0.tv_sec)
                + (double)(t1.tv_nsec - t0.tv_nsec) * 1e-9;

    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    printf("RUNTIME_SEC=%.9f\n", secs);
    fflush(stdout);
    return 0;
}
