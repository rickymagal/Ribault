/* CYK parsing baseline -- sequential C reference.
 *
 * Reads input.bin, grammar.bin, config.txt from <data-dir>.  Computes the
 * full CYK table d[i*N + j] (Word64 bitmask of non-terminals deriving
 * s[i..j]) and prints CHECKSUM=<hex of d[0][N-1]>  RUNTIME_SEC=<seconds>.
 *
 * Algorithm mirrored byte-for-byte by cyk_seq.rs and cyk_seq.hs, and by the
 * leaf body inside every parallel variant (Strategies / par-pseq / Timely /
 * Ribault Hs / C / Rust / Sucuri).  Fairness preservation rigorous.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

static int N;
static int N_NT, N_SIGMA;
static int N_PROD_BIN, N_PROD_TERM;

static int read_config(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) return -1;
    char key[64]; long val;
    while (fscanf(f, "%63s %ld", key, &val) == 2) {
        if (!strcmp(key, "N"))            N = (int)val;
        else if (!strcmp(key, "N_NT"))    N_NT = (int)val;
        else if (!strcmp(key, "N_SIGMA")) N_SIGMA = (int)val;
        else if (!strcmp(key, "N_PROD_BIN"))  N_PROD_BIN = (int)val;
        else if (!strcmp(key, "N_PROD_TERM")) N_PROD_TERM = (int)val;
    }
    fclose(f);
    return 0;
}

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s DATA_DIR\n", argv[0]); return 2; }
    char path[2048];
    snprintf(path, sizeof path, "%s/config.txt", argv[1]);
    if (read_config(path) < 0) { perror("config.txt"); return 1; }
    if (N_NT != 64) { fprintf(stderr, "N_NT must be 64 (Word64 bitmask); got %d\n", N_NT); return 1; }

    /* Load input.bin */
    snprintf(path, sizeof path, "%s/input.bin", argv[1]);
    FILE *fi = fopen(path, "rb");
    if (!fi) { perror(path); return 1; }
    unsigned char *s = malloc((size_t)N);
    if (fread(s, 1, (size_t)N, fi) != (size_t)N) { fprintf(stderr, "short input.bin\n"); return 1; }
    fclose(fi);

    /* Load grammar.bin: 4 u32 header + N_NT*N_NT u64 produce_bin + N_SIGMA u64 produce_term */
    snprintf(path, sizeof path, "%s/grammar.bin", argv[1]);
    FILE *fg = fopen(path, "rb");
    if (!fg) { perror(path); return 1; }
    uint32_t hdr[4];
    if (fread(hdr, sizeof hdr, 1, fg) != 1) { fprintf(stderr, "short grammar header\n"); return 1; }
    uint64_t *produce_bin = malloc((size_t)N_NT * N_NT * sizeof(uint64_t));
    if (fread(produce_bin, sizeof(uint64_t), (size_t)N_NT * N_NT, fg) != (size_t)N_NT * N_NT) {
        fprintf(stderr, "short produce_bin\n"); return 1;
    }
    uint64_t *produce_term = malloc((size_t)N_SIGMA * sizeof(uint64_t));
    if (fread(produce_term, sizeof(uint64_t), (size_t)N_SIGMA, fg) != (size_t)N_SIGMA) {
        fprintf(stderr, "short produce_term\n"); return 1;
    }
    fclose(fg);

    /* Allocate triangular table -- store full N*N for simple indexing. */
    uint64_t *d = calloc((size_t)N * (size_t)N, sizeof(uint64_t));
    if (!d) { perror("calloc d"); return 1; }

    /* Initialize base diagonal: d[i][i] = produce_term[s[i]] */
    for (int i = 0; i < N; i++) d[(size_t)i * N + i] = produce_term[s[i]];

    /* Main wavefront: span = 1..N-1, i = 0..N-1-span, j = i+span. */
    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    for (int span = 1; span < N; span++) {
        for (int i = 0; i < N - span; i++) {
            int j = i + span;
            uint64_t acc = 0;
            for (int k = i; k < j; k++) {
                uint64_t left  = d[(size_t)i * N + k];
                uint64_t right = d[(size_t)(k + 1) * N + j];
                if (left == 0 || right == 0) continue;
                /* For each B bit in left, for each C bit in right, acc |= produce[B*64+C] */
                uint64_t lb = left;
                while (lb) {
                    int B = __builtin_ctzll(lb);
                    lb &= lb - 1;
                    uint64_t rb = right;
                    while (rb) {
                        int C = __builtin_ctzll(rb);
                        rb &= rb - 1;
                        acc |= produce_bin[(size_t)B * N_NT + C];
                    }
                }
            }
            d[(size_t)i * N + j] = acc;
        }
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double secs = (double)(t1.tv_sec - t0.tv_sec) + (double)(t1.tv_nsec - t0.tv_nsec) / 1e9;

    uint64_t top = d[(size_t)0 * N + (N - 1)];
    printf("CHECKSUM=%016lX\n", (unsigned long)top);
    printf("RUNTIME_SEC=%.6f\n", secs);
    fflush(stdout);

    free(d); free(s); free(produce_bin); free(produce_term);
    return 0;
}
