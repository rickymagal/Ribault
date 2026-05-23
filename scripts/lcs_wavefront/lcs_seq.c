/* LCS sequential baseline in C — monolithic 2-row DP, identical structure
 * to gen_hs_sequential.py (Data.Vector.Unboxed.Mutable) but in pure C
 * without bounds checking. Reads N from argv; generates sequences A, B
 * from the same LCG (seed=42, alpha=4) used by every Haskell variant.
 *
 * Build:  gcc -O3 -march=native -o lcs_seq_c lcs_seq.c
 * Run:    ./lcs_seq_c N
 * Prints: RESULT=<lcs_score>\nRUNTIME_SEC=<wall_seconds>
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

#define SEED 42ULL
#define ALPHA 4

static inline uint64_t lcg_next(uint64_t r) {
    return (6364136223846793005ULL * r + 1442695040888963407ULL) & 0x7FFFFFFFFFFFFFFFULL;
}

static uint64_t gen_seq(uint64_t rng, int *arr, int len) {
    for (int i = 0; i < len; i++) {
        rng = lcg_next(rng);
        arr[i] = (int)((rng >> 33) % ALPHA);
    }
    return rng;
}

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s N\n", argv[0]); return 2; }
    int N = atoi(argv[1]);
    if (N <= 0) { fprintf(stderr, "bad N\n"); return 2; }

    int *sa = (int*)malloc((size_t)N * sizeof(int));
    int *sb = (int*)malloc((size_t)N * sizeof(int));
    int *prev = (int*)calloc((size_t)N + 1, sizeof(int));
    int *cur  = (int*)calloc((size_t)N + 1, sizeof(int));
    if (!sa || !sb || !prev || !cur) { fprintf(stderr, "OOM\n"); return 1; }

    uint64_t rng = SEED;
    rng = gen_seq(rng, sa, N);
    rng = gen_seq(rng, sb, N);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    for (int i = 1; i <= N; i++) {
        cur[0] = 0;
        int ai = sa[i - 1];
        for (int j = 1; j <= N; j++) {
            int bj = sb[j - 1];
            if (ai == bj) {
                cur[j] = prev[j - 1] + 1;
            } else {
                int u = prev[j];
                int l = cur[j - 1];
                cur[j] = u > l ? u : l;
            }
        }
        int *tmp = prev; prev = cur; cur = tmp;
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double secs = (double)(t1.tv_sec - t0.tv_sec) + (double)(t1.tv_nsec - t0.tv_nsec) / 1e9;

    int result = prev[N];
    printf("RESULT=%d\n", result);
    printf("RUNTIME_SEC=%f\n", secs);

    free(sa); free(sb); free(prev); free(cur);
    return 0;
}
