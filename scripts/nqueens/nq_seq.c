/* N-Queens sequential baseline in C — full recursion from row=0
 * inside this binary; no pre-expanded prefix list.  Canonical
 * reference: result must match OEIS A000170 Q(N).
 *
 * Fairness contract across the three sequential baselines: identical
 * recursive algorithm, `int queens[16]` stack array (no malloc in
 * the inner solver), `safe()` checks three conditions per
 * previously-placed queen in a tight loop.  Build with
 * gcc -O3 -march=native.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#define MAX_N 16

static int N;

static void load_config(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/config.txt", dir);
    FILE *f = fopen(path, "r"); if (!f) { perror(path); exit(1); }
    char k[64], v[64];
    while (fscanf(f, "%63s %63s", k, v) == 2) {
        if (!strcmp(k, "N")) N = atoi(v);
    }
    fclose(f);
}

static inline int safe_q(const int *queens, int row, int col) {
    for (int r = 0; r < row; r++) {
        int c = queens[r];
        if (c == col)             return 0;
        if (c - r == col - row)   return 0;
        if (c + r == col + row)   return 0;
    }
    return 1;
}

static uint64_t solve(int *queens, int row) {
    if (row == N) return 1ULL;
    uint64_t cnt = 0;
    for (int c = 0; c < N; c++) {
        if (safe_q(queens, row, c)) {
            queens[row] = c;
            cnt += solve(queens, row + 1);
        }
    }
    return cnt;
}


int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s DATA_DIR\n", argv[0]); return 2; }
    load_config(argv[1]);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    int queens[MAX_N];
    uint64_t total = solve(queens, 0);

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double secs = (double)(t1.tv_sec - t0.tv_sec)
                + (double)(t1.tv_nsec - t0.tv_nsec) * 1e-9;

    printf("CHECKSUM=%lu\n", (unsigned long)total);
    printf("RUNTIME_SEC=%.9f\n", secs);
    fflush(stdout);
    return 0;
}
