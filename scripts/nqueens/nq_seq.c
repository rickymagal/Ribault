/* N-Queens sequential baseline in C — canonical mathematical reference.
 *
 * Reads `states.bin` (a list of `cutoff`-long valid queens prefixes
 * produced by gen_input.py) and, for each prefix, runs a recursive
 * sequential backtracking solver from row=cutoff down to row=N,
 * summing the count of complete solutions.  The total must match
 * the OEIS A000170 value Q(N).
 *
 * The fairness contract across the three sequential baselines:
 * (1) identical recursive algorithm, (2) `int queens[16]` stack
 * array (no malloc inside the inner solver), (3) `safe` checks
 * three conditions per previously-placed queen in a tight loop.
 * Build with gcc -O3 -march=native.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

#define MAX_N 16

static int N;
static int CUTOFF;
static int N_STATES;
static int32_t *states;   /* [N_STATES * CUTOFF] */

static void load_config(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/config.txt", dir);
    FILE *f = fopen(path, "r"); if (!f) { perror(path); exit(1); }
    char k[64], v[64];
    while (fscanf(f, "%63s %63s", k, v) == 2) {
        if      (!strcmp(k, "N"))        N        = atoi(v);
        else if (!strcmp(k, "CUTOFF"))   CUTOFF   = atoi(v);
        else if (!strcmp(k, "N_STATES")) N_STATES = atoi(v);
    }
    fclose(f);
}

static void load_states(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/states.bin", dir);
    FILE *f = fopen(path, "rb"); if (!f) { perror(path); exit(1); }
    int32_t hdr[2];
    if (fread(hdr, sizeof(int32_t), 2, f) != 2) { fprintf(stderr, "bad states.bin hdr\n"); exit(1); }
    if (hdr[0] != N_STATES || hdr[1] != CUTOFF) {
        fprintf(stderr, "states.bin header mismatch (%d,%d) vs config (%d,%d)\n",
                hdr[0], hdr[1], N_STATES, CUTOFF); exit(1);
    }
    states = malloc((size_t)N_STATES * CUTOFF * sizeof(int32_t));
    if (!states) { fprintf(stderr, "OOM\n"); exit(1); }
    if (fread(states, sizeof(int32_t), (size_t)N_STATES * CUTOFF, f) !=
        (size_t)N_STATES * CUTOFF) { fprintf(stderr, "bad states.bin\n"); exit(1); }
    fclose(f);
}


static inline int safe_q(const int *queens, int row, int col) {
    for (int r = 0; r < row; r++) {
        int c = queens[r];
        if (c == col)             return 0;
        if (c - r == col - row)   return 0;   /* / diagonal */
        if (c + r == col + row)   return 0;   /* \ diagonal */
    }
    return 1;
}

/* Solve the subtree rooted at `queens[0..row-1]` already placed; return
 * the count of complete solutions extending this prefix. */
static uint64_t solve_sub(int *queens, int row) {
    if (row == N) return 1ULL;
    uint64_t count = 0;
    for (int c = 0; c < N; c++) {
        if (safe_q(queens, row, c)) {
            queens[row] = c;
            count += solve_sub(queens, row + 1);
        }
    }
    return count;
}


int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s DATA_DIR\n", argv[0]); return 2; }
    load_config(argv[1]);
    load_states(argv[1]);

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);

    uint64_t total = 0;
    int queens[MAX_N];
    for (int s = 0; s < N_STATES; s++) {
        const int32_t *prefix = states + (size_t)s * CUTOFF;
        for (int r = 0; r < CUTOFF; r++) queens[r] = prefix[r];
        total += solve_sub(queens, CUTOFF);
    }

    clock_gettime(CLOCK_MONOTONIC, &t1);
    double secs = (double)(t1.tv_sec - t0.tv_sec)
                + (double)(t1.tv_nsec - t0.tv_nsec) * 1e-9;

    printf("CHECKSUM=%lu\n", (unsigned long)total);
    printf("RUNTIME_SEC=%.9f\n", secs);
    fflush(stdout);
    return 0;
}
