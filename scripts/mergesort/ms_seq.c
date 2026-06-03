/* Mergesort sequential baseline in C — monolithic top-down recursive
 * merge sort on a flat int32 array. Below CUTOFF: insertion sort.
 * Above: split, recurse, merge into scratch, copy back.
 *
 * Build:  gcc -O3 -march=native -o ms_seq_c ms_seq.c
 * Run:    ./ms_seq_c DATA_DIR
 * Output: CHECKSUM=<sum> RUNTIME_SEC=<float>
 *
 * The checksum is sum-mod-2^32 of the array. Sum is order-invariant so it
 * cross-validates against expected_checksum.txt regardless of how sorted
 * the result is — but we ALSO verify monotonicity at the end to catch
 * bugs where the algorithm just shuffles or returns the input.
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

static int N;
static int CUTOFF;

static int *arr;
static int *tmp;

static void load_config(const char *dir) {
    char path[1024]; snprintf(path, sizeof path, "%s/config.txt", dir);
    FILE *f = fopen(path, "r"); if (!f) { perror(path); exit(1); }
    char k[64]; int v;
    while (fscanf(f, "%63s %d", k, &v) == 2) {
        if      (!strcmp(k, "N"))      N      = v;
        else if (!strcmp(k, "CUTOFF")) CUTOFF = v;
    }
    fclose(f);
}

/* Insertion sort on arr[lo..hi). */
static inline void insertion_sort(int *a, int lo, int hi) {
    for (int i = lo + 1; i < hi; i++) {
        int x = a[i];
        int j = i - 1;
        while (j >= lo && a[j] > x) { a[j + 1] = a[j]; j--; }
        a[j + 1] = x;
    }
}

/* Merge a[lo..mid) and a[mid..hi) into t[lo..hi). */
static inline void merge_to(int *a, int lo, int mid, int hi, int *t) {
    int i = lo, j = mid, k = lo;
    while (i < mid && j < hi) {
        if (a[i] <= a[j]) t[k++] = a[i++];
        else              t[k++] = a[j++];
    }
    while (i < mid) t[k++] = a[i++];
    while (j < hi)  t[k++] = a[j++];
    memcpy(a + lo, t + lo, (size_t)(hi - lo) * sizeof(int));  /* copy back */
}

static void ms_sort(int *a, int lo, int hi) {
    if (hi - lo <= CUTOFF) { insertion_sort(a, lo, hi); return; }
    int mid = lo + (hi - lo) / 2;
    ms_sort(a, lo, mid);
    ms_sort(a, mid, hi);
    merge_to(a, lo, mid, hi, tmp);
}

int main(int argc, char **argv) {
    if (argc < 2) { fprintf(stderr, "usage: %s DATA_DIR\n", argv[0]); return 2; }
    const char *dir = argv[1];
    load_config(dir);

    arr = (int*)malloc((size_t)N * sizeof(int));
    tmp = (int*)malloc((size_t)N * sizeof(int));
    if (!arr || !tmp) { fprintf(stderr, "OOM\n"); return 1; }

    char inp[1024]; snprintf(inp, sizeof inp, "%s/input.bin", dir);
    FILE *f = fopen(inp, "rb"); if (!f) { perror(inp); return 1; }
    if (fread(arr, sizeof(int), N, f) != (size_t)N) {
        fprintf(stderr, "short read %s\n", inp); return 1;
    }
    fclose(f);

    /* Compute pre-sort checksum for sanity (must match post-sort). */
    uint64_t pre = 0;
    for (int i = 0; i < N; i++) pre = (pre + (uint64_t)(uint32_t)arr[i]) & 0xFFFFFFFFu;

    struct timespec t0, t1;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    ms_sort(arr, 0, N);
    clock_gettime(CLOCK_MONOTONIC, &t1);
    double secs = (double)(t1.tv_sec - t0.tv_sec) + (double)(t1.tv_nsec - t0.tv_nsec) / 1e9;

    /* Verify sorted and compute post-sort checksum. */
    uint64_t cs = 0;
    int ok = 1;
    for (int i = 0; i < N; i++) {
        if (i > 0 && arr[i] < arr[i - 1]) { ok = 0; }
        cs = (cs + (uint64_t)(uint32_t)arr[i]) & 0xFFFFFFFFu;
    }
    if (!ok) { fprintf(stderr, "FATAL: array not sorted\n"); return 1; }
    if (cs != pre) { fprintf(stderr, "FATAL: checksum changed (pre=%lu post=%lu)\n",
                              (unsigned long)pre, (unsigned long)cs); return 1; }

    printf("CHECKSUM=%lu\n", (unsigned long)cs);
    printf("RUNTIME_SEC=%f\n", secs);

    free(arr); free(tmp);
    return 0;
}
