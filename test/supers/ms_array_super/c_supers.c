/*
 * C supers for array-based parallel merge sort v2b.
 * sort_leaf receives [ptr, size] list since sub-array ptrs from DF arithmetic
 * don't have embedded size. sort_leaf copies to arr_alloc'd array.
 * merge_pair receives [lptr, rptr] — both are arr_alloc'd (from sort_leaf or
 * merge_pair), so ptr[-1] is always valid.
 *
 * Super index mapping (from codegen with verify_sorted):
 *   s4 = verify_sorted — input: arr_alloc'd ptr; output: 0 (prints 1/0)
 *   s5 = merge_pair    — input: C-list [lptr, rptr]; output: merged ptr
 *   s6 = sort_leaf     — input: C-list [ptr, size];  output: arr_alloc'd ptr
 *   s7 = init_super    — input: C-list [N];          output: raw ptr
 *
 * Array layout for arr_alloc'd: raw[0] = size, data at raw+1.
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Dummy GHC RTS symbols for SUPERS_FORCE_PAR=1 */
void hs_init_ghc(int *argc, char ***argv, void *rts) { (void)argc; (void)argv; (void)rts; }
void hs_init_thread(void) {}
void hs_exit_thread(void) {}
void supers_io_init(void) {}

/* df_list_cell_t layout */
typedef struct { int64_t head; int64_t tail; } cell_t;
static inline int64_t cell_head(int64_t h) { return ((cell_t *)h)->head; }
static inline int64_t cell_tail(int64_t h) { return ((cell_t *)h)->tail; }

/* ---------- s0: df_list_cons (built-in) ---------- */
/* Input: oper[0]=head, oper[1]=tail.  Output: pointer to cell. */
void s0(int64_t *in, int64_t *out) {
    cell_t *c = (cell_t *)malloc(sizeof(cell_t));
    c->head = in[0];
    c->tail = in[1];
    out[0] = (int64_t)c;
}

/* Allocate array with embedded size: raw[0]=size, data at raw+1. Returns &raw[1]. */
static inline int64_t *arr_alloc(int64_t size) {
    int64_t *raw = (int64_t *)malloc((size + 1) * sizeof(int64_t));
    raw[0] = size;
    return raw + 1;
}
static inline int64_t arr_size(int64_t *ptr) { return ptr[-1]; }

/* ---------- merge sort helpers ---------- */

static void merge_impl(int64_t *arr, int64_t *tmp, int64_t lo, int64_t mid, int64_t hi) {
    int64_t i = lo, j = mid, k = lo;
    while (i < mid && j < hi) {
        if (arr[i] <= arr[j]) tmp[k++] = arr[i++];
        else                  tmp[k++] = arr[j++];
    }
    while (i < mid) tmp[k++] = arr[i++];
    while (j < hi)  tmp[k++] = arr[j++];
    memcpy(arr + lo, tmp + lo, (hi - lo) * sizeof(int64_t));
}

static void msort_rec(int64_t *arr, int64_t *tmp, int64_t lo, int64_t hi) {
    if (hi - lo <= 1) return;
    int64_t mid = lo + (hi - lo) / 2;
    msort_rec(arr, tmp, lo, mid);
    msort_rec(arr, tmp, mid, hi);
    merge_impl(arr, tmp, lo, mid, hi);
}

/* ---------- s7: init_super ---------- */
/* Input: C-list [N].  Output: raw ptr (no embedded size — DF graph knows size). */
void s7(int64_t *in, int64_t *out) {
    int64_t N = cell_head(in[0]);
    int64_t *arr = (int64_t *)malloc(N * sizeof(int64_t));
    for (int64_t i = 0; i < N; i++) arr[i] = N - i;
    out[0] = (int64_t)arr;
}

/* ---------- s6: sort_leaf ---------- */
/* Input: C-list [ptr, size].  Output: arr_alloc'd ptr (sorted copy). */
void s6(int64_t *in, int64_t *out) {
    int64_t h = in[0];
    int64_t *src = (int64_t *)cell_head(h);
    int64_t size = cell_head(cell_tail(h));
    int64_t *arr = arr_alloc(size);
    memcpy(arr, src, size * sizeof(int64_t));
    int64_t *tmp = (int64_t *)malloc(size * sizeof(int64_t));
    msort_rec(arr, tmp, 0, size);
    free(tmp);
    out[0] = (int64_t)arr;
}

/* ---------- s5: merge_pair ---------- */
/* Input: C-list [lptr, rptr].  Output: ptr to merged array (with embedded size). */
void s5(int64_t *in, int64_t *out) {
    int64_t h = in[0];
    int64_t *lptr = (int64_t *)cell_head(h);
    int64_t *rptr = (int64_t *)cell_head(cell_tail(h));
    int64_t lsize = arr_size(lptr);
    int64_t rsize = arr_size(rptr);
    int64_t total = lsize + rsize;
    int64_t *merged = arr_alloc(total);
    int64_t i = 0, j = 0, k = 0;
    while (i < lsize && j < rsize) {
        if (lptr[i] <= rptr[j]) merged[k++] = lptr[i++];
        else                    merged[k++] = rptr[j++];
    }
    while (i < lsize) merged[k++] = lptr[i++];
    while (j < rsize) merged[k++] = rptr[j++];
    out[0] = (int64_t)merged;
}

/* ---------- s4: verify_sorted ---------- */
/* Input: arr_alloc'd ptr.  Output: 0 (prints 1 if sorted, 0 if not). */
void s4(int64_t *in, int64_t *out) {
    int64_t *arr = (int64_t *)in[0];
    int64_t size = arr_size(arr);
    int sorted = 1;
    for (int64_t i = 1; i < size; i++) {
        if (arr[i] < arr[i - 1]) { sorted = 0; break; }
    }
    printf("%d\n", sorted);
    fflush(stdout);
    out[0] = 0;
}
