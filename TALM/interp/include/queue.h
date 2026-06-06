/*
 * Trebuchet - A multithreaded implementation of TALM.
 *
 *
 * File:
 *     queue.h
 *
 * Authors:
 *     Tiago A.O.A. <tiagoaoa@cos.ufrj.br>, Leandro J. Marzulo <lmazrulo@cos.ufrj.br>
 *
 *
 *
 *             THIS IS NOT AN OFFICIAL RELEASE.
 *             DO NOT COPY OR REDISTRIBUTE.
 *
 *
 */


//#define MAX_ELEMENTS 200

/* INITIAL_QUEUE_SIZE is used for both the (growable) queue_t and the
 * (fixed-size) deque_t.  The deque has no realloc path -- push_last
 * aborts the process with "Deque is full." if count == allocsize --
 * so this value is effectively the per-PE token capacity.  Bumped
 * from 50000 to 1000000 to support workloads with deeper TALM
 * decomposition (e.g. N-Queens CUTOFF=3 at N>=13 generated ~30k+
 * in-flight tokens and was hitting the limit).  Cost: each PE
 * allocates 8 bytes * 1M = 8 MB; with P=48 that's ~400 MB total --
 * trivial on the scherbius-class servers used for paper benchmarks.
 */
#ifndef INITIAL_QUEUE_SIZE
#define INITIAL_QUEUE_SIZE 1000000
#endif

#ifndef REALLOC_INCREMENT
#define REALLOC_INCREMENT 1000000
#endif
#define HAS_ELEMENTS(q) (q->count > 0)

#define DEQUE_RETURN_EMPTY (qelem) 0
#define DEQUE_RETURN_ABORT (qelem) 1

#include "cas.h"



typedef void * qelem;
//typedef int qelem;
typedef struct {
	int first;
	int last;

	int count;
	int allocsize;	
//	qelem elem[MAX_ELEMENTS];	
	qelem *elem;
} queue_t;



struct anchor_struct {
	cashalfword_t index;
	cashalfword_t tag;


};


typedef union {
	struct anchor_struct st;
	casword_t w;

} deque_anchor_t;


typedef struct {
	/* --- cache line 0: owner-written (pop_first writes first) --- */
	int first;
	int allocsize;
	qelem *elem;
	char _pad0[64 - sizeof(int) - sizeof(int) - sizeof(qelem *)];
	/* --- cache line 1: CAS-contended (push_last/pop_last CAS last) --- */
	deque_anchor_t last;
	char _pad1[64 - sizeof(deque_anchor_t)];
} deque_t;


void enqueue(qelem x, queue_t *q);

void init_queue(queue_t *q);

void init_deque(deque_t *d);

qelem get_first(queue_t *q);

void print_queue(queue_t q);

void push_last(qelem x, deque_t *d);

qelem pop_first(deque_t *d);

qelem pop_last(deque_t *d);


