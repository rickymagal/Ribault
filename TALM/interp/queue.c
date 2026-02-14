/*
 * Trebuchet - A multithreaded implementation of TALM.
 *
 *
 * File:
 *     queue.c
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


#include <stdio.h>
#include <stdlib.h>
#include "queue.h"


void expand_queue(queue_t *q);

void enqueue(qelem x, queue_t *q) {

	if (q->count == q->allocsize) {
#ifdef DEBUG_QUEUE
		printf("Queue is full. Reallocating\n");
#endif
		//q->elem = realloc(q->elem, (q->allocsize + REALLOC_INCREMENT)*sizeof(qelem));
		expand_queue(q);
	}



	q->last = (q->last + 1) % q->allocsize;
	q->elem[q->last] = x;
	(q->count)++;


}
void init_queue(queue_t *q) {
	q->first = 0;
	q->last = -1;
	q->count = 0;
	q->elem = (qelem *)malloc(sizeof(qelem) * INITIAL_QUEUE_SIZE);
	q->allocsize = INITIAL_QUEUE_SIZE;
}

void init_deque(deque_t *d) {
	d->first = 1;
	d->last.st.index = 0;
	d->last.st.tag = 0;
	d->elem = (qelem *)malloc(sizeof(qelem) * INITIAL_QUEUE_SIZE);
	d->allocsize = INITIAL_QUEUE_SIZE;
}


//int has_elements(queue_t *q) {
//	return (q->count > 0);

//}


qelem get_first(queue_t *q) {
	qelem first;
	if (q->count > 0) {
		(q->count)--;
		first = q->elem[q->first];
		q->first = (q->first + 1) % q->allocsize;


	} else
		first = NULL;

	return(first);

}
void expand_queue(queue_t *q) {
	int i, count_old = q->count;
	qelem *elem_old = q->elem;
	qelem *elem_new = (qelem *)malloc(sizeof(qelem) * (REALLOC_INCREMENT + q->allocsize));

	if (elem_new == NULL) {
		printf("Error expanding queue memory\n");
		exit(0);
	}
	//printf("Tamanho %d ", q->count);
	for (i=0; i<count_old; i++) {
		elem_new[i] = get_first(q);
	//	printf("%d, ", (int)elem_new[i]);
	}
#ifdef DEBUG_QUEUE
	printf("\n");
#endif
	q->first = 0;
	q->last = count_old-1;
	q->count = count_old;
	q->allocsize += REALLOC_INCREMENT;

	free(elem_old);

	q->elem = elem_new;


}



void push_last(qelem x, deque_t *d) {
	int count, retry = 1;
	deque_anchor_t last, last_new;
	/* Notice that deque_anchor_t is a union where deque.st is its interpretation as a struct anchor_struct
	 * and deque.w is its interpretion as a casword_t, to be used with CAS() */
	while (retry) {
		last = d->last;

		count = last.st.index - d->first;

		if (count == d->allocsize) {
			fprintf(stderr, "Deque is full. Aborting.\n");
			exit(1);
		}

		last_new.st.index = last.st.index + 1;
		last_new.st.tag = last.st.tag + 1;

		d->elem[last_new.st.index % d->allocsize] = x;

		retry = !(CAS(&(d->last), last.w, last_new.w)) ? retry + 1 : 0;
	}
}


qelem pop_first(deque_t *d) {
	int count, first;
	deque_anchor_t last, newlast;

	qelem output = NULL;

	first = d->first;
	d->first = first + 1;

	last = d->last;

	output = d->elem[first % d->allocsize];

	count = last.st.index - first;
	if (count < 0) {
		output = DEQUE_RETURN_EMPTY;
		d->first = first;

	} else {

		if (count > 0) {
			output = d->elem[first % d->allocsize];

		} else {
			/* Last element: CAS to prevent conflict with thief */
			newlast.st.index = 0;
			newlast.st.tag = last.st.tag + 1;
			if (CAS(&(d->last), last.w, newlast.w)) {
				/* Owner got the last element; reset deque indices */
				d->first = 1;
			} else {
				output = DEQUE_RETURN_ABORT;
				d->first = first;
			}
		}
	}

	return(output);
}



qelem pop_last(deque_t *d) {
	int first, count;
	deque_anchor_t last, newlast;
	qelem output;

	last = d->last;

	first = d->first;

	output = d->elem[last.st.index % d->allocsize];

	count = last.st.index - first;

	if (count < 0)
		output = DEQUE_RETURN_EMPTY;
	else {
		newlast.st.index = last.st.index - 1;
		newlast.st.tag = last.st.tag;

		if (!CAS(&(d->last), last.w, newlast.w))
			output = DEQUE_RETURN_ABORT;
	}

	return(output);
}
