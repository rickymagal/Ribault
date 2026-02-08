/*
 * Trebuchet - A multithreaded implementation of TALM.
 *
 *
 * File:
 *     interp.c
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
#define _GNU_SOURCE
#include <sched.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "queue.h"

#include "interp.h"	

#include "atomcount.h"

void * pe_main(void *args);

void initialize_threads(thread_args *args, int n_threads, FILE *fp, FILE *pla);

//void eval(instr_t *instr, oper_t **oper, thread_args  *pe_attr);
void eval(dispatch_t *disp, thread_args *pe_attr);
//oper_t * create_oper(void *value);

void treat_msgs(thread_args *pe_attr, int isblocking);
int got_waited_ops(oper_t **oper, int n_waits, dispatch_t *disp);
void add_wait_counter(wcounters_list_t *counters, int tstamp, int count, int n_threads);
void dec_wait_counter(wcounters_list_t *counters, int tstamp, int n_threads);
void spec_clean(thread_args *pe_attr, int tstamp);

void debug_oplists(thread_args *pe_attr);
oper_t ** get_oper(oper_t **oplist, int tag, int exectag);

void propagate_oper(instr_t *instr, oper_t result[], thread_args *pe_attr);
//void bypass_oper(oper_t **oplist, oper_t oper); 

void send_single_oper(instr_t *instr, oper_t result[], thread_args *pe_attr, int i);
void bypass_oper(opmatch_t **matchptr, int inport, oper_t *oper, thread_args *pe_attr, int match_tag_override, int match_exec_override);

void inter_pe_send(int pe_id, instr_t *target, int dstn, oper_t oper, thread_args *pe_attr);
//int can_exec(instr_t *instr);

dispatch_t * can_exec(instr_t *instr, int tag, int exectag, thread_args *pe_attr);


void init_combuff(combuff_t *comm);
void comm_send(combuff_t *comm, qelem elem);

qelem comm_recv(combuff_t *comm, int blocking);


int treat_marker(int tag, int *pmax_tag, int *pcount, thread_args *attr, int isidle);

void send_markers(int tag, int id, int n);

void send_dispatch(dispatch_t *disp, int pe_id, thread_args *pe_attr);
int get_notnull_spec(oper_t **oper, int len);

void dispandtx_cleanup(void *disptx, int islast); //callback function to cleanup operand
/* ----------------------------
	GLOBAL VARIABLES       */
barrier_t barrier;
combuff_t comm_buffer[MAX_THREADS];
void *libhandle;
static thread_args *g_args = NULL;
static int g_n_threads = 0;

typedef void (*hs_init_thread_fn)(void);
typedef void (*hs_exit_thread_fn)(void);
typedef void (*rts_lock_fn)(void);
typedef void (*hs_init_fn)(void);
static hs_init_thread_fn hs_init_thread_fp = NULL;
static hs_exit_thread_fn hs_exit_thread_fp = NULL;
static hs_init_fn supers_hs_init_fp = NULL;
static hs_init_fn supers_hs_exit_fp = NULL;
static rts_lock_fn rts_lock_fp = NULL;
static rts_lock_fn rts_unlock_fp = NULL;
static int use_rts_lock = 0;
static int use_supers_mutex = 0;
static int use_supers_worker = 0;
static int use_supers_worker_main = 0;
static pthread_mutex_t supers_mutex = PTHREAD_MUTEX_INITIALIZER;
static inline int is_list_super_op(int opcode) {
	int idx = opcode - OP_SUPER1;
	return (idx >= 0 && idx <= 3);
}

static inline int valtag_is_execonly_oper1(const oper_t *op) {
	return (op && op->value.i < 0);
}

opmatch_t *create_opmatch(int tag, int exec, thread_args *pe_attr);

static int valtag_execonly_bucket(instr_t *instr, int exec) {
	for (opmatch_t *m = instr->opmatch; m != NULL; m = m->next) {
		if (m->tag == 0 && m->exec == exec && m->op[1] && m->op[1]->value.i < 0) {
			return 1;
		}
	}
	return 0;
}

// For exec-only valtag: move any pending operand0 into tag=0 bucket for this exec.
static void valtag_promote_execonly_op0(instr_t *instr, int exec, thread_args *pe_attr) {
	opmatch_t **matchptr = &(instr->opmatch);
	opmatch_t *m0 = NULL, *m1 = NULL;

	for (opmatch_t *m = *matchptr; m != NULL; m = m->next) {
		if (m->exec != exec) continue;
		if (m->tag == 0) m1 = m;
		if (m->tag != 0 && m->op[0] != NULL && m0 == NULL) m0 = m;
	}
	if (!m0 || !m0->op[0]) return;

	if (!m1) {
		opmatch_t **pp = matchptr;
		while (*pp != NULL && ((0 > (*pp)->tag) || ((0 == (*pp)->tag) && (exec != (*pp)->exec)))) {
			pp = &((*pp)->next);
		}
		m1 = create_opmatch(0, exec, pe_attr);
		m1->next = *pp;
		*pp = m1;
	}
	if (!m1->op[0]) {
		m0->op[0]->tag = 0;
		m1->op[0] = m0->op[0];
		m1->count++;
		m0->op[0] = NULL;
		m0->count--;
	}
}
static int use_df_list_builtin = 0;
static size_t df_list_block_size = 4096;

typedef struct df_list_cell {
	int64_t head;
	int64_t tail;
} df_list_cell_t;

static __thread df_list_cell_t *df_list_block = NULL;
static __thread size_t df_list_block_used = 0;
static __thread int debug_match_target_id = -1;
static __thread int debug_match_target_op = -1;
static __thread int debug_trace_port = -1;
opmatch_t *create_opmatch(int tag, int exec, thread_args *pe_attr);
static int debug_trace_id(int id) {
	const char *env = getenv("DF_DEBUG_TRACE_ID");
	char buf[32];
	if (!env || !*env) return 0;
	snprintf(buf, sizeof(buf), ",%d,", id);
	if (strstr(env, buf)) return 1;
	return 0;
}
static int debug_trace_port_match(int id, int port) {
	const char *env_id = getenv("DF_DEBUG_TRACE_ID");
	const char *env_port = getenv("DF_DEBUG_TRACE_PORT");
	char buf[32];
	if (!env_id || !*env_id || !env_port || !*env_port) return 0;
	snprintf(buf, sizeof(buf), ",%d,", id);
	if (!strstr(env_id, buf)) return 0;
	return (port == atoi(env_port));
}

int64_t df_list_cons(int64_t head, int64_t tail) {
	if (!df_list_block || df_list_block_used >= df_list_block_size) {
		df_list_cell_t *next = (df_list_cell_t *)malloc(sizeof(*next) * df_list_block_size);
		if (!next) {
			fprintf(stderr, "Error: df_list_cons block malloc failed\n");
			exit(1);
		}
		df_list_block = next;
		df_list_block_used = 0;
	}
	df_list_cell_t *cell = &df_list_block[df_list_block_used++];
	cell->head = head;
	cell->tail = tail;
	return (int64_t)(uintptr_t)cell;
}


int64_t df_list_head(int64_t handle) {
	if (handle == 0) return 0;
	df_list_cell_t *cell = (df_list_cell_t *)(uintptr_t)handle;
	return cell->head;
}

int64_t df_list_tail(int64_t handle) {
	if (handle == 0) return 0;
	df_list_cell_t *cell = (df_list_cell_t *)(uintptr_t)handle;
	return cell->tail;
}

int64_t df_list_is_nil(int64_t handle) {
	return handle == 0 ? 1 : 0;
}

typedef enum {
	SUPERS_REQ_NSPEC = 0,
	SUPERS_REQ_SPEC = 1
} supers_req_kind_t;

typedef struct {
	supers_req_kind_t kind;
	cblockptr_t fptr;
	tm_tx_t *tx;
	oper_t **oper;
	oper_t *result;
} supers_req_t;

static pthread_t supers_worker_thread;
static pthread_mutex_t supers_req_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t supers_req_cv = PTHREAD_COND_INITIALIZER;
static supers_req_t *supers_req = NULL;
static int supers_worker_started = 0;
static int supers_stop = 0;
static int ignore_exec_match = 0;
static int ignore_tag_match = 0;
static volatile int pe_running = 0;
static cblockptr_t *super_fptrs = NULL;
static int super_fptrs_len = 0;
static pthread_mutex_t super_fptrs_mutex = PTHREAD_MUTEX_INITIALIZER;
static int check_msgs_interval = CHECK_MSGS_INTERVAL;
static int idle_spins = 0;
static int idle_yield = 0;
static int idle_spins_env_set = 0;
static int idle_yield_env_set = 0;
static long comm_wait_usec = -1;
static int comm_wait_env_set = 0;
static int force_spin = 0;
static int force_spin_env_set = 0;
static int steal_enabled = 1;
static int steal_batch = 1;
static int steal_batch_env_set = 0;
static int steal_threshold = 0;
static int steal_threshold_env_set = 0;
static int sched_prof = 0;
static uint64_t *sched_busy_iters = NULL;
static uint64_t *sched_idle_iters = NULL;
static uint64_t *sched_execs = NULL;
static int *sched_max_ready = NULL;
static __thread thread_args *tls_attr = NULL;

static inline void readyq_enqueue(thread_args *attr, qelem x) {
	if (steal_enabled) {
		pthread_mutex_lock(&attr->ready_mutex);
		enqueue(x, &attr->ready_queue);
		pthread_mutex_unlock(&attr->ready_mutex);
	} else {
		enqueue(x, &attr->ready_queue);
	}
}

static inline dispatch_t *readyq_dequeue(thread_args *attr, int *count_before) {
	dispatch_t *disp = NULL;
	if (steal_enabled) {
		pthread_mutex_lock(&attr->ready_mutex);
		if (count_before) {
			*count_before = attr->ready_queue.count;
		}
		if (attr->ready_queue.count > 0) {
			disp = (dispatch_t *)get_first(&attr->ready_queue);
		}
		pthread_mutex_unlock(&attr->ready_mutex);
	} else {
		if (count_before) {
			*count_before = attr->ready_queue.count;
		}
		if (attr->ready_queue.count > 0) {
			disp = (dispatch_t *)get_first(&attr->ready_queue);
		}
	}
	return disp;
}

static dispatch_t *steal_work(thread_args *attr) {
	if (!steal_enabled || !g_args || g_n_threads <= 1) {
		return NULL;
	}
	dispatch_t *stolen[32];
	int stolen_count = 0;
	int batch = steal_batch;
	if (batch < 1) {
		batch = 1;
	}
	if (batch > 32) {
		batch = 32;
	}
	int start = (attr->id + 1) % g_n_threads;
	for (int offset = 0; offset < g_n_threads - 1; offset++) {
		int vid = (start + offset) % g_n_threads;
		thread_args *victim = &g_args[vid];
		if (pthread_mutex_trylock(&victim->ready_mutex) != 0) {
			continue;
		}
		dispatch_t *disp = NULL;
		if (victim->ready_queue.count > 0) {
			disp = (dispatch_t *)get_first(&victim->ready_queue);
			int to_take = batch - 1;
			while (to_take > 0 && victim->ready_queue.count > 0 && stolen_count < 32) {
				stolen[stolen_count++] = (dispatch_t *)get_first(&victim->ready_queue);
				to_take--;
			}
		}
		pthread_mutex_unlock(&victim->ready_mutex);
		for (int i = 0; i < stolen_count; i++) {
			readyq_enqueue(attr, (qelem)stolen[i]);
		}
		stolen_count = 0;
		if (disp) {
			return disp;
		}
	}
	return NULL;
}

#define TOKEN_POOL_BATCH 256

static inline void optoken_pool_grow(thread_args *attr) {
	optoken_t *block = (optoken_t *)malloc(sizeof(optoken_t) * TOKEN_POOL_BATCH);
	if (!block) {
		fprintf(stderr, "Error allocating optoken pool\n");
		exit(1);
	}
	for (int i = 0; i < TOKEN_POOL_BATCH; i++) {
		block[i].next = attr->optoken_free;
		attr->optoken_free = &block[i];
	}
}

static inline optoken_t *optoken_alloc(thread_args *attr) {
	if (!attr->optoken_free) {
		optoken_pool_grow(attr);
	}
	optoken_t *tk = attr->optoken_free;
	attr->optoken_free = tk->next;
	return tk;
}

static inline void optoken_release(thread_args *attr, optoken_t *tk) {
	tk->next = attr->optoken_free;
	attr->optoken_free = tk;
}

static inline void dispsnd_pool_grow(thread_args *attr) {
	dispsnd_t *block = (dispsnd_t *)malloc(sizeof(dispsnd_t) * TOKEN_POOL_BATCH);
	if (!block) {
		fprintf(stderr, "Error allocating dispsnd pool\n");
		exit(1);
	}
	for (int i = 0; i < TOKEN_POOL_BATCH; i++) {
		block[i].next = attr->dispsnd_free;
		attr->dispsnd_free = &block[i];
	}
}

static inline dispsnd_t *dispsnd_alloc(thread_args *attr) {
	if (!attr->dispsnd_free) {
		dispsnd_pool_grow(attr);
	}
	dispsnd_t *msg = attr->dispsnd_free;
	attr->dispsnd_free = msg->next;
	return msg;
}

static inline void dispsnd_release(thread_args *attr, dispsnd_t *msg) {
	msg->next = attr->dispsnd_free;
	attr->dispsnd_free = msg;
}

static inline thread_args *pool_attr(thread_args *attr) {
	if (attr) {
		return attr;
	}
	return tls_attr;
}

static inline void oper_pool_grow(thread_args *attr) {
	oper_t *block = (oper_t *)malloc(sizeof(oper_t) * TOKEN_POOL_BATCH);
	if (!block) {
		fprintf(stderr, "Error allocating oper pool\n");
		exit(1);
	}
	for (int i = 0; i < TOKEN_POOL_BATCH; i++) {
		block[i].next = attr->oper_free;
		attr->oper_free = &block[i];
	}
}

static inline oper_t *oper_alloc(thread_args *attr) {
	thread_args *pool = pool_attr(attr);
	if (!pool) {
		return (oper_t *)malloc(sizeof(oper_t));
	}
	if (!pool->oper_free) {
		oper_pool_grow(pool);
	}
	oper_t *op = pool->oper_free;
	pool->oper_free = op->next;
	op->next = NULL;
	return op;
}

static inline void oper_release(thread_args *attr, oper_t *op) {
	thread_args *pool = pool_attr(attr);
	if (!pool) {
		free(op);
		return;
	}
	if (op->owner_token) {
		optoken_release(pool, (optoken_t *)op->owner_token);
		return;
	}
	op->next = pool->oper_free;
	pool->oper_free = op;
}

static inline void opmatch_pool_grow(thread_args *attr) {
	opmatch_t *block = (opmatch_t *)malloc(sizeof(opmatch_t) * TOKEN_POOL_BATCH);
	if (!block) {
		fprintf(stderr, "Error allocating opmatch pool\n");
		exit(1);
	}
	for (int i = 0; i < TOKEN_POOL_BATCH; i++) {
		block[i].next = attr->opmatch_free;
		attr->opmatch_free = &block[i];
	}
}

static inline opmatch_t *opmatch_alloc(thread_args *attr) {
	thread_args *pool = pool_attr(attr);
	if (!pool) {
		return (opmatch_t *)malloc(sizeof(opmatch_t));
	}
	if (!pool->opmatch_free) {
		opmatch_pool_grow(pool);
	}
	opmatch_t *match = pool->opmatch_free;
	pool->opmatch_free = match->next;
	match->next = NULL;
	return match;
}

static inline void opmatch_release(thread_args *attr, opmatch_t *match) {
	thread_args *pool = pool_attr(attr);
	if (!pool) {
		free(match);
		return;
	}
	match->next = pool->opmatch_free;
	pool->opmatch_free = match;
}

static inline void dispatch_pool_grow(thread_args *attr) {
	dispatch_t *block = (dispatch_t *)malloc(sizeof(dispatch_t) * TOKEN_POOL_BATCH);
	if (!block) {
		fprintf(stderr, "Error allocating dispatch pool\n");
		exit(1);
	}
	for (int i = 0; i < TOKEN_POOL_BATCH; i++) {
		block[i].next = attr->dispatch_free;
		attr->dispatch_free = &block[i];
	}
}

static inline dispatch_t *dispatch_alloc(thread_args *attr) {
	if (!attr->dispatch_free) {
		dispatch_pool_grow(attr);
	}
	dispatch_t *disp = attr->dispatch_free;
	attr->dispatch_free = disp->next;
	disp->next = NULL;
	return disp;
}

static inline void dispatch_release(thread_args *attr, dispatch_t *disp) {
	disp->next = attr->dispatch_free;
	attr->dispatch_free = disp;
}

static cblockptr_t get_super_fptr(int opcode) {
	int idx = opcode - OP_SUPER1;
	if (idx < 0) {
		cblockptr_t empty = {0};
		return empty;
	}
	pthread_mutex_lock(&super_fptrs_mutex);
	if (idx >= super_fptrs_len) {
		int new_len = idx + 1;
		cblockptr_t *next = (cblockptr_t *)realloc(super_fptrs, sizeof(*super_fptrs) * new_len);
		if (!next) {
			pthread_mutex_unlock(&super_fptrs_mutex);
			fprintf(stderr, "Error allocating super pointer cache\n");
			exit(1);
		}
		memset(next + super_fptrs_len, 0, sizeof(*super_fptrs) * (new_len - super_fptrs_len));
		super_fptrs = next;
		super_fptrs_len = new_len;
	}
	if (!super_fptrs[idx].nspec) {
		char function_name[64];
		snprintf(function_name, sizeof(function_name), "super%d", idx);
		super_fptrs[idx].nspec = dlsym(libhandle, function_name);
		if (!super_fptrs[idx].nspec) {
			pthread_mutex_unlock(&super_fptrs_mutex);
			fprintf(stderr, "%s\n", dlerror());
			exit(1);
		}
	}
	cblockptr_t fptr = super_fptrs[idx];
	pthread_mutex_unlock(&super_fptrs_mutex);
	return fptr;
}

static void setup_hs_thread_fns(void) {
	void *handle = RTLD_DEFAULT;
	const char *lock_env = getenv("SUPERS_RTS_LOCK");
	if (lock_env && lock_env[0] != '\0' && lock_env[0] != '0') {
		use_rts_lock = 1;
	}
	const char *serial_env = getenv("SUPERS_SERIAL");
	if (serial_env && serial_env[0] != '\0' && serial_env[0] != '0') {
		use_supers_mutex = 1;
	}
	const char *worker_env = getenv("SUPERS_WORKER");
	if (worker_env && worker_env[0] != '\0' && worker_env[0] != '0') {
		use_supers_worker = 1;
	}
	const char *worker_main_env = getenv("SUPERS_WORKER_MAIN");
	if (worker_main_env && worker_main_env[0] != '\0' && worker_main_env[0] != '0') {
		use_supers_worker_main = 1;
	}
	if (use_supers_worker_main) {
		use_supers_worker = 1;
	}
	if (libhandle) {
		handle = libhandle;
	}
	supers_hs_init_fp = (hs_init_fn)dlsym(handle, "supers_hs_init");
	supers_hs_exit_fp = (hs_init_fn)dlsym(handle, "supers_hs_exit");
	hs_init_thread_fp = (hs_init_thread_fn)dlsym(handle, "supers_hs_init_thread");
	hs_exit_thread_fp = (hs_exit_thread_fn)dlsym(handle, "supers_hs_thread_done");
	rts_lock_fp = (rts_lock_fn)dlsym(handle, "rts_lock");
	rts_unlock_fp = (rts_lock_fn)dlsym(handle, "rts_unlock");
	if (!rts_lock_fp) {
		rts_lock_fp = (rts_lock_fn)dlsym(RTLD_DEFAULT, "rts_lock");
	}
	if (!rts_unlock_fp) {
		rts_unlock_fp = (rts_lock_fn)dlsym(RTLD_DEFAULT, "rts_unlock");
	}
	if (!supers_hs_init_fp) {
		supers_hs_init_fp = (hs_init_fn)dlsym(RTLD_DEFAULT, "supers_hs_init");
	}
	if (!supers_hs_exit_fp) {
		supers_hs_exit_fp = (hs_init_fn)dlsym(RTLD_DEFAULT, "supers_hs_exit");
	}
	if (!hs_init_thread_fp) {
		hs_init_thread_fp = (hs_init_thread_fn)dlsym(RTLD_DEFAULT, "hs_init_thread");
	}
	if (!hs_exit_thread_fp) {
		hs_exit_thread_fp = (hs_exit_thread_fn)dlsym(RTLD_DEFAULT, "hs_thread_done");
	}
	// If threaded RTS is available, avoid the worker thread and use direct calls,
	// unless explicitly forced.
	const char *force_worker_env = getenv("SUPERS_FORCE_WORKER");
	int force_worker = (force_worker_env && force_worker_env[0] != '\0' && force_worker_env[0] != '0');
	if (hs_init_thread_fp && !force_worker) {
		use_supers_worker = 0;
		use_supers_worker_main = 0;
	}
}

static void *supers_worker_main(void *arg) {
	(void)arg;
	if (supers_hs_init_fp) {
		supers_hs_init_fp();
	}
	if (hs_init_thread_fp) {
		hs_init_thread_fp();
	}
	for (;;) {
		pthread_mutex_lock(&supers_req_mutex);
		if (getenv("SUPERS_DEBUG")) {
			fprintf(stderr, "[sup ] server loop ptr=%p stop=%d\n", (void *)supers_req, supers_stop);
		}
		while (!supers_req && !supers_stop) {
			pthread_cond_wait(&supers_req_cv, &supers_req_mutex);
		}
		if (supers_stop) {
			pthread_mutex_unlock(&supers_req_mutex);
			break;
		}
		supers_req_t *req = supers_req;
		pthread_mutex_unlock(&supers_req_mutex);
		if (getenv("SUPERS_DEBUG")) {
			fprintf(stderr, "[sup ] server handling kind=%d\n", (int)req->kind);
		}

		if (use_rts_lock && rts_lock_fp) {
			rts_lock_fp();
		}
		if (req->kind == SUPERS_REQ_SPEC) {
			req->fptr.spec(req->tx, req->oper, req->result);
		} else {
			req->fptr.nspec(req->oper, req->result);
		}
		if (use_rts_lock && rts_unlock_fp) {
			rts_unlock_fp();
		}

		pthread_mutex_lock(&supers_req_mutex);
		supers_req = NULL;
		pthread_cond_broadcast(&supers_req_cv);
		pthread_mutex_unlock(&supers_req_mutex);
	}
	if (hs_exit_thread_fp) {
		hs_exit_thread_fp();
	}
	if (supers_hs_exit_fp) {
		supers_hs_exit_fp();
	}
	return NULL;
}

static void supers_server_loop(void) {
	if (supers_hs_init_fp) {
		supers_hs_init_fp();
	}
	if (getenv("SUPERS_DEBUG")) {
		fprintf(stderr, "[sup ] server loop start\n");
	}
	for (;;) {
		pthread_mutex_lock(&supers_req_mutex);
		if (getenv("SUPERS_DEBUG")) {
			fprintf(stderr, "[sup ] server loop ptr=%p stop=%d\n", (void *)supers_req, supers_stop);
		}
		while (!supers_req && !supers_stop) {
			if (getenv("SUPERS_DEBUG")) {
				fprintf(stderr, "[sup ] server wait\n");
			}
			if (pe_running == 0) {
				supers_stop = 1;
				break;
			}
			pthread_cond_wait(&supers_req_cv, &supers_req_mutex);
			if (getenv("SUPERS_DEBUG")) {
				fprintf(stderr, "[sup ] server wake\n");
			}
		}
		if (getenv("SUPERS_DEBUG")) {
			fprintf(stderr, "[sup ] server after wait ptr=%p stop=%d\n", (void *)supers_req, supers_stop);
		}
		if (supers_stop && !supers_req) {
			pthread_mutex_unlock(&supers_req_mutex);
			break;
		}
		supers_req_t *req = supers_req;
		pthread_mutex_unlock(&supers_req_mutex);
		if (getenv("SUPERS_DEBUG")) {
			fprintf(stderr, "[sup ] server handling kind=%d\n", (int)req->kind);
		}

		if (use_rts_lock && rts_lock_fp) {
			rts_lock_fp();
		}
		if (req->kind == SUPERS_REQ_SPEC) {
			req->fptr.spec(req->tx, req->oper, req->result);
		} else {
			req->fptr.nspec(req->oper, req->result);
		}
		if (use_rts_lock && rts_unlock_fp) {
			rts_unlock_fp();
		}

		pthread_mutex_lock(&supers_req_mutex);
		supers_req = NULL;
		pthread_cond_broadcast(&supers_req_cv);
		pthread_mutex_unlock(&supers_req_mutex);
	}
	if (supers_hs_exit_fp) {
		supers_hs_exit_fp();
	}
}
static void start_supers_worker(void) {
	if (supers_worker_started) return;
	supers_stop = 0;
	if (pthread_create(&supers_worker_thread, NULL, supers_worker_main, NULL) != 0) {
		fprintf(stderr, "Error: failed to start supers worker thread\n");
		exit(1);
	}
	supers_worker_started = 1;
}

static void stop_supers_worker(void) {
	if (!supers_worker_started) return;
	pthread_mutex_lock(&supers_req_mutex);
	supers_stop = 1;
	pthread_cond_broadcast(&supers_req_cv);
	pthread_mutex_unlock(&supers_req_mutex);
	pthread_join(supers_worker_thread, NULL);
	supers_worker_started = 0;
}

static void supers_call(cblockptr_t fptr, supers_req_kind_t kind, tm_tx_t *tx, oper_t **oper, oper_t *result) {
	supers_req_t req;
	req.kind = kind;
	req.fptr = fptr;
	req.tx = tx;
	req.oper = oper;
	req.result = result;

	if (!supers_worker_started && !use_supers_worker_main) {
		fprintf(stderr, "Error: supers worker not started\n");
		exit(1);
	}
	if (getenv("SUPERS_DEBUG")) {
		fprintf(stderr, "[sup ] req start kind=%d\n", (int)kind);
	}
	pthread_mutex_lock(&supers_req_mutex);
	while (supers_req) {
		pthread_cond_wait(&supers_req_cv, &supers_req_mutex);
	}
	supers_req = &req;
	if (getenv("SUPERS_DEBUG")) {
		fprintf(stderr, "[sup ] req set ptr=%p\n", (void *)supers_req);
	}
	pthread_cond_broadcast(&supers_req_cv);
	while (supers_req) {
		pthread_cond_wait(&supers_req_cv, &supers_req_mutex);
	}
	pthread_mutex_unlock(&supers_req_mutex);
	if (getenv("SUPERS_DEBUG")) {
		fprintf(stderr, "[sup ] req done kind=%d\n", (int)kind);
	}
}
int spec_global_time = 0;
wcounters_list_t wcounters = {NULL, NULL};  //Wait-counters for garbage (old operands) collection. (head = NULL, tail = NULL)
//NOTE: This won't work if two or more Commit instructions may run in parallel. Need to change the data structure if that is desirable
#ifdef STAT_STM
int number_of_rollbacks = 0;
#endif
char **superargv;
int superargc;
int n_tasks;
__thread int tid;
int n_procs;
int n_av_procs;
/*----------------------------*/







void barrier_init(barrier_t *barrier, int n_threads)  {
	pthread_mutex_init(&(barrier->mutex), NULL);
	pthread_cond_init(&(barrier->cond), NULL);
	barrier->count = 0;
	barrier->n = n_threads; 
	
}


void barrier_wait(barrier_t *barrier) {

	pthread_mutex_lock(&(barrier->mutex));
	barrier->count++;
	if (barrier->count == barrier->n) {
		barrier->count=0;
		pthread_cond_broadcast(&(barrier->cond));

	} else 
		pthread_cond_wait(&(barrier->cond), &(barrier->mutex));
	
		
	pthread_mutex_unlock(&(barrier->mutex));
	
} 

void init_combuff(combuff_t *comm) {
	init_queue(&(comm->queue));
	pthread_mutex_init(&(comm->mutex), NULL);
	pthread_cond_init(&(comm->cond), NULL);
	comm->waiting = 0;	


}
void comm_send(combuff_t *comm, qelem elem) {
	pthread_mutex_lock(&(comm->mutex));

	enqueue(elem, &(comm->queue)); 
	if (comm->waiting)
		pthread_cond_signal(&(comm->cond));
	
	
	pthread_mutex_unlock(&(comm->mutex));

}
qelem comm_recv(combuff_t *comm, int blocking) {
	qelem elem = NULL;

	queue_t *queue = &(comm->queue);
	
	pthread_mutex_lock(&(comm->mutex));
	if (force_spin) {
		blocking = 0;
	}
	//printf("Peguei mutex %x\n",&(comm->mutex));	
	if (!HAS_ELEMENTS(queue) && blocking) {
		comm->waiting = 1;
		while (!HAS_ELEMENTS(queue)) {
			if (comm_wait_usec > 0) {
				struct timespec ts;
				clock_gettime(CLOCK_REALTIME, &ts);
				long nsec = ts.tv_nsec + (comm_wait_usec * 1000L);
				ts.tv_sec += nsec / 1000000000L;
				ts.tv_nsec = nsec % 1000000000L;
				if (pthread_cond_timedwait(&(comm->cond), &(comm->mutex), &ts) == ETIMEDOUT) {
					break;
				}
			} else {
				pthread_cond_wait(&(comm->cond), &(comm->mutex));
			}
		}
		comm->waiting = 0;
	}
	if (HAS_ELEMENTS(queue)) {
		elem = get_first(&(comm->queue));	
	}
 	

	
	pthread_mutex_unlock(&(comm->mutex));

	return(elem);

}

/*
Another way of getting the number of processors (without using sysconf)
int GetCPUCount()
{
 cpu_set_t cs;
 CPU_ZERO(&cs);
 sched_getaffinity(0, sizeof(cs), &cs);

 int i,count = 0;
 for (i = 0; i < (sizeof(cs)*8) ; i++)
 {
  if (CPU_ISSET(i, &cs))
   count++;
 }
 return count;
}*/
int main(int argc, char **argv) {
	if (getenv("DF_DEBUG_LOAD")) {
		fprintf(stderr, "[interp] start argc=%d\n", argc);
		fflush(stderr);
	}

	int n_threads, i;
	thread_args *t_args;
	n_procs = sysconf( _SC_NPROCESSORS_ONLN );//GetCPUCount();
#ifdef SET_AFFINITY
#endif
	pthread_t *threads;
	FILE *fp, *pla;
	if (argc < 4) {

		printf("Uso: ./inter <n_pes> <input.flb> <input.fla> <superinstructions_lib>(optional)\n");
		return(1);
	}
	superargv = argv + 5;
	superargc = argc - 5;
	n_threads=atoi(argv[1]);
	int check_msgs_env_set = 0;
	const char *check_env = getenv("CHECK_MSGS_INTERVAL");
	if (check_env && check_env[0] != '\0') {
		int v = atoi(check_env);
		if (v > 0) {
			check_msgs_interval = v;
			check_msgs_env_set = 1;
		}
	}
	const char *idle_spins_env = getenv("IDLE_SPINS");
	if (idle_spins_env && idle_spins_env[0] != '\0') {
		int v = atoi(idle_spins_env);
		if (v >= 0) {
			idle_spins = v;
			idle_spins_env_set = 1;
		}
	}
	const char *idle_yield_env = getenv("IDLE_YIELD");
	if (idle_yield_env && idle_yield_env[0] != '\0') {
		idle_yield = (idle_yield_env[0] != '0');
		idle_yield_env_set = 1;
	}
	const char *wait_env = getenv("COMM_WAIT_USEC");
	if (wait_env && wait_env[0] != '\0') {
		long v = atol(wait_env);
		if (v >= 0) {
			comm_wait_usec = v;
			comm_wait_env_set = 1;
		}
	}
	const char *steal_env = getenv("STEAL_WORK");
	if (steal_env && steal_env[0] != '\0') {
		if (steal_env[0] == '0') {
			steal_enabled = 0;
		}
	}
	const char *steal_batch_env = getenv("STEAL_BATCH");
	if (steal_batch_env && steal_batch_env[0] != '\0') {
		int v = atoi(steal_batch_env);
		if (v > 0) {
			steal_batch = v;
			steal_batch_env_set = 1;
		}
	}
	const char *steal_thr_env = getenv("STEAL_THRESHOLD");
	if (steal_thr_env && steal_thr_env[0] != '\0') {
		int v = atoi(steal_thr_env);
		if (v >= 0) {
			steal_threshold = v;
			steal_threshold_env_set = 1;
		}
	}
	const char *sched_env = getenv("SCHED_PROF");
	if (sched_env && sched_env[0] != '\0' && sched_env[0] != '0') {
		sched_prof = 1;
	}
	const char *force_env = getenv("FORCE_SPIN");
	if (force_env && force_env[0] != '\0') {
		force_spin = (force_env[0] != '0');
		force_spin_env_set = 1;
	}
	const char *df_list_env = getenv("DF_LIST_BUILTIN");
	if (df_list_env && df_list_env[0] != '\0') {
		use_df_list_builtin = (df_list_env[0] != '0');
	}
	const char *df_list_block_env = getenv("DF_LIST_BLOCK");
	if (df_list_block_env && df_list_block_env[0] != '\0') {
		long v = atol(df_list_block_env);
		if (v > 0) {
			df_list_block_size = (size_t)v;
		}
	}
	const char *ignore_exec_env = getenv("DF_IGNORE_EXEC");
	if (ignore_exec_env && ignore_exec_env[0] != '\0') {
		ignore_exec_match = (ignore_exec_env[0] != '0');
	} else {
		// Default to enforcing exec matching for correctness.
		ignore_exec_match = 0;
	}
	const char *ignore_tag_env = getenv("DF_IGNORE_TAG");
	if (ignore_tag_env && ignore_tag_env[0] != '\0') {
		ignore_tag_match = (ignore_tag_env[0] != '0');
	} else {
		// Default to enforcing tag matching for correctness.
		ignore_tag_match = 0;
	}

	if (n_threads > 1 && steal_enabled) {
		if (!check_msgs_env_set && check_msgs_interval < 8) {
			check_msgs_interval = 64;
		}
		if (!comm_wait_env_set && comm_wait_usec < 0) {
			comm_wait_usec = 50;
		}
		if (!idle_spins_env_set && idle_spins == 0) {
			idle_spins = 1000;
		}
		if (!idle_yield_env_set) {
			idle_yield = 1;
		}
		if (!steal_threshold_env_set && steal_threshold == 0) {
			steal_threshold = 8;
		}
		if (!steal_batch_env_set && steal_batch <= 1) {
			steal_batch = 8;
		}
	}

	if (n_threads < 1) {
		fprintf(stderr, "The number of PEs has to be >= 1\n");
		return(1);
	}
	if (!getenv("SUPERS_RTS_N")) {
		char rts_n_buf[16];
		snprintf(rts_n_buf, sizeof(rts_n_buf), "%d", n_threads);
		setenv("SUPERS_RTS_N", rts_n_buf, 0);
	}
	
	//TODO separate n_tasks and n_threads
	n_tasks=n_threads;
	

	if (!(fp = fopen(argv[2], "rb"))) {
		fprintf(stderr, "Error opening file\n");	
		return(1);
	}


        if (!(pla = fopen(argv[3], "r"))) {
                fprintf(stderr, "Error opening placement file\n");
                return(1);
        }

	libhandle = dlopen(argv[4], RTLD_GLOBAL | RTLD_LAZY);
	if ((argc >=5) && ! libhandle ) { //Load the object code with the superinstructions
		fprintf(stderr, "Error opening dynamic library %s\n", argv[4]);
		return(1);
	}
	int threaded_available = 0;
	if (libhandle && dlsym(libhandle, "supers_hs_init_thread")) {
		threaded_available = 1;
	} else if (dlsym(RTLD_DEFAULT, "hs_init_thread")) {
		threaded_available = 1;
	}
	const char *force_par_env = getenv("SUPERS_FORCE_PAR");
	int force_par = force_par_env && force_par_env[0] != '\0' && force_par_env[0] != '0';
	if (force_par && !threaded_available) {
		fprintf(stderr, "[sup ] SUPERS_FORCE_PAR ignored (no threaded RTS)\n");
		force_par = 0;
	}
	if (force_par) {
		setenv("SUPERS_RTS_LOCK", "0", 1);
		setenv("SUPERS_SERIAL", "0", 1);
		setenv("SUPERS_WORKER", "0", 1);
		setenv("SUPERS_WORKER_MAIN", "0", 1);
	}
	const char *lock_env = getenv("SUPERS_RTS_LOCK");
	const char *serial_env = getenv("SUPERS_SERIAL");
	const char *worker_env = getenv("SUPERS_WORKER");
	const char *worker_main_env = getenv("SUPERS_WORKER_MAIN");
	const char *manual_env = getenv("SUPERS_HS_INIT_MANUAL");
	if (!lock_env) {
		setenv("SUPERS_RTS_LOCK", threaded_available ? "0" : "1", 0);
	}
	if (!serial_env) {
		setenv("SUPERS_SERIAL", threaded_available ? "0" : "1", 0);
	}
	if (!worker_env && !worker_main_env) {
		setenv("SUPERS_WORKER", threaded_available ? "0" : "1", 0);
	}
	if (!manual_env) {
		setenv("SUPERS_HS_INIT_MANUAL", "1", 0);
	}
	setup_hs_thread_fns();
	fprintf(stderr, "[sup ] worker=%d worker_main=%d\n", use_supers_worker, use_supers_worker_main);
	if (use_df_list_builtin) {
		fprintf(stderr, "[list] builtin=df\n");
	}
	if (libhandle && use_supers_worker && !use_supers_worker_main) {
		start_supers_worker();
		atexit(stop_supers_worker);
	}
	if (!use_supers_worker && !use_supers_worker_main && supers_hs_init_fp) {
		supers_hs_init_fp();
	}
	if (use_supers_worker_main && supers_hs_init_fp) {
		supers_hs_init_fp();
	}
	
	#ifdef DEBUG_MEMORY
	mtrace();	
	#endif
	
	if ((t_args = (thread_args *)malloc(sizeof(thread_args)*n_threads)) == NULL) {
		fprintf(stderr, "Error allocating memory for thread args\n");
		exit(1);
	}
	
	if ((threads = (pthread_t *)malloc(sizeof(pthread_t)*n_threads)) == NULL) {
		fprintf(stderr, "Error allocating memory for thread structures\n");
		exit(1);
	}; 	
	if (sched_prof) {
		sched_busy_iters = (uint64_t *)calloc(n_threads, sizeof(uint64_t));
		sched_idle_iters = (uint64_t *)calloc(n_threads, sizeof(uint64_t));
		sched_execs = (uint64_t *)calloc(n_threads, sizeof(uint64_t));
		sched_max_ready = (int *)calloc(n_threads, sizeof(int));
		if (!sched_busy_iters || !sched_idle_iters || !sched_execs || !sched_max_ready) {
			fprintf(stderr, "Error allocating scheduler profile buffers\n");
			exit(1);
		}
		fprintf(stderr, "[sched] profiling enabled\n");
	}

	barrier_init(&barrier, n_threads);
	pe_running = n_threads;

	initialize_threads(t_args, n_threads, fp, pla);
	g_args = t_args;
	g_n_threads = n_threads;
	#ifdef USE_STM
	//stm_init(); //initialize main stm
	#endif
	
	int NUMBER_OF_CORES=n_procs;
	char* NCs;
	NCs = getenv ("NUM_CORES");
	if (NCs!=NULL)
		NUMBER_OF_CORES = atoi(NCs);
		
	n_av_procs = (n_procs<NUMBER_OF_CORES) ? n_procs : NUMBER_OF_CORES;
	fprintf(stderr, "Procs %d\n", n_av_procs);
	struct timespec _ts_start, _ts_end;
	clock_gettime(CLOCK_MONOTONIC, &_ts_start);
	for (i = 0; i < n_threads; i++) {
		//printf("Criando thread: %d\n", i);fflush(stdout);
		pthread_create(threads+i, NULL, pe_main, (void *)(t_args+i));
	}
	if (use_supers_worker_main) {
		supers_server_loop();
	}
	for (i=0; i < n_threads; i++) {
		pthread_join(threads[i], NULL);
		free(t_args[i].ready_queue.elem);
		pthread_mutex_destroy(&t_args[i].ready_mutex);
	}
	clock_gettime(CLOCK_MONOTONIC, &_ts_end);
	{
		double _elapsed = (_ts_end.tv_sec - _ts_start.tv_sec)
		                + (_ts_end.tv_nsec - _ts_start.tv_nsec) / 1e9;
		fprintf(stderr, "EXEC_TIME_S %.9f\n", _elapsed);
	}
	if (sched_prof) {
		uint64_t total_execs = 0;
		fprintf(stderr, "[sched] per-PE summary (busy/idle loop iters)\n");
		for (i = 0; i < n_threads; i++) {
			uint64_t busy = sched_busy_iters[i];
			uint64_t idle = sched_idle_iters[i];
			uint64_t total = busy + idle;
			double busy_pct = total ? (100.0 * (double)busy / (double)total) : 0.0;
			fprintf(stderr,
				"[sched] pe=%d execs=%llu busy=%llu idle=%llu busy_pct=%.1f max_ready=%d\n",
				i,
				(unsigned long long)sched_execs[i],
				(unsigned long long)busy,
				(unsigned long long)idle,
				busy_pct,
				sched_max_ready[i]);
			total_execs += sched_execs[i];
		}
		fprintf(stderr, "[sched] total_execs=%llu\n", (unsigned long long)total_execs);
		free(sched_busy_iters);
		free(sched_idle_iters);
		free(sched_execs);
		free(sched_max_ready);
	}
	
	#ifdef STAT_STM
	printf("STM STAT: Total number of rollbacks = %d\n", number_of_rollbacks);
	#endif
	#ifdef USE_STM
	//stm_exit();
	#endif
	free(t_args);
	free(threads);
	#ifdef DEBUG_MEMORY
	muntrace();
	#endif
	return(0);
}


void * pe_main(void *args) {
	thread_args *attr = (thread_args *)args;
	tls_attr = attr;
	instr_t *instr;
	
	dispatch_t *disp;
	queue_t *commq = &((comm_buffer + attr->id)->queue);
	uint64_t busy_iters = 0;
	uint64_t idle_iters = 0;
	uint64_t execs = 0;
	int max_ready = 0;

#ifdef SET_AFFINITY
	{
		cpu_set_t affinity_mask;
		CPU_ZERO(&affinity_mask);
		CPU_SET(attr->id % n_av_procs, &affinity_mask);
		if (pthread_setaffinity_np(pthread_self(), sizeof(affinity_mask), &affinity_mask) != 0) {
			perror("pthread_setaffinity_np");
		}
	}
#endif

	int check_msgs_count = check_msgs_interval;
	// Register only if this thread will call into Haskell directly.
	if (!use_supers_worker && !use_supers_worker_main && hs_init_thread_fp) {
		hs_init_thread_fp();
	}

	while (!attr->global_termination) {  //MAIN LOOP
		int ready_count = 0;
		#define EXEC_DISP() \
			do { \
				if (sched_prof && ready_count > max_ready) { \
					max_ready = ready_count; \
				} \
				if (sched_prof) { \
					busy_iters++; \
					execs++; \
				} \
				instr = disp->instr; \
				eval(disp, attr); \
				check_msgs_count--; \
				if (!check_msgs_count) { \
					treat_msgs(attr, 0); \
					check_msgs_count = check_msgs_interval; \
				} \
			} while (0)
		while ((disp = readyq_dequeue(attr, &ready_count)) != NULL) {
			#ifdef DEBUG_EXECUTION
			printf("Executando instrucao %d (pe: %d)\n", disp->instr->opcode, attr->id);
			#endif
			EXEC_DISP();
			if (steal_threshold > 0 && ready_count <= steal_threshold) {
				dispatch_t *stolen = steal_work(attr);
				if (stolen) {
					readyq_enqueue(attr, (qelem)stolen);
				}
			}
			//free(disp);
		}
		disp = steal_work(attr);
		if (disp) {
			attr->isidle = 0;
			EXEC_DISP();
			continue;
		}
		if (idle_spins > 0) {
			for (int spin = 0; spin < idle_spins; spin++) {
				treat_msgs(attr, 0);
				disp = readyq_dequeue(attr, &ready_count);
				if (disp) {
					attr->isidle = 0;
					EXEC_DISP();
					goto continue_loop;
				}
				disp = steal_work(attr);
				if (disp) {
					attr->isidle = 0;
					EXEC_DISP();
					goto continue_loop;
				}
				if (idle_yield) {
					sched_yield();
				}
			}
		}
		// TODO: move this if outside of the loop
		if (attr->n_edges == 0) {
			attr->global_termination = 1; 
			continue;
		}
		check_msgs_count = check_msgs_interval;
		if (!HAS_ELEMENTS(commq) && !attr->isidle)  {
			attr->isidle = 1;
			attr->termination_tag++;
			attr->termination_count = 0;
			#ifdef DEBUG_TERMINATION
			printf("Initiating termination detection with tag %d. (pe: %d)\n", attr->termination_tag, attr->id);
			#endif
			send_markers(attr->termination_tag, attr->id, attr->n_edges);
			
		}
	
		#ifdef DEBUG_COMMUNICATION
	        printf("PE: %d esperando msg\n", attr->id);
	        #endif
		if (sched_prof) {
			idle_iters++;
		}
		if (force_spin) {
			treat_msgs(attr, 0);
		} else {
			treat_msgs(attr, 1);
		}
continue_loop:
		;
	} //END OF MAIN LOOP
	#undef EXEC_DISP
	
	

	#ifdef DEBUG_GC
	debug_oplists(attr);
	#endif

	__sync_fetch_and_sub(&pe_running, 1);
	pthread_mutex_lock(&supers_req_mutex);
	pthread_cond_broadcast(&supers_req_cv);
	pthread_mutex_unlock(&supers_req_mutex);
	barrier_wait(&barrier);
	if (!use_supers_worker && !use_supers_worker_main && hs_exit_thread_fp) {
		hs_exit_thread_fp();
	}
	if (sched_prof) {
		sched_busy_iters[attr->id] = busy_iters;
		sched_idle_iters[attr->id] = idle_iters;
		sched_execs[attr->id] = execs;
		sched_max_ready[attr->id] = max_ready;
	}
	return(NULL);

}
void treat_msgs(thread_args *attr, int isblocking) {
	qelem rcvmsg;
	optoken_t *rcvtoken;
	dispatch_t *disp;
	int blocking = isblocking;
#ifdef DEBUG_COMMUNICATION
	printf("PE %d verificando buffer de mensagens\n", attr->id);
#endif
        for (;;) {
                rcvmsg = comm_recv(comm_buffer + attr->id, blocking);
                if (rcvmsg == NULL) {
                        break;
                }
                blocking = 0;
		switch (((marker_t *)rcvmsg)->type) {
			case MSG_TERMDETECT:
				#ifdef DEBUG_TERMINATION
				printf("Received marker with tag: %d (pe: %d, count = %d).\n", ((marker_t*)rcvmsg)->tag, attr->id, attr->termination_count);
				#endif
				attr->global_termination = treat_marker(((marker_t *)rcvmsg)->tag, &(attr->termination_tag), &(attr->termination_count), attr, attr->isidle);
				free((marker_t *)rcvmsg);
				break;
			case MSG_GC:
				#ifdef DEBUG_COMMUNICATION
				printf("Garbage collection msg received for timestamp: %d (pe: %d)\n", ((marker_t *)rcvmsg)->tag, attr->id);
				#endif
				#ifdef DEBUG_GC
				printf("Antes..\n");
				debug_oplists(attr);
				printf("Depois..\n");
				#endif
				spec_clean(attr, ((marker_t*)rcvmsg)->tag);
				free((marker_t *)rcvmsg);				
				#ifdef DEBUG_GC
				//printf("Garbage collecting opers with timestamp:%d (pe: %d)\n", ((marker_t*)rcvmsg)->tag, attr->id);
				debug_oplists(attr);
				#endif
				break;

			case MSG_OPER:
				rcvtoken = (optoken_t *)rcvmsg;
				#ifdef DEBUG_COMMUNICATION
				printf("PE: %d - Token recebido %x - tag: %d spec: %d\n", attr->id, (int)(rcvtoken->oper).value.i, (rcvtoken->oper).tag,(rcvtoken->oper).spec);
				#endif
				//bypass_oper((rcvtoken->dst)->src + rcvtoken->dstn, rcvtoken->oper);

				rcvtoken->oper.owner_token = rcvtoken;
				rcvtoken->oper.next = NULL;
				int tag_mode = (ignore_tag_match ? 0 : -1);
				int exec_mode = (ignore_exec_match ? 0 : -1);
				if (rcvtoken->dst->opcode == OP_VALTOTAG && !ignore_tag_match) {
					if (rcvtoken->dstn == 1 && valtag_is_execonly_oper1(&rcvtoken->oper)) {
						tag_mode = 0;
					} else if (rcvtoken->dstn == 0 && valtag_execonly_bucket(rcvtoken->dst, rcvtoken->oper.exec)) {
						tag_mode = 0;
					}
				}
				bypass_oper(&((rcvtoken->dst)->opmatch), rcvtoken->dstn, &rcvtoken->oper, attr,
				            tag_mode, exec_mode);
				if (rcvtoken->dst->opcode == OP_VALTOTAG &&
				    rcvtoken->dstn == 1 && valtag_is_execonly_oper1(&rcvtoken->oper)) {
					valtag_promote_execonly_op0(rcvtoken->dst, rcvtoken->oper.exec, attr);
				}
				int tag_exec = (ignore_tag_match ? 0 : (rcvtoken->oper).tag);
				int exec_exec = (ignore_exec_match ? 0 : (rcvtoken->oper).exec);
				if (rcvtoken->dst->opcode == OP_VALTOTAG && !ignore_tag_match) {
					if (rcvtoken->dstn == 1 && valtag_is_execonly_oper1(&rcvtoken->oper)) {
						tag_exec = 0;
					} else if (rcvtoken->dstn == 0 && valtag_execonly_bucket(rcvtoken->dst, rcvtoken->oper.exec)) {
						tag_exec = 0;
					}
				}
				if ((disp = can_exec(rcvtoken->dst,
				                     tag_exec,
				                     exec_exec,
				                     attr)))
					readyq_enqueue(attr, (qelem)disp);
				// token ownership transferred; release happens when operand is cleaned
				attr->isidle = 0;
				break;
			case MSG_DISPSND:
				#ifdef DEBUG_COMMUNICATION
				printf("PE: %d - dispatch recebido %x\n", attr->id, ((dispsnd_t *)rcvmsg)->disp);
				#endif
				disp = ((dispsnd_t *)rcvmsg)->disp;	
				readyq_enqueue(attr, (qelem)disp);
				dispsnd_release(attr, (dispsnd_t *)rcvmsg);
				attr->isidle = 0;
				break;


			default:
				#ifdef DEBUG_COMMUNICATION
				printf("PE: %d - Unknown msg type received: %d\n", attr->id,((marker_t *)rcvmsg)->type );
				#endif
				break;	
		}	
        }



}
/* Global termination detection algorithm: 
   This is an implementation of the termination detection algorithm based on distributed snapshots. The difference is that, since our topology is a complete graph, we don't need a leader to collect all states. 
   A node (thread) begins the termination detection after entering state (isisdle == 1). It then broadcasts a marker with termination_tag equal to the greatest marker tag seen plus 1. Other nodes enter this termination detection if they already are in (isidle == 1) state and if the tag received is greater than the greatest termination tag seen. After entering this new termination detection this other node also broadcasts the marker with the termination_tag received, this indicates his (is_idle == 1) state to all the other nodes. Once a node participating in a termination detection has received a marker corresponding to that termination detection from all neighboors (i.e. all other nodes in the complete graph topololy), it broadcasts another marker with the same tag if it is still in state (isidle == 1). 
  The second marker broadcasted carries the information that all input edges to that node were detected empty, since the node only broadcasted the marker because it stayed in (isidle == 1) state until receiving the first (node state) markers from all other nodes, meaning it did not receive a message containing an operand.

  So, in our implementation, we use only one type of message, the marker with the termination_tag, and the first one carries the node state information (isidle == 1) while the second one carries the edges state information (all empty). It is then clear that after receiving the second round of markers from all other nodes, the node can terminate.

 */


int treat_marker(int tag, int *pmax_tag, int *pcount, thread_args *attr, int isidle) {
	//Global termination detection algorithm

	int n_edges = attr->n_edges;

	int term_detected = 0;

	if (tag > *pmax_tag) {

		*pmax_tag = tag;
		if (isidle) {
			*pcount = 0; //the count will be incremented in the next if
			#ifdef DEBUG_TERMINATION
			printf("Entered termination detection tag: %d (pe: %d).\n", tag, attr->id); //Got into a new termination detection
			#endif
			send_markers(tag, attr->id, attr->n_edges); 
		}

	}


	if (tag == *pmax_tag && isidle) {
		(*pcount)++;
		
		if (*pcount == n_edges) {
			#ifdef DEBUG_TERMINATION
			printf("Sending state (pe: %d, count = %d)\n", attr->id, *pcount);
			#endif
			send_markers(tag, attr->id, attr->n_edges); //send state
		} else
				
			if (*pcount == 2*n_edges) {
				term_detected = 1; //termination detected
				#ifdef DEBUG_TERMINATION
				printf("Termination detected (pe: %d)\n", attr->id);
				#endif
			}
		
	}

	return(term_detected);

}

void send_markers(int tag, int id, int n) {
	/*NOTE: Since the communication is implemented in a complete graph topology, the target nodes of all nodes(threads) are the same, so we just have one array of communication buffers, the global variable comm_buffer[] */
	int i;

	marker_t *marker;

	for (i = 0; i < n+1; i++) {
		if (i != id) {
			marker = (marker_t *) malloc(sizeof(marker_t));
			//marker->ptr = NULL;                                      	
			marker->tag = tag;
			marker->type = MSG_TERMDETECT;
			#ifdef DEBUG_TERMINATION
			printf("Sending token with tag %d to %d. (pe: %d), nedges= %d\n", tag, i, id, n);
			#endif
			comm_send(comm_buffer + i, (qelem)marker);		
		}
	}




}
//void eval(instr_t instr, thread_args  *pe_attr) {

//void eval(instr_t *instr, oper_t **oper, thread_args  *pe_attr) {
void eval(dispatch_t *disp, thread_args *pe_attr) {	
	//oper_t **oper = disp->op;
	//
	oper_t **oper = disp->op; 
	instr_t *instr = disp->instr;
	int i, tag, exectag, spectag;
	//int free_disp = 1
	int  free_opers = !disp->speculative;
#ifdef USE_STM	
	tm_tx_t *tx;
#endif	
	cblockptr_t fptr; //pointer to the function of the SuperInstruction block

	oper_t result[MAX_DEST];
	

	tag = 0;
	exectag = 0;
	for (i = 0; i < instr->n_src; i++) {
		if (oper[i] != NULL && oper[i]->exec >= exectag) {
			exectag = oper[i]->exec;
			tag = oper[i]->tag;
		}
	}
	if (oper[0] != NULL && exectag == 0) {
		tag = oper[0]->tag;
	}
	// Default tag/exec propagation for ops that don't explicitly set them.
	for (i = 0; i < MAX_DEST; i++) {
		result[i].tag = tag;
		result[i].exec = exectag;
	}

	if (debug_trace_id(instr->id)) {
		fprintf(stderr, "[trace] exec id=%d op=%d n_src=%d n_dst=%d\n",
		        instr->id, instr->opcode, instr->n_src, instr->n_dst);
		for (i = 0; i < instr->n_src; i++) {
			if (oper[i]) {
				fprintf(stderr, "[trace]  src%d tag=%d exec=%d val=%lld\n",
				        i, oper[i]->tag, oper[i]->exec, (long long)oper[i]->value.li);
			}
		}
	}
        if (instr->speculative) {
                spectag = atomic_inc(&spec_global_time);
                free_opers = 0;
                /*for (i=0; i<instr->n_src; i++) {
                        //if (!oper[i]->isspeculative)
                        oper[i]->max_match = spectag; //the non-speculative operands of speculative instructions should havethe instruction spectag (execution time), so they can be garbage collected
                }*/
        } else
                spectag = disp->speculative;//get_notnull_spec(oper, instr->n_src); //only one can be >0, otherwise instr would have to be speculative
        for (i=0; i<instr->n_dst; i++) {
                result[i].tag = tag;
                result[i].exec = exectag;
                result[i].next = NULL;
                result[i].spec = spectag;
                //result[i].isspeculative = (spectag > 0);
                result[i].cleanup = NULL;

        }




	switch (instr->opcode) {
			
		case OP_CONST:
			//result = create_oper((void *) instr.immed );
			result[0].value.i = instr->immed.i;
			result[0].value.li = (long int)result[0].value.i;
			#ifdef DEBUG_EXECUTION
			printf("CONST: %lld tag: %d\n", (long long)result[0].value.li, result[0].tag);
			#endif
			break;

		case OP_DCONST:
			result[0].value.d = (double)instr->immed.f;
			#ifdef DEBUG_EXECUTION
                       	printf("DCONST: %lf tag: %d -- immed: %lf \n", result[0].value.d, result[0].tag, instr->immed.f);
                       	#endif
			break;
		case OP_ADD:
			//oper1 = get_oper(instr.src[0], instr.tag);
			//oper2 = get_oper(instr.src[1], instr.tag);   

			//result = create_oper((void *)((int) oper1->value + (int) oper2->value));
		
			
			{
				int64_t a = (int64_t)oper[0]->value.li;
				int64_t b = (int64_t)oper[1]->value.li;
				int64_t r = a + b;
				result[0].value.li = (long int)r;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
			printf("ADD: %lld + %lld = %lld\n",
			       (long long)oper[0]->value.li,
			       (long long)oper[1]->value.li,
			       (long long)result[0].value.li);
			#endif
			break;

	
		case OP_SUB:
			{
				int64_t a = (int64_t)oper[0]->value.li;
				int64_t b = (int64_t)oper[1]->value.li;
				int64_t r = a - b;
				result[0].value.li = (long int)r;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
			printf("SUB: %lld - %lld = %lld\n",
			       (long long)oper[0]->value.li,
			       (long long)oper[1]->value.li,
			       (long long)result[0].value.li);
			#endif
		   	break;


		case OP_ADDI:
			{
				int64_t r = (int64_t)instr->immed.i + (int64_t)oper[0]->value.li;
				result[0].value.li = (long int)r;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
			printf("ADDI: %d + %lld\n", instr->immed.i, (long long)oper[0]->value.li);
			#endif
			break;


                case OP_SUBI:
			{
				int64_t r = (int64_t)oper[0]->value.li - (int64_t)instr->immed.i;
				result[0].value.li = (long int)r;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
  			printf("SUBI: %lld + %d\n", (long long)oper[0]->value.li, instr->immed.i);
			#endif
			break;


		case OP_MULI:
			{
				int64_t r = (int64_t)oper[0]->value.li * (int64_t)instr->immed.i;
				result[0].value.li = (long int)r;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION	
                	printf("MUL: %lld + %d = %lld\n",
			       (long long)oper[0]->value.li, instr->immed.i,
			       (long long)result[0].value.li);
                	#endif
			break;
		case OP_DIVI:
			{
				int64_t a = (int64_t)oper[0]->value.li;
				int64_t b = (int64_t)instr->immed.i;
				int64_t q = a / b;
				int64_t r = a % b;
				result[0].value.li = (long int)q;
				result[0].value.i = (int)result[0].value.li;
				result[1].value.li = (long int)r;
				result[1].value.i = (int)result[1].value.li;
			}

			#ifdef DEBUG_EXECUTION	
        		printf("DIVI: %lld / %d = %lld (remainder %lld)\n",
			       (long long)oper[0]->value.li, instr->immed.i,
			       (long long)result[0].value.li, (long long)result[1].value.li);
	        	#endif
			break;


		case OP_MUL:
			{
				int64_t a = (int64_t)oper[0]->value.li;
				int64_t b = (int64_t)oper[1]->value.li;
				int64_t r = a * b;
				result[0].value.li = (long int)r;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION	
			printf("MUL: %lld + %lld = %lld\n",
			       (long long)oper[0]->value.li,
			       (long long)oper[1]->value.li,
			       (long long)result[0].value.li);
			#endif
			break;
		
		case OP_DIV:
			{
				int64_t a = (int64_t)oper[0]->value.li;
				int64_t b = (int64_t)oper[1]->value.li;
				int64_t q = a / b;
				int64_t r = a % b;
				result[0].value.li = (long int)q;
				result[0].value.i = (int)result[0].value.li;
				result[1].value.li = (long int)r;
				result[1].value.i = (int)result[1].value.li;
			}
			#ifdef DEBUG_EXECUTION	
        		printf("DIV: %lld / %lld = %lld (remainder %lld)\n",
			       (long long)oper[0]->value.li,
			       (long long)oper[1]->value.li,
			       (long long)result[0].value.li,
			       (long long)result[1].value.li);
	        	#endif
			break;

		case OP_FADD:
			result[0].value.f = (oper[0]->value.f + oper[1]->value.f);
			#ifdef DEBUG_EXECUTION
			printf("FADD: %f + %f = %f\n", oper[0]->value.f, oper[1]->value.f, result[0].value.f);
			#endif
			break;
		case OP_FMUL:
			result[0].value.f = (oper[0]->value.f * oper[1]->value.f);
			#ifdef DEBUG_EXECUTION
			printf("FMUL: %f * %f = %f\n", oper[0]->value.f, oper[1]->value.f, result[0].value.f);
			#endif
			break;
				
                case OP_DADD:
                        result[0].value.d = oper[0]->value.d + oper[1]->value.d;
                        #ifdef DEBUG_EXECUTION
                        printf("DADD: %17.9e + %17.9e = %17.9e\n", oper[0]->value.d, oper[1]->value.d, result[0].value.d);
			#endif
			break;
			
		case OP_AND:

			{
				int v = ((oper[0]->value.li != 0) && (oper[1]->value.li != 0)) ? 1 : 0;
				result[0].value.li = (long int)v;
				result[0].value.i = (int)result[0].value.li;
			}

			#ifdef DEBUG_EXECUTION
			printf("AND: %lld and %lld = %d",
			       (long long)oper[0]->value.li,
			       (long long)oper[1]->value.li,
			       (int)result[0].value.li);
			#endif

			break;



		case OP_STEER:
			//The steer instruction sends operand through port 0, if boolean_input == 0, or throught port 1, if boolean_input == 1.
			{
				int steer = (oper[0]->value.li != 0);
				instr->port_enable[0] = !steer;
				instr->port_enable[1] = steer;
				result[steer].value = oper[1]->value;
				if (getenv("DF_DEBUG_STEER")) {
					fprintf(stderr, "[steer] id=%d sel=%d val=%lld\n",
					        instr->id, steer, (long long)result[steer].value.li);
				}
			}
			#ifdef DEBUG_EXECUTION
			printf("STEER boolean = %d out = %lld specin = %d specout = %d\n",
			       (oper[0]->value.li != 0),
			       (long long)result[(oper[0]->value.li != 0)].value.li,
			       disp->speculative,
			       result[(oper[0]->value.li != 0)].spec);
			#endif
			break;
		case OP_LTHAN:
			{
				int v = (oper[0]->value.li < oper[1]->value.li) ? 1 : 0;
				result[0].value.li = (long int)v;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
			printf("LTHAN %lld < %lld\n",
			       (long long)oper[0]->value.li,
			       (long long)oper[1]->value.li);
			#endif 
			break;
		
		case OP_LTHANI:
			{
				int v = (oper[0]->value.li < instr->immed.i) ? 1 : 0;
				result[0].value.li = (long int)v;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
			printf("LTHANI %lld < %d\n", (long long)oper[0]->value.li, instr->immed.i);
			#endif 
			break;
	
		case OP_GTHAN:
			{
				int v = (oper[0]->value.li > oper[1]->value.li) ? 1 : 0;
				result[0].value.li = (long int)v;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
			printf("GTHAN %lld > %lld\n",
			       (long long)oper[0]->value.li,
			       (long long)oper[1]->value.li);
			#endif 
			break;
	

		case OP_GTHANI:
			{
				int v = (oper[0]->value.li > instr->immed.i) ? 1 : 0;
				result[0].value.li = (long int)v;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
			printf("GTHANI %lld > %d\n", (long long)oper[0]->value.li, instr->immed.i);
			#endif 
			break;

		case OP_EQUAL:
			{
				int v = (oper[0]->value.li == oper[1]->value.li) ? 1 : 0;
				result[0].value.li = (long int)v;
				result[0].value.i = (int)result[0].value.li;
			}
			#ifdef DEBUG_EXECUTION
			printf("EQUAL (%lld == %lld) = %d\n",
			       (long long)oper[0]->value.li,
			       (long long)oper[1]->value.li,
			       (int)result[0].value.li);
			#endif 
			break;
		
		case OP_ITAG:
			result[0].value = oper[0]->value;
			result[0].tag = oper[0]->tag + 1;
			#ifdef DEBUG_EXECUTION
			printf("ITAG %d + 1 = %d (valor %d)\n", oper[0]->tag, result[0].tag, result[0].value.i);
			#endif
			break;

		case OP_ITAGI:
			result[0].value = oper[0]->value;
			result[0].tag = oper[0]->tag + instr->immed.i;
			#ifdef DEBUG_EXECUTION
			printf("ITAGO %d + %d = %d (valor %d)\n", oper[0]->tag, instr->immed.i, result[0].tag, result[0].value.i);
			#endif
			break;

		case OP_CALLSND:
			{
				oper_t *tagop = oper[1] ? oper[1] : oper[0];
				result[0].value = oper[0]->value;
				result[0].exec = tagop->exec;
				result[0].tag = tagop->tag;
			#ifdef DEBUG_EXECUTION
                        printf("CALLSND TAG:%d  EXEC:%d  VAL:%d", result[0].tag, result[0].exec, result[0].value.i);
                        #endif
			if (getenv("DF_DEBUG_CALLRET")) {
				fprintf(stderr, "[callret] callsnd imm=%d count=%d in_exec=%d out_exec=%d\n",
				        instr->immed.i, instr->call_count, oper[0]->exec, result[0].exec);
			}
			}

			break;

		case OP_RETSND:
			{
				oper_t *tagop = oper[1] ? oper[1] : oper[0];
				result[0].value.i = instr->immed.i;
				result[0].tag = tagop->tag;
				result[0].exec = tagop->exec;
				
			#ifdef DEBUG_EXECUTION
			printf("RETSND TAG:%d EXEC:%d VAL:%d i:%d", result[0].tag, result[0].exec, result[0].value.i, instr->immed.i);
			#endif
			if (getenv("DF_DEBUG_CALLRET")) {
				fprintf(stderr, "[callret] retsnd imm=%d in_exec=%d out_exec=%d\n",
				        instr->immed.i, oper[0]->exec, result[0].exec);
			}
			}



			break;


		case OP_TAGTOVAL:
        		result[0].value.i = oper[0]->tag;
			result[0].exec = oper[0]->exec;
			result[0].tag = oper[0]->tag;
			#ifdef DEBUG_EXECUTION
                        printf("TAGVAL TAG:%d TAG:%d\n", result[0].value.i, oper[0]->tag);
                        #endif	
				
		break;

		case OP_VALTOTAG:
			if (oper[1]->value.i < 0) {
				result[0].tag = -oper[1]->value.i - 1;
			} else {
				result[0].tag = oper[1]->value.i;
			}
			result[0].value = oper[0]->value;
			result[0].exec = oper[1]->exec;
			#ifdef DEBUG_EXECUTION
                        printf("VALTAG TAG:%d VALUE:%d\n", result[0].tag, result[0].value.i);
                        #endif
			if (getenv("DF_DEBUG_VALTAG")) {
				fprintf(stderr, "[valtag] id=%d tag=%d exec=%d val=%lld\n",
				        instr->id, result[0].tag, result[0].exec, (long long)result[0].value.li);
			}


		break;
		case OP_RET:
			for (i = 0; i < instr->n_dst; i++)
				instr->port_enable[i] = 0;
			i = oper[1]->value.i; // callgroup id
			if (i < 0 || i >= instr->n_dst) {
				i = 0;
			}
			instr->port_enable[i] = 1;

			result[i].tag = oper[0]->tag;
			result[i].exec = oper[0]->exec;
			result[i].value = oper[0]->value;

			#ifdef DEBUG_EXECUTION
                         printf("Ret tag:%d  exec:%d val: %d outport: %d\n", result[i].tag, result[i].exec,result[i].value.i, i);
			#endif
			if (getenv("DF_DEBUG_CALLRET")) {
				fprintf(stderr, "[callret] ret outport=%d sel_exec=%d val_exec=%d\n",
				        i, oper[1]->exec, oper[1]->value.i);
			}

			break;
		case OP_STOPSPC:
			result[0].value = oper[0]->value;
			result[0].spec = 0;
			#ifdef DEBUG_EXECUTION
			printf("SPEC STOP\n");
			#endif
			break;
		#ifdef USE_STM
		case OP_COMMIT: 
			{	
				dispandtx_t *disptx;
				dispatch_t *disprcvd;
				disptx = oper[0]->value.p;
				disprcvd = disptx->disp;
				tx = disptx->tx;
				if (got_waited_ops(oper+2, instr->n_src - 2, disprcvd)) {	

					instr->port_enable[0] = instr->port_enable[1] = tm_commit(tx);			
					if (instr->port_enable[0]) {
					#ifdef DEBUG_STM
						printf("Instrucao commitada: opcode %d\n", disprcvd->instr->opcode);
					#endif
						result[0].value.i = 0; //Go-ahead token, could have any value
						result[1].value.i = oper[0]->spec; //Wait token, equal to the last reexecution time
						result[0].spec = result[1].spec = 0;
						//result[0].isspeculative = result[1].isspeculative = 0; 
						//A commit's outputs are not speculative, commit ends speculation.

						add_wait_counter(&wcounters, oper[0]->spec, instr->dst_len[1], pe_attr->n_threads);

						for (i = 0; i < (instr->n_src - 2); i++)				
							dec_wait_counter(&wcounters, oper[2+i]->value.i, pe_attr->n_threads); 
						//dispandtx_cleanup(disptx);

					} else {
						#ifdef STAT_STM
						number_of_rollbacks++;
						#endif
						#ifdef DEBUG_STM
						printf("Rollback no commit\n");
						if ((disprcvd->instr)->pe_id != pe_attr->id)
							printf("WARNING: Rolling back an instruction from another pe\n");
						
						#endif
						if ((disprcvd->instr)->pe_id != pe_attr->id)
							send_dispatch(disprcvd, (disprcvd->instr)->pe_id, pe_attr);
						else
							readyq_enqueue(pe_attr, (qelem)disprcvd);
						//free_disprcvd = 0; //don't free the dispatch because it was sent for reexecution
						//do rollback
					}
				} else {
					#ifdef DEBUG_STM
						printf("No commit.\n");
					#endif
					for (i = 0; i < instr->n_dst; i++)
						instr->port_enable[i] = 0;

					//dispandtx_cleanup(disptx);
				}
			}
			break;

		#endif
		default: 
			//Super Instruction
			//pe_attr.superinsts[instr->opcode - OP_SUPER1](oper, result);
			#ifdef DEBUG_EXECUTION
			#endif
			{
				int opcode=instr->opcode;
				int super_idx = opcode - OP_SUPER1;
				dispandtx_t *disptx;
				tid = instr->immed.i;				
     
				if (use_df_list_builtin && super_idx >= 0 && super_idx <= 3) {
					int debug_list = getenv("DF_DEBUG_LIST") != NULL;
					if (debug_list) {
						long long in0 = oper[0] ? (long long)oper[0]->value.li : 0;
						long long in1 = oper[1] ? (long long)oper[1]->value.li : 0;
						fprintf(stderr, "[list] id=%d op=%d in0=%lld in1=%lld\n",
						        instr->id, super_idx, in0, in1);
					}
					switch (super_idx) {
						case 0:
							result[0].value.li = df_list_cons(oper[0]->value.li, oper[1]->value.li);
							result[0].value.i = (int)result[0].value.li;
							break;
						case 1:
							result[0].value.li = df_list_head(oper[0]->value.li);
							result[0].value.i = (int)result[0].value.li;
							break;
						case 2:
							result[0].value.li = df_list_tail(oper[0]->value.li);
							result[0].value.i = (int)result[0].value.li;
							break;
						case 3:
							result[0].value.li = df_list_is_nil(oper[0]->value.li);
							result[0].value.i = (int)result[0].value.li;
							break;
					}
					if (super_idx == 0 && oper[1]) {
						// For cons, prefer tail tag/exec when tail is non-nil.
						if (oper[1]->value.li != 0) {
							result[0].tag = oper[1]->tag;
							result[0].exec = oper[1]->exec;
						} else if (oper[0]) {
							result[0].tag = oper[0]->tag;
							result[0].exec = oper[0]->exec;
						}
					} else if (oper[0]) {
						result[0].tag = oper[0]->tag;
						result[0].exec = oper[0]->exec;
					}
					if (debug_list) {
						fprintf(stderr, "[list] id=%d op=%d out=%lld\n",
						        instr->id, super_idx, (long long)result[0].value.li);
					}
					break;
				}
				fptr = get_super_fptr(opcode);
				#ifdef DEBUG_EXECUTION
				{
					char function_name[64]; // debug only
					snprintf(function_name, sizeof(function_name), "super%d", opcode - OP_SUPER1);
					printf("SuperInstruction %s\n", function_name);
				}
				#endif	
				#ifdef USE_STM
				if (instr->speculative) {
					#ifdef DEBUG_EXECUTION
					printf("Speculative Instruction\n");
					#endif



					tx = tm_create_tx(instr);
					//stm_start(tx, NULL, NULL);
					if (use_supers_worker) {
						supers_call(fptr, SUPERS_REQ_SPEC, tx, oper, result);
					} else {
						if (getenv("SUPERS_DEBUG")) {
							fprintf(stderr, "[sup ] direct spec start %s\n", function_name);
						}
						if (use_supers_mutex) {
							pthread_mutex_lock(&supers_mutex);
						}
						if (use_rts_lock && rts_lock_fp) {
							rts_lock_fp();
						}
						fptr.spec(tx, oper, result);
						if (use_rts_lock && rts_unlock_fp) {
							rts_unlock_fp();
						}
						if (use_supers_mutex) {
							pthread_mutex_unlock(&supers_mutex);
						}
						if (getenv("SUPERS_DEBUG")) {
							fprintf(stderr, "[sup ] direct spec done super%d\n", opcode - OP_SUPER1);
						}
					}

					disptx =  (dispandtx_t *)malloc(sizeof(dispandtx_t));
					//*disptx = (dispandtx_t) {disp, tx};
					//disp->free_disp = 0; //it will be free'd by the opers' GC
					disptx->disp = (dispatch_t *)malloc(sizeof(dispatch_t));
					*(disptx->disp) = *disp;
					disptx->disp->free_disp = 0;	//It will be free'd by the GC
					disptx->tx = tx;
					//memcpy(disptx->disp, disp, sizeof(dispatch_t));
					//printf("Dispatch que estou mandando: %d\n", disptx->disp->op[0]->spec);
					result[0].value.p = disptx;
					result[0].cleanup = &dispandtx_cleanup;
				}
				else
				#endif
					if (use_supers_worker) {
						supers_call(fptr, SUPERS_REQ_NSPEC, NULL, oper, result);
					} else {
						if (getenv("SUPERS_DEBUG")) {
							fprintf(stderr, "[sup ] direct nspec start super%d\n", opcode - OP_SUPER1);
						}
						if (use_supers_mutex) {
							pthread_mutex_lock(&supers_mutex);
						}
						if (use_rts_lock && rts_lock_fp) {
							rts_lock_fp();
						}
						fptr.nspec(oper, result);
						if (use_rts_lock && rts_unlock_fp) {
							rts_unlock_fp();
						}
						if (use_supers_mutex) {
							pthread_mutex_unlock(&supers_mutex);
						}
						if (getenv("SUPERS_DEBUG")) {
							fprintf(stderr, "[sup ] direct nspec done super%d\n", opcode - OP_SUPER1);
						}
					}
				{
					int super_idx2 = instr->opcode - OP_SUPER1;
					if (super_idx2 >= 0 && super_idx2 <= 3) {
						// List supers: keep tags aligned with the list structure.
						if (super_idx2 == 0 && instr->n_src > 1 && oper[1]) {
							if (oper[1]->value.li != 0) {
								result[0].tag = oper[1]->tag;
								result[0].exec = oper[1]->exec;
							} else if (oper[0]) {
								result[0].tag = oper[0]->tag;
								result[0].exec = oper[0]->exec;
							}
						} else if (oper[0]) {
							result[0].tag = oper[0]->tag;
							result[0].exec = oper[0]->exec;
						}
					} else {
						oper_t *src = NULL;
						for (i = 0; i < instr->n_src; i++) {
							if (oper[i]) {
								src = oper[i];
								break;
							}
						}
						if (src) {
							for (i = 0; i < instr->n_dst; i++) {
								result[i].tag = src->tag;
								result[i].exec = src->exec;
							}
						}
					}
				}

			}

			break;
	
	}
	
	for (i=0; i<instr->n_src && free_opers; i++)  {
		if (oper[i]->cleanup != NULL)
			oper[i]->cleanup(oper[i]->value.p, 1);
				oper_release(NULL, oper[i]);

	}
	
	
	if (disp->free_disp) {
		dispatch_release(pe_attr, disp);
	}
	
	#ifdef DEBUG_EXECUTION
	printf("Resultado: %d executado na thread: %d instr->n_dst = %d\n", result[0].value.i, pe_attr->id, instr->n_dst);
	#endif
	
	propagate_oper(instr, result, pe_attr);	
	

}


int got_waited_ops(oper_t **waitoper, int n_waits, dispatch_t *disp) {
	int i,j, n_opers, not_found, all_found = 1;
	int wait;
	oper_t **rcvdoper = disp->op;
	n_opers = (disp->instr)->n_src;

	
	for (i = 0; i < n_waits && all_found; i++) {
	  //n_waits will be <1 if their commit needs no waits
		not_found = 1;

		wait = waitoper[i]->value.i;
		//printf("Waiting for %d\n", wait);
		for (j = 0; j < n_opers && not_found; j++) {
			//printf("rcvdoper[%d]->spec = %d\n",j,rcvdoper[j]->spec);
			//if (wait == rcvdoper[j]->spec && rcvdoper[j]->isspeculative) {
			if (wait == rcvdoper[j]->spec) {
				//the non-speculative operands must not be counted, despite having the most recent spectag
				not_found = 0;
				rcvdoper[j]->spec *= -1; //Mark it so this wait is not counted again when doing the search for the next waits. 
				//printf("got wait! %d\n",rcvdoper[j]->spec);
			}
		}
		if (not_found) 
			all_found = 0;
	}

	for (i = 0; i < n_opers; i++)
		//Set the positive value back in the end.
		rcvdoper[i]->spec = rcvdoper[i]->spec * ((rcvdoper[i]->spec > 0) ? 1 : -1);

	return(all_found);
}

void debug_oplists(thread_args *pe_attr) {
	instr_t *instr;
	int i, j;
	oper_t *oper;
	opmatch_t *match;

	fprintf(stderr, "Remaining ops in pe: %d\n", pe_attr->id);
	for (i = 0; i < pe_attr->n_instrs; i++) {
		instr = pe_attr->instrs + i;
		match = instr->opmatch;
		while (match != NULL) {
			if (match->count > 0) {
				for (j = 0; j < instr->n_src; j++) {
					oper = match->op[j];	
					while (oper != NULL) {
					
						fprintf(stderr, "Instr: %d (opcode: %d) Input: %d tag: %d spec: %d valor: %x\n", i, instr->opcode, j, oper->tag, oper->spec, oper->value.i);

						oper = oper->next;
					
					}
				}	
			}
			match = match->next;
		}
	}


}

/*void debug_oplists(thread_args *pe_attr) {
	//Shows all remaining operands in the PE's instructions.
	instr_t *instr;
	int i,j;
	oper_t *oper;

	printf("Remaining ops in pe: %d\n", pe_attr->id);
	for (i=0; i < pe_attr->n_instrs; i++) {
		instr = pe_attr->instrs+i;
		
		for (j = 0; j < instr->n_src; j++) {
			oper = instr->src[j];
			while (oper != NULL) {
				printf("Instr: %d (opcode: %d) Input: %d tag: %d spec: %d valor: %x\n", i, instr->opcode, j, oper->tag, oper->spec, oper->value.i);

				//if (oper->cleanup != NULL)
				//	printf("   dispatch: %lx\n" , (double)((dispandtx_t *)oper->value.p)->disp);
				oper = oper->next;

			}


		}
	}


}
*/

/*
void clean_oplist_old(oper_t **oplist, int tstamp) {
	oper_t **ptr = oplist, *removed;
	int exectag, tag;

	#ifdef DEBUG_GC
//	printf("Clean oplist\n");fflush(stdout);
	#endif
	if (oplist != NULL) {
	
		while (*ptr != NULL && (*ptr)->max_match != tstamp) 
			//pprev = ptr;
			ptr = &((*ptr)->next);
		
		if ((*ptr) != NULL && (*ptr)->max_match == tstamp) {
			//if it has been matched with an operand with timestamp tstamp, we can remove it now.
			exectag = (*ptr)->exec;
			tag = (*ptr)->tag;
			while ((*ptr) != NULL && (*ptr)->exec == exectag && (*ptr)->tag == tag) {
				//we can also remove all the operands from old (re)executions of the same iteration that this one has replaced
				#ifdef DEBUG_GC
				printf("Spec cleaning %d\n", (*ptr)->max_match);
				#endif
				removed = *ptr;
				*ptr = (*ptr)->next;
				
				if (removed->cleanup != NULL) 
					removed->cleanup(removed->value.p);

				
				free(removed);
			}
	
	
	
		}
	
	}


}
*/
void remove_match(opmatch_t **matchptr, int n_src, thread_args *pe_attr) {
	int i;
	oper_t *oper, *next;
	opmatch_t *match = *matchptr;
	for (i = 0; i < n_src; i++) {
		oper = match->op[i];
		while (oper != NULL) {
			next = oper->next;
			if (oper->cleanup != NULL)
				oper->cleanup(oper->value.p, (next == NULL));
			oper_release(pe_attr, oper);
			oper = next;
		
		}
	
	}
	*matchptr = (*matchptr)->next;
	opmatch_release(pe_attr, match);

}
void spec_clean(thread_args *pe_attr, int tstamp) {
	int i;
	instr_t *instr;
	opmatch_t **matchptr = NULL, **nextptr;

	for (i = 0; i < pe_attr->n_instrs; i++) {
		instr = pe_attr->instrs + i;
		matchptr = &(instr->opmatch);
		
		while ((*matchptr) != NULL) {
			nextptr = &((*matchptr)->next); 
			if ((*matchptr)->spec == tstamp) {
				#ifdef DEBUG_GC
				printf("Removing match spec: %d\n", (*matchptr)->spec);
				#endif
				remove_match(matchptr, instr->n_src, pe_attr);
				
			} else
				matchptr = nextptr;
		}	
	
	
	}

}

/*
void spec_clean(thread_args *pe_attr, int tstamp) {
	int i, j;
	instr_t *instr;

	for (i = 0; i < pe_attr->n_instrs; i++) {
		instr = pe_attr->instrs + i;
		for (j = 0; j < instr->n_src; j++)
			clean_oplist(instr->src+j, tstamp);	
		

	}
}
*/

void send_dispatch(dispatch_t *disp, int pe_id, thread_args *pe_attr) {
	dispsnd_t *dispmsg = dispsnd_alloc(pe_attr);
//	printf("Sending dispatch to %d\n", pe_id);
	dispmsg->disp = disp;
	dispmsg->type = MSG_DISPSND;
	comm_send(comm_buffer + pe_id, (qelem)dispmsg);

}

void send_gc_markers(int tstamp, int n_threads) {
	int i;
	marker_t *marker;
	for (i=0; i<n_threads; i++) {
		marker = (marker_t *)malloc(sizeof(marker_t));
		marker->tag = tstamp;
		marker->type = MSG_GC;
		comm_send(comm_buffer + i, (qelem)marker);
	}


}


void add_wait_counter(wcounters_list_t *counters, int tstamp, int count, int n_threads) {
	waitcounter_t *wcounter;
	#ifdef DEBUG_GC
	printf("Adding wait counter with timestamp %d and init value %d\n", tstamp, count);
	#endif 
	if (count == 0) {
	
		//ready for GC, don't even need to add a counter
		send_gc_markers(tstamp, n_threads);
		#ifdef DEBUG_GC
		printf("Operands with timestamp %d can be removed by GC. No need for a counter.\n", tstamp);
		#endif

	} else 	{//TODO: adapt to message passing
		wcounter = (waitcounter_t *)malloc(sizeof(waitcounter_t));
		wcounter->tstamp = tstamp;
		wcounter->count = count;
		wcounter->next = NULL;

		if (counters->head == NULL)
			counters->head = counters->tail = wcounter;
		else {
			(counters->tail)->next = wcounter;
			counters->tail = wcounter;
		}
	}


}
void dec_wait_counter(wcounters_list_t *counters, int tstamp, int n_threads) {
	/* Decrement the counter of the tstamp reexecution, indicating that the current commit instruction has committed and has received a wait with timestamp tstamp. If the corresponding counter becomes 0, all speculative operands with timestamp tstamp or less than timestamp, but with the same exectag and tag, can be cleaned. */
	waitcounter_t *ptr, *prev;
	//TODO: adapt to message passing
	#ifdef DEBUG_GC
	printf("Decrementing wait counter for timestamp %d\n", tstamp);
	#endif
	prev = ptr = counters->head;
	while (ptr->tstamp != tstamp) {
		prev = ptr;
		ptr = ptr->next; //can't reach a NULL value because the counter of tstamp must have been added to the list and can't have been removed yet.
	
	}
	//We don't need to use atomic operations to decrement the counter, because this function is only called inside the Commit instructions.
	if (--(ptr->count) == 0) {
		prev->next = ptr->next;
		free(ptr);
		send_gc_markers(tstamp, n_threads);
	//	enqueue((qelem)tstamp, garbage_specs); //no need for a lock on garbage_specs because only one commit executes at once
		#ifdef DEBUG_GC
		printf("Operands with timestamp %d can be removed by GC\n", tstamp);
		#endif
	}
	

}



int get_notnull_spec(oper_t **oper, int len) {
	int i;

	for (i = 0; i < len; i++)
		if (oper[i]->spec != 0)
			return(oper[i]->spec);

	return(0);

}



/*
oper_t * get_oper(oper_t **oplist, int tag) {
	//TODO; remove from list
	oper_t *op_ptr, *op_ret=NULL, **prevptr;
	

	op_ptr = *oplist;
	prevptr = oplist;
	while (op_ptr != NULL && op_ptr->tag <= tag && op_ret == NULL) {
	
		if (op_ptr->tag == tag) {
			op_ret = op_ptr;
			//ptrret prevptr
			
		
		} else {
			//prevptr = &(op_ptr->next);
			op_ptr = op_ptr->next;
				
		}	

	}
	return(op_ret);
} 
*/
/*
oper_t ** get_oper(oper_t **oplist, int tag, int exectag) {
	//returns the address of the pointer to the operand(if found), so the operand can be then removed from the list
	//with just one step
	oper_t **ptr;


	ptr = oplist;

	//The first operand with the desired tag and exectag in the linked list is the most recent one, i.e. the one from the most recent speculative (or not) execution
	while (*ptr != NULL && ((*ptr)->tag < tag || 
			( (*ptr)->tag == tag && (*ptr)->exec != exectag ) ) )
		ptr = &((*ptr)->next);

	if ( *ptr == NULL || ((*ptr)->tag != tag) || ((*ptr)->exec != exectag) ) 
		ptr = NULL;
	
	 
		
	return(ptr);


}*/
/*
void remove_oper(oper_t **ptr) {
	// ptr is the address of the pointer to the oper, so to remove the oper from the linked list we just have
	to change the pointer's value(*ptr) to the address of the next operand in the list 
	*ptr = (*ptr)->next;

}
*/


void propagate_oper(instr_t *instr, oper_t result[], thread_args *pe_attr) {

	int i ,j, inputn;
	dispatch_t *dispatch;
	instr_t *target;
	int match_exec_override;
	static int debug_produce = -1;
	if (debug_produce < 0) {
		const char *env = getenv("DF_DEBUG_PRODUCE");
		debug_produce = (env && env[0] != '\0' && env[0] != '0') ? 1 : 0;
	}
	for (i = 0; i < instr->n_dst; i++)
		if (instr->port_enable[i]) 
			for (j = 0; j < instr->dst_len[i]; j++) {
				target = instr->dst[i][j].instr;
				inputn = instr->dst[i][j].dstn;
		
				#ifdef DEBUG_OP_SEND
				printf("Propagarei para %d (spec %d - value %d) entrada %d -- pe: %d\n", target->opcode,result[i].spec, result[0].value.i, inputn, target->pe_id);
				#endif
				if (debug_produce) {
					fprintf(stderr, "[prod ] id=%d op=%d -> id=%d op=%d port=%d tag=%d exec=%d val=%lld\n",
					        instr->id, instr->opcode, target->id, target->opcode, inputn, result[i].tag,
					        result[i].exec, (long long)result[i].value.li);
				}
				if (debug_trace_id(target->id)) {
					fprintf(stderr, "[trace] in id=%d op=%d port=%d tag=%d exec=%d val=%lld\n",
					        target->id, target->opcode, inputn, result[i].tag, result[i].exec,
					        (long long)result[i].value.li);
				}
				if (debug_trace_port_match(target->id, inputn)) {
					fprintf(stderr, "[trace-port] send id=%d op=%d port=%d tag=%d exec=%d val=%lld\n",
					        target->id, target->opcode, inputn, result[i].tag, result[i].exec,
					        (long long)result[i].value.li);
				}
				if (target->pe_id == pe_attr->id) {
					//bypass_oper(target->src+inputn, result[i]);	
					{
						oper_t *opcopy = oper_alloc(pe_attr);
						*opcopy = result[i];
						opcopy->owner_token = NULL;
						opcopy->next = NULL;
						debug_match_target_id = target->id;
						debug_match_target_op = target->opcode;
						match_exec_override = (ignore_exec_match ? 0 : -1);
						int tag_mode = (ignore_tag_match ? 0 : -1);
						int exec_mode = (ignore_exec_match ? 0 : -1);
						if (target->opcode == OP_VALTOTAG && !ignore_tag_match) {
							if (inputn == 1 && valtag_is_execonly_oper1(opcopy)) {
								tag_mode = 0;
							} else if (inputn == 0 && valtag_execonly_bucket(target, opcopy->exec)) {
								tag_mode = 0;
							}
						}
						bypass_oper(&(target->opmatch), inputn, opcopy, pe_attr,
						            tag_mode, exec_mode);
						if (target->opcode == OP_VALTOTAG &&
						    inputn == 1 && valtag_is_execonly_oper1(opcopy)) {
							valtag_promote_execonly_op0(target, opcopy->exec, pe_attr);
						}
						debug_match_target_id = -1;
						debug_match_target_op = -1;
					}
					int tag_exec = (ignore_tag_match ? 0 : result[i].tag);
					int exec_exec = (ignore_exec_match ? 0 : result[i].exec);
					if (target->opcode == OP_VALTOTAG && !ignore_tag_match) {
						if (inputn == 1 && valtag_is_execonly_oper1(&result[i])) {
							tag_exec = 0;
						} else if (inputn == 0 && valtag_execonly_bucket(target, result[i].exec)) {
							tag_exec = 0;
						}
					}
					if ((dispatch = can_exec(target,
					                         tag_exec,
					                         exec_exec,
					                         pe_attr))) {
						readyq_enqueue(pe_attr, (qelem)dispatch);
						}
			
				} else {
					inter_pe_send(target->pe_id, target, inputn, result[i], pe_attr);
				}
		
			
			}
	

}


dispatch_t * can_exec(instr_t *instr, int tag, int exectag, thread_args *pe_attr) {
	int i;
	dispatch_t *disp;
	opmatch_t **matchptr = &(instr->opmatch), *match;
	int debug_ret = getenv("DF_DEBUG_RET") != NULL;

	if (instr->opmatch == NULL) {
		return NULL;
	}
	
	disp = NULL;

	while (*matchptr &&
	       ((tag > (*matchptr)->tag) ||
	        ((tag == (*matchptr)->tag) && exectag != (*matchptr)->exec))) {
		matchptr = &((*matchptr)->next);
	}

	if (*matchptr == NULL) {
		return NULL;
	}
	
	match = *matchptr;
		if ((tag == match->tag) && (exectag == match->exec))  {
			if (match->count == instr->n_src) {
				if (debug_ret && instr->opcode == OP_RET) {
					fprintf(stderr, "[ret] exec id=%d tag=%d exec=%d count=%d n_src=%d\n",
					        instr->id, match->tag, match->exec, match->count, instr->n_src);
				}
				//printf("Deu match\n");
				disp = dispatch_alloc(pe_attr);
				disp->instr = instr;
				disp->free_disp = 1;	
				for (i = 0; i < MAX_SOURCE; i++) {
					disp->op[i] = NULL;
				}
				for (i = 0; i < instr->n_src; i++) {
					disp->op[i] = match->op[i];
				
				}
			
			if (match->spec == 0)  {
				disp->speculative = 0;
				*matchptr = (*matchptr)->next;
				opmatch_release(pe_attr, match);
			} else
				disp->speculative = match->spec;	
		
		} //else
		//	printf("Nao deu match 1 - count = %d n_src = %d\n", match->count, instr->n_src);

		
	}
	if (debug_ret && instr->opcode == OP_RET) {
		fprintf(stderr, "[ret] miss id=%d tag=%d exec=%d count=%d n_src=%d\n",
		        instr->id, match->tag, match->exec, match->count, instr->n_src);
	}

	return(disp);

}

/*
dispatch_t * can_exec_old(instr_t *instr, int tag, int exectag) {

	oper_t *op, **opptr[MAX_SOURCE]; //TODO: allocate dynamically??
	int i, no_null_found = 1;
	dispatch_t *disp = dispatch_alloc(pe_attr);
 	disp->instr = instr;
	disp->speculative = 0;
	disp->free_disp = 1; //defaults to freeing the dispatch after execution
	for (i = 0; i<instr->n_src && no_null_found; i++) {
		opptr[i] = get_oper(instr->src + i, tag, exectag);

		if (opptr[i] == NULL) {
			no_null_found = 0;
			dispatch_release(pe_attr, disp);
			disp = NULL;
		
		} else { 

			op = *(opptr[i]);
			disp->op[i] = op;
			if (op->spec > disp->speculative) //store the largest spec number
				disp->speculative = op->spec;
		}
		
	}
	if (no_null_found) {
		if (disp->speculative) {
			for (i = 0; i < instr->n_src; i++) 
				//if (!disp->op[i]->isspeculative)
				disp->op[i]->max_match = disp->speculative;
					//If the operand is not speculative we set its timestamp to the latest speculation to guarantee it will be removed by the gc. If another match occurs in the future with due to a new reexecution, this number is updated to the new latest. This operand will be removed by gc because the timestamp of the latest reexecution will be queued for cleaning up, since the speculative oper with that timestamp has not been replaced, which makes this timestamp the wait value of the source instruction's commit.
		} else
			for (i = 0; i<instr->n_src; i++)  
				remove_oper(opptr[i]);
		
	}
	return(disp);
		
}
*/
void inter_pe_send(int pe_id, instr_t *target, int dstn, oper_t oper, thread_args *pe_attr) {
	//TODO: check if placing directly on a queue of tokens(instead of pointers to tokens) is faster

	optoken_t *tk = optoken_alloc(pe_attr);
	
	tk->oper = oper;
	tk->oper.owner_token = NULL;
	tk->oper.next = NULL;
	tk->dst = target;
	tk->dstn = dstn;	
	tk->type = MSG_OPER;
/*	pthread_mutex_lock(&(comm_buff[pe_id].mutex));
	
	enqueue((qelem)tk, &(comm_buff[pe_id].operqueue)); 
	
	pthread_mutex_unlock(&(comm_buff[pe_id].mutex));*/
	#ifdef DEBUG_COMMUNICATION
	printf("Enviando para pe: %d ... lado: %d - %s...\n", pe_id, dstn, (comm_buffer+pe_id)->waiting ?  "esperando" : "livre");
	#endif
	comm_send(comm_buffer + pe_id, (qelem)tk);
	#ifdef DEBUG_COMMUNICATION
	printf("Enviado para(pe: %d)\n", pe_id);
	#endif
}

void add_to_match(opmatch_t *match, int inport, oper_t *oper, thread_args *pe_attr) {
	oper_t *old;
	//if (oper->next != NULL)
	//	printf("Nao eh nulo\n");
	if (match->op[inport] != NULL) {
		old = match->op[inport];
		if (oper->spec > old->spec) {
			oper->next = old;
			match->op[inport] = oper;
			
			if (oper->spec > match->spec)
				match->spec = oper->spec;
		
		} else {
			#ifdef DEBUG_GC
			printf("O novo eh mais velho\n");
			#endif

			if (oper->cleanup != NULL) {
					printf("Cleaning up %d\n", oper->spec);
					oper->cleanup(oper->value.p, 0); 
					//the 0 param indicates that this is not a full cleanup,
					//because we do full cleanups only at garbage collection.
			}

			oper_release(pe_attr, oper);
			
		}

			
	
	
	} else {
		match->op[inport] = oper;
		if (oper->spec > match->spec)
			match->spec = oper->spec;
		(match->count)++;
	}

}
opmatch_t *create_opmatch(int tag, int exec, thread_args *pe_attr) {
	opmatch_t *match = opmatch_alloc(pe_attr);
	int i;

	match->tag = tag;
	match->exec = exec;
	match->count = 0;
	match->spec = 0;
	match->next = NULL;
	for (i = 0; i < MAX_SOURCE; i++)
		match->op[i] = NULL;
	return(match);

}

void bypass_oper(opmatch_t **matchptr, int inport, oper_t *opcopy, thread_args *pe_attr, int match_tag_override, int match_exec_override) {
	opmatch_t *match = *matchptr, *prev = NULL, *newmatch;
	int match_tag = (match_tag_override >= 0) ? match_tag_override : opcopy->tag;
	int match_exec = (match_exec_override >= 0) ? match_exec_override : opcopy->exec;
	int debug_match = getenv("DF_DEBUG_MATCH") != NULL;
	int dbg_id = debug_match_target_id;
	int dbg_op = debug_match_target_op;
	int debug_ret = getenv("DF_DEBUG_RET") != NULL;
	int debug_flow = getenv("DF_DEBUG_FLOW") != NULL;

	if (match == NULL) {
	       	*matchptr = create_opmatch(match_tag, match_exec, pe_attr);
		//(*matchptr)->op[inport] = opcopy;
		add_to_match(*matchptr, inport, opcopy, pe_attr);
		if (debug_flow && (dbg_op == OP_STEER || dbg_op == OP_CALLSND || dbg_op == OP_VALTOTAG || dbg_op == OP_TAGTOVAL)) {
			fprintf(stderr, "[flow] new id=%d op=%d port=%d tag=%d exec=%d val=%lld\n",
			        dbg_id, dbg_op, inport, match_tag, match_exec, (long long)opcopy->value.li);
		}
		if (debug_ret && dbg_op == OP_RET) {
			fprintf(stderr, "[ret] newmatch id=%d port=%d tag=%d exec=%d val=%lld\n",
			        dbg_id, inport, match_tag, match_exec, (long long)opcopy->value.li);
		}
		if (debug_match) {
			fprintf(stderr, "[match] tgt=%d op=%d new tag=%d exec=%d port=%d\n",
			        dbg_id, dbg_op, match_tag, match_exec, inport);
		}
	} else {
		//TODO: Maybe you should do the opposite and put the new one at the head of the list, because the old ones can be kept because of speculation
		while (match != NULL && ( (match_tag >  match->tag) ||
						((match_tag == match->tag) && (match_exec != match->exec) )) ) {

			prev = match;
			match = match->next;
		
		}
	
		if (match != NULL && ( (match_tag == match->tag) && (match_exec == match->exec) ) ) {
			add_to_match(match, inport,  opcopy, pe_attr);
			if (debug_ret && dbg_op == OP_RET) {
				fprintf(stderr, "[ret] add id=%d port=%d tag=%d exec=%d val=%lld count=%d\n",
				        dbg_id, inport, match_tag, match_exec, (long long)opcopy->value.li, match->count);
			}
			if (debug_match) {
				fprintf(stderr, "[match] tgt=%d op=%d add tag=%d exec=%d port=%d count=%d\n",
				        dbg_id, dbg_op, match_tag, match_exec, inport, match->count);
			}
		
		} else {
			//newmatch =(opmatch_t *)malloc(sizeof(opmatch_t));
			newmatch = create_opmatch(match_tag, match_exec, pe_attr);
			
			add_to_match(newmatch, inport, opcopy, pe_attr);
			if (debug_ret && dbg_op == OP_RET) {
				fprintf(stderr, "[ret] ins id=%d port=%d tag=%d exec=%d val=%lld\n",
				        dbg_id, inport, match_tag, match_exec, (long long)opcopy->value.li);
			}
			if (debug_match) {
				fprintf(stderr, "[match] tgt=%d op=%d ins tag=%d exec=%d port=%d\n",
				        dbg_id, dbg_op, match_tag, match_exec, inport);
			}
			if (prev!=NULL) {
		
				prev->next = newmatch;
				newmatch->next = match;
			} else {
				*matchptr = newmatch;
				newmatch->next = match;
			}


		}
	
	
	
	}



} 

/*
void bypass_oper_old(oper_t **oplist, oper_t oper) {
	
	oper_t *ptr = *oplist, *prev = NULL, *opcopy;
	opcopy = (oper_t *)malloc(sizeof(oper_t));

	*opcopy = oper; //each instruction has its own copy of the operands that are sent to it	
	//printf("bypassando oper com tag: %d e value %d\n", oper.tag, oper.value);


	if (ptr == NULL) 
		*oplist	= opcopy;
	
	else {
		//TODO: return error if two operands with the same tags are received.
		while (ptr != NULL && ( (opcopy->tag > ptr->tag) || 
				((opcopy->tag == ptr->tag) && (opcopy->exec != ptr->exec)) ) ) {
				
			prev = ptr;
			ptr = ptr->next;

		}
		if ((ptr != NULL) && (opcopy->tag == ptr->tag) && (opcopy->exec == ptr->exec)) { //same operand, different executions
			if (opcopy->spec > ptr->spec) { //opcopy is from a more recent execution
				if (prev!=NULL) {
					prev->next = opcopy;
					opcopy->next = ptr;
				}
				else	{
					*oplist = opcopy;
					opcopy->next = ptr;
				}
				//opcopy->next = ptr->next;
				//old = ptr;
			} else {
				printf("O novo eh mais velho\n");
				//oldd = opcopy;
				if (opcopy->cleanup != NULL) {
					printf("Cleaning up %d\n", opcopy->spec);
					opcopy->cleanup(opcopy->value.p);
				}
				free(opcopy);
			}
		
		} else {
			prev->next = opcopy;
			opcopy->next = ptr;
		}		
		
	}

}
*/
void dispandtx_cleanup(void *ptr, int islast) {
	int i;
	oper_t **oper;
	dispandtx_t *disptx = (dispandtx_t *)ptr;
	dispatch_t *disp = disptx->disp;
	#ifdef DEBUG_GC
	//printf("Freeing disp %x tx %x no disptx %x\n", disptx->disp, disptx->tx, disptx); fflush(stdout);
	#endif
	
	if (islast && disp->speculative == 0) { 
	//Is the last one in the opmatch structure, which means we can do a full cleanup. It may not be the last one when the commit receives multiple commit messages for the same instance, due to reexecutions. Also, we have to check if disp->speculative == 0, because otherwise the operands would still be in the instruction's opmatch and, hence, would be removed by garbage collection on their side.
		oper = disp->op;
		for (i =  0; i < disp->instr->n_src; i++) {
			if (oper[i]->cleanup != NULL) {
				oper[i]->cleanup(oper[i]->value.p, 1);
			}
			oper_release(NULL, oper[i]);
		}

			
		
	
	
	}


	free(disptx->disp);
	//stm_exit_thread(disptx->tx);
	tm_cleanup_tx(disptx->tx);
	free(disptx);
	
}
//instr_t create_instruction(int opcode, 

void initialize_threads(thread_args *args, int n_threads, FILE *fp, FILE *pla) {
	int i;

	int *placement;
	int pla_inst_count;
	int *instrs_per_pe;

	int pla_inst;
	
	if (!fscanf(pla, "%d\n", &pla_inst_count)) {
		fprintf(stderr, "Error reading placement file\n");
		exit(1);
	}
	placement = (int *) malloc(pla_inst_count*sizeof(int));
	if (!placement) {
		fprintf(stderr, "Error allocating placement array\n");
		exit(1);
	}
	instrs_per_pe = (int *)calloc(n_threads, sizeof(int));
	if (!instrs_per_pe) {
		fprintf(stderr, "Error allocating instrs_per_pe\n");
		exit(1);
	}

	for (pla_inst=0; pla_inst<pla_inst_count; pla_inst++) {
		if (!fscanf(pla, "%d\n", &placement[pla_inst])) {
			fprintf(stderr, "Error reading placement file\n");
			exit(1);
		}
		if (placement[pla_inst] < 0 || placement[pla_inst] >= n_threads) {
			fprintf(stderr, "Invalid placement entry: %d\n", placement[pla_inst]);
			exit(1);
		}
		instrs_per_pe[placement[pla_inst]]++;
	}



	
			//TODO: automatize placement

	//args[0].instrs = (instr_t *)malloc(3*sizeof(instr_t))
	
	//args[1].instrs = (instr_t *)malloc(2*sizeof(instr_t))

	/*dispatch_t *disp1, *disp2;

	disp1 = (dispatch_t *)malloc(sizeof(dispatch_t));
	
	disp2 = (dispatch_t *)malloc(sizeof(dispatch_t));
*/
	for (i = 0; i < n_threads; i++) {
		args[i].id = i;
		init_queue(&(args[i].ready_queue));
		pthread_mutex_init(&args[i].ready_mutex, NULL);
		init_combuff(comm_buffer + i);	
		args[i].n_edges = n_threads - 1;
		args[i].n_threads = n_threads;
		args[i].instrs_cap = instrs_per_pe[i];
		args[i].instrs = (instr_t *)calloc((size_t)args[i].instrs_cap, sizeof(instr_t));
		if (!args[i].instrs) {
			fprintf(stderr, "Error allocating instrs for PE %d\n", i);
			exit(1);
		}
		args[i].n_instrs = 0;
		args[i].global_termination = 0;
		args[i].termination_tag = 0;
		args[i].termination_count = 0;
		args[i].isidle = 0;
		args[i].optoken_free = NULL;
		args[i].dispsnd_free = NULL;
		args[i].dispatch_free = NULL;
		args[i].oper_free = NULL;
		args[i].opmatch_free = NULL;

	}


	loader(placement, args, fp);
	free(instrs_per_pe);
	free(placement);



	/*args[0].instrs[0].opcode = OP_CONST;

	args[0].instrs[0].dst[1][0] = args[0].instrs+1;
	args[0].instrs[0].dst_len[0] = 0;
	args[0].instrs[0].dst_len[1] = 1;
	args[0].instrs[0].immed=5;


	args[0].instrs[2].opcode = OP_CONST;
	args[0].instrs[2].dst[0][0] = args[0].instrs+1;;
	args[0].instrs[2].dst_len[0] = 1;
	args[0].instrs[2].dst_len[1] = 0;
	args[0].instrs[2].immed = 4;


	args[0].instrs[1].opcode = OP_ADD;
	args[0].instrs[1].dst_len[0] = 0;
	args[0].instrs[1].dst_len[1] = 0;


	disp1->instr = args[0].instrs;
	disp2->instr = args[0].instrs+2;
	enqueue((qelem)disp1, &(args[0].ready_queue));
	enqueue((qelem)disp2, &(args[0].ready_queue));*/





/*
	args[1].instrs[0].op = OP_CONST;

	args[1].instrs[0].src1 = 7;
	args[1].instrs[0].dst = 3;





	args[1].instrs[1].op = OP_ADD;
	args[1].instrs[1].src1=3;
	args[1].instrs[1].src2=2;

	args[1].instrs[1].dst=4;

*/

}
