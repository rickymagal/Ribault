#include "queue.h"
#include "interp.h"
extern int superargc;
extern char ** superargv;

#include <stdlib.h>
#include <stdio.h>


void super1(oper_t **oper, oper_t *result){
	int threadid;
