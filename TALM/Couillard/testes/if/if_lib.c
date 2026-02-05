#include "queue.h"
#include "interp.h"
extern int superargc;
extern char ** superargv;
super1(oper_t **oper, oper_t *result){
	int b  = oper[0]->value.i;

	
	result[0].value.i = b;
}
