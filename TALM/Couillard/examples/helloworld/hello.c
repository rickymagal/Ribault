#BEGINBLOCK
#include <stdlib.h>
#include <stdio.h>

#ENDBLOCK

int main() {
	treb_parout int threadid;
	float pi_approx=3.14;
	

	treb_super parallel input(pi_approx) output(threadid)
	#BEGINSUPER
		threadid = treb_get_tid();
		int nthreads = treb_get_n_tasks();
		printf("Hello World. I'm super %d and pi is approximately: %f\n", threadid, pi_approx); 

	#ENDSUPER


	/* out */
	treb_super single input (threadid::2) output(pi_approx)
		#BEGINSUPER
		printf("Hello, I run only after parallel super %d has finished.\n", threadid); 
		pi_approx=3.1415; //overwrite the variable num

	#ENDSUPER
		
}
