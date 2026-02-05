#BEGINBLOCK
#include <stdlib.h>
#include <stdio.h>

int * a;
int * b;
int * c;
#ENDBLOCK

int main()
{
	int*k;
	int tam = 100;
	treb_parout int x,y;
	int z;
	/* init */
	treb_super single input(tam) output(tam)
	#BEGINSUPER
		a=(int *) malloc(tam*sizeof(int));
		b=(int *) malloc(tam*sizeof(int));
		c=(int *) malloc(tam*sizeof(int));
	#ENDSUPER

	treb_super parallel input(tam) output(x)
	#BEGINSUPER
		int threadid = treb_get_tid();
		int nthreads = treb_get_n_tasks();
		int i;
		for (i=(threadid*tam/nthreads); i<((threadid+1)*tam/nthreads); i++)
		{
			a[i]=i;
			b[i]=i+1;
		}
	#ENDSUPER

	/* calc */
	treb_super parallel input(x::mytid,tam) output(y)
	#BEGINSUPER
		int threadid = treb_get_tid();
		int nthreads = treb_get_n_tasks();
		int i;
		for (i=(threadid*tam/nthreads); i<((threadid+1)*tam/nthreads); i++)
		{
			c[i]=a[i]+b[i];
		}
	#ENDSUPER

	/* out */
	treb_super single input (tam,y::*) output(z)
	#BEGINSUPER
		int i;
		for (i=0; i<tam; i++)
		{
			printf("%d, ", c[i]);
		}
	#ENDSUPER
		
}
