//#BEGINBLOCK
#include <stdlib.h>
#include <stdio.h>

int * a;
int * b;
int * c;
//#ENDBLOCK

int main()
{
	int tam = 10000000;
	int x,y;
	int z;
	/* init */
	//super single input(tam) output(tam)
	//#BEGINSUPER
		a=(int *) malloc(tam*sizeof(int));
		b=(int *) malloc(tam*sizeof(int));
		c=(int *) malloc(tam*sizeof(int));
	//#ENDSUPER

	//super parallel input(tam) output(x)
	//#BEGINSUPER
	//	int threadid = treb_get_tid();
	//	int nthreads = treb_get_n_tasks();
		int i;
		for (i=0; i<tam; i++)
		{
			a[i]=i;
			b[i]=i+1;
		}
	//#ENDSUPER

	/* calc */
	//super parallel input(x::mytid,tam) output(y)
	//#BEGINSUPER
	//	int threadid = treb_get_tid();
	//	int nthreads = treb_get_n_tasks();

		for (i=0; i<tam; i++)
		{
			c[i]=a[i]+b[i];
		}
	//#ENDSUPER

	/* out */
	//super single input (tam,y::*) output(z)
	//#BEGINSUPER
		for (i=0; i<tam; i++)
		{
			printf("%d, ", c[i]);
		}
	//#ENDSUPER
		
}
