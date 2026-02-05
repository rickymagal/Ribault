#BEGINBLOCK
#include <stdio.h>
#include <stdlib.h>

FILE * pfile;
#ENDBLOCK

int main()
{
	int a=0,b,c=0,d;
	int i=0, num=10;
	

	/*pipe*/
	while (i<10)
	{
		/* Estagio de leitura */
		super single input(i) output(i)
		#BEGINSUPER
			printf("iteracao do loop %d\n", i);
			i = i + 1;
			//fscanf(pfile, "%d\n", &a);
		#ENDSUPER
	}

	/* finalização */
	super single input(i) output(d)
	#BEGINSUPER
		printf("Sai do loop\n");

	#ENDSUPER
	
	return 0;
}
