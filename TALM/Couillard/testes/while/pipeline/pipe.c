#BEGINBLOCK
#include <stdio.h>
#include <stdlib.h>

FILE * pfile;
#ENDBLOCK

int main()
{
	int a=0,b,c=0,d;
	int i=0;
	
	/*init*/
	super single input(a) output(a)
	#BEGINSUPER
		printf ("%s\n", superargv[0]);
		//pfile = fopen(superargv[0],"r");
	#ENDSUPER

	/*pipe*/
	while (i<10)
	{
		/* Estagio de leitura */
		super single input(a,i) output(a,i)
		#BEGINSUPER
			i = i + 1;
			//fscanf(pfile, "%d\n", &a);
			a = i + 2;
		#ENDSUPER

		/* Estagio de processamento */
		super single input(a) output(b)
		#BEGINSUPER
			printf("Estagio do meio %d\n", a);
			b = a + 3;
		#ENDSUPER

		/* Estagio de saida */
		super single input(b,c) output(c)
		#BEGINSUPER
			printf("Ultimo estagio %d\n", b, c);
			c = b + 4;
		#ENDSUPER
	}

	/* finalização */
	super single input(c) output(d)
	#BEGINSUPER
		printf("Saida: %d\n", c);

	#ENDSUPER
	
	return 0;
}
