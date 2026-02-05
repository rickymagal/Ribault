#BEGINBLOCK
#include <stdio.h>
#include <stdlib.h>

#ENDBLOCK

int main()
{
	int a=1,b=2,c=3, d;
	int i=0;
	int n = 10;
	/*init*/
	while (i < n) {
	       	i = i + 1;	
		super single input(a) output(a)
		#BEGINSUPER
		//pfile = fopen("numeros.txt","r");
			printf("Inicializacao\n");
		#ENDSUPER

		/* Estagio de leitura */
		super single input(a) output(b)
		#BEGINSUPER
			fscanf(pfile, "%d\n", &a);
		#ENDSUPER

		super single input(b) output(c)
		#BEGINSUPER
			fscanf(pfile, "%d\n", &a);
		#ENDSUPER

	}	

	super single input(c) output(d)
	#BEGINSUPER
		fclose(pfile);
	#ENDSUPER
	
	return 0;
}
