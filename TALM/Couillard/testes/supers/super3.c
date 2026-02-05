

int main() {
	int a=0, b,c;
	
	super single input(a) output(b)
	#BEGINSUPER
		printf("A: %d \n", a)
		b=a+3;
	#ENDSUPER
	super single input(b) output(c)
	#BEGINSUPER
		printf("B: %d \n", b)
		c=b+5;
	#ENDSUPER
	return 0;
}
