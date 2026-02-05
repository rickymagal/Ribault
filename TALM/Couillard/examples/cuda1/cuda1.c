#BEGINBLOCK
extern float calcpi(void);
#ENDBLOCK

int main() {
	float pi = 2;


	treb_super single output (pi)
	#BEGINSUPER
		pi = calcpi();
		printf("pi calculado pela gpu %f\n", pi);
	#ENDSUPER



}
