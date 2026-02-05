TALMHOME=${HOME}/mthings/Doutorado/Dataflow

CFLOW=python ${TALMHOME}/CFlow/c.py
FLOWASM=python ${TALMHOME}/interp/asm/assembler.py


FLAGS=-O2 -g
INCLUDES=-I${HOME}/mthings/Doutorado/Dataflow/interp/include


NUMTHREADS=2
PROGRAM_NAME=pipe


#INCLUDES+=-I${HOME}/mthings/Mestrado/Dataflow/tinySTM-0.9.9/include

%.o: %.c
	gcc ${FLAGS} -fPIC -c $< ${INCLUDES}


TARGET = ${PROGRAM_NAME}_lib.o ${PROGRAM_NAME}.flb

all:    $(TARGET) 
	gcc ${FLAGS} -shared -Wl,-soname,libinterp-blocks.so -o libinterp-blocks.so *.o

${PROGRAM_NAME}.flb: ${PROGRAM_NAME}.fl
	${FLOWASM} -n ${NUMTHREADS} ${PROGRAM_NAME}.fl -o ${PROGRAM_NAME}

${PROGRAM_NAME}_lib.c: 
	${CFLOW} ${PROGRAM_NAME}.c

${PROGRAM_NAME}.fl: ${PROGRAM_NAME}.c
	${CFLOW} ${PROGRAM_NAME}.c


clean: 
	rm *.flb *.fl *.pla *.o *_lib.c libinterp-blocks.so


cflow: ${PROGRAM_NAME}.fl


bytecode: ${PROGRAM_NAME}.flb
