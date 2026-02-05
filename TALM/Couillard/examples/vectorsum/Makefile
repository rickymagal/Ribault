NUMTHREADS=4

PROGRAM_NAME=vectorsum

TALM_HOME=${HOME}/TALM
TREBUCHET_HOME=${TALM_HOME}/interp

TREBUCHET_BIN=${TREBUCHET_HOME}/interp


COUILLARD_HOME=${TALM_HOME}/Couillard

CFLOW=python ${COUILLARD_HOME}/c.py


FLOWASM=python ${TALM_HOME}/asm/assembler.py


FLAGS=-O2 -g
INCLUDES=-I${TREBUCHET_HOME}/include

TREBLIB=libinterp-blocks.so 


%.o: %.c
	gcc ${FLAGS} -fPIC -c $< ${INCLUDES}


#TARGET = ${TREBLIB} ${PROGRAM_NAME}.flb

all: ${TREBLIB} ${PROGRAM_NAME}.flb

${TREBLIB}: ${PROGRAM_NAME}_lib.o
	gcc ${FLAGS} -shared -Wl,-soname,libinterp-blocks.so -o libinterp-blocks.so *.o



${PROGRAM_NAME}.flb: ${PROGRAM_NAME}.fl
	${FLOWASM} -n ${NUMTHREADS} ${PROGRAM_NAME}.fl -o ${PROGRAM_NAME}

${PROGRAM_NAME}_lib.c: ${PROGRAM_NAME}.c
	${CFLOW} ${PROGRAM_NAME}.c

${PROGRAM_NAME}.fl: ${PROGRAM_NAME}.c
	${CFLOW} ${PROGRAM_NAME}.c


.PHONY: cflow bytecode lib clean

cflow: 
	${CFLOW} ${PROGRAM_NAME}.c

bytecode: 
	${FLOWASM} -n ${NUMTHREADS} ${PROGRAM_NAME}.fl -o ${PROGRAM_NAME}

lib:	${TREBLIB}

clean: 
	rm *.flb *.fl *.pla *.o *_lib.c libinterp-blocks.so

