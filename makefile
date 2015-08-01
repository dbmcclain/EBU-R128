
#GCC=g++ -dynamiclib -I$(HOME)/projects/include -O2 -msse3 -mfpmath=sse -m64
GCC=g++ -dynamiclib -I$(HOME)/projects/include -Ofast -msse3 -mfpmath=sse -m64 -funroll-loops

all:  hsiir

hsiir: hsiir.c
	$(GCC) -o libHsIIR.dylib hsiir.c \
		-lstdc++ -framework Accelerate
	mv libHsIIR.dylib /usr/local/lib64

