
#GCC=g++ -dynamiclib -I$(HOME)/projects/include -O2 -msse3 -mfpmath=sse -m64
GCC=g++ -dynamiclib -I$(HOME)/projects/include -Ofast -msse3 -mfpmath=sse -m64 -funroll-loops -mtune=corei7-avx

all:  hsiir

hsiir: hsiir.cpp
	$(GCC) -o libHsIIR.dylib hsiir.cpp \
		-lstdc++ -framework Accelerate
	mv libHsIIR.dylib /usr/local/lib64/libHsIIR.dylib

