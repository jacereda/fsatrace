OS=$(shell uname -s)
ifeq ($(OS),Linux)
PRGLIBS=-lrt
SOLIBS=-ldl -lrt
else
LIBS=
SOLIBS=
endif	

all: fsatrace fsatrace.so

fsatrace: fsatraceunix.c
	cc -std=c99 -O3 -Wall fsatraceunix.c -o fsatrace $(PRGLIBS)

fsatrace.so: fsatrace.c hooks.h
	cc -std=c99 -O3 -shared -fPIC -Wall fsatrace.c -o fsatrace.so $(SOLIBS)

clean:
	rm -f fsatrace fsatrace.so

