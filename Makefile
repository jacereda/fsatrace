OS=$(shell uname -s)
ifeq ($(OS),Linux)
PRGLIBS=-lrt
SOLIBS=-ldl -lrt
else
LIBS=
SOLIBS=
endif	

CC=cc -D_GNU_SOURCE -D_BSD_SOURCE=1 -std=c99 -Wall -O3

all: fsatrace fsatrace.so

fsatrace: fsatraceunix.c
	$(CC) fsatraceunix.c -o fsatrace $(PRGLIBS)

fsatrace.so: fsatrace.c hooks.h
	$(CC) -shared -fPIC fsatrace.c -o fsatrace.so $(SOLIBS)

clean:
	rm -f fsatrace fsatrace.so

