OS=$(shell uname -s)
ifeq ($(OS),Linux)
LIBS=-lrt
else
LIBS=
endif	

all: fsatrace fsatrace.so

fsatrace: fsatraceunix.c
	cc -O3 -Wall fsatraceunix.c -o fsatrace $(LIBS)

fsatrace.so: fsatrace.c hooks.h
	cc -O3 -shared -fPIC -Wall fsatrace.c -o fsatrace.so $(LIBS)

clean:
	rm -f fsatrace fsatrace.so

