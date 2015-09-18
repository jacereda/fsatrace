OS=$(shell uname -s)
ifeq ($(OS),Linux)
PRGLIBS=
SOLIBS=-ldl
else
LIBS=
SOLIBS=
endif	

all: fsatrace fsatrace.so

fsatrace: fsatraceunix.c
	cc -O3 -Wall fsatraceunix.c -o fsatrace $(PRGLIBS)

fsatrace.so: fsatrace.c hooks.h
	cc -O3 -shared -fPIC -Wall fsatrace.c -o fsatrace.so $(SOLIBS)

clean:
	rm -f fsatrace fsatrace.so

