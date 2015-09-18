all: fsatrace fsatrace.so

fsatrace: fsatraceunix.c
	cc -g -O0 -Wall fsatraceunix.c -o fsatrace

fsatrace.so: fsatrace.c hooks.h
	cc -O3 -shared -fPIC -Wall fsatrace.c -o fsatrace.so
