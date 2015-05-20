
fsatrace.so: fsatrace.c hooks.h
	cc -O3 -shared -fPIC -Wall fsatrace.c -o fsatrace.so
