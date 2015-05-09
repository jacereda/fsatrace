
fsatrace.so: fsatrace.c hooks.h
	cc -O3 -shared -fPIC -Wall $< -o $@ 
