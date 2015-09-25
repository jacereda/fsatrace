OS=$(shell uname -s)
ifeq ($(OS),Linux)
PRGLIBS=-lrt
SOLIBS=-ldl -lrt
endif
ifeq ($(OS),Darwin)
PRGLIBS=
SOLIBS=
endif

CC=cc -D_GNU_SOURCE -D_BSD_SOURCE=1 -std=c99 -Wall -O2 -fomit-frame-pointer -fno-stack-protector -Wno-incompatible-pointer-types 

all: fsatrace fsatrace.so

fsatrace: fsatrace.c fsatrace.h
	$(CC) fsatrace.c -o fsatrace $(PRGLIBS)

fsatrace.so: fsatraceso.c fsatrace.h
	$(CC) -shared -fPIC fsatraceso.c -o fsatrace.so $(SOLIBS)

clean:
	rm -f fsatrace fsatrace.so

