OS=$(shell uname -s)
ifeq ($(OS),Linux)
PRGLIBS=-lrt
SOLIBS=-ldl -lrt
endif
ifeq ($(OS),Darwin)
PRGLIBS=
SOLIBS=
endif

CPPFLAGS=-D_GNU_SOURCE -D_BSD_SOURCE=1
CFLAGS=-std=c99 -Wall -pedantic -g -O2 -fomit-frame-pointer -fno-stack-protector -Wno-incompatible-pointer-types -MT $@ -MMD -MP -MF $*.d

SRCS=proc.c shm.c ../fsatrace.c

all: fsatrace fsatrace.so

%.d: ;

-include $(patsubst %.c,%.d,$(SRCS))

fsatrace: $(patsubst %.c,%.o,$(SRCS))
	$(CC) $^ -o fsatrace $(PRGLIBS)

fsatrace.so: fsatraceso.c ../fsatrace.h
	$(CC) -shared -fPIC fsatraceso.c -o fsatrace.so $(SOLIBS)

clean:
	rm -f fsatrace fsatrace.so $(patsubst %.c,%.o,$(SRCS)) $(patsubst %.c,%.d,$(SRCS))

