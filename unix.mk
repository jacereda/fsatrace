OS=$(shell uname -s)
ifeq ($(OS),Linux)
SOLIBS=-ldl -lrt
endif
ifeq ($(OS),Darwin)
SOLIBS=
endif

CPPFLAGS=-D_GNU_SOURCE -D_BSD_SOURCE=1
CFLAGS=-std=c99 -Wall -pedantic -g -O2 -fomit-frame-pointer -fno-stack-protector -Wno-incompatible-pointer-types -MMD

lib: fsatrace.so

fsatrace.so: unix/fsatraceso.c
	$(CC) -shared -fPIC unix/fsatraceso.c -o fsatrace.so $(SOLIBS)

cleanlib:
	rm -f fsatrace.so fsatraceso.d


-include $(patsubst %.c,%.d,$(SRCS))
