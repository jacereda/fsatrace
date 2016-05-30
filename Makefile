ifeq ($(OS), Windows_NT)

PLAT=win
EXE=.exe
ROOT64=$(LOCALAPPDATA)\Programs\stack\x86_64-windows\ghc-7.10.2\mingw
ROOT32=$(LOCALAPPDATA)\Programs\stack\i386-windows\ghc-7.10.2\mingw
CC=$(ROOT64)\bin\gcc
CC32=$(ROOT32)\bin\gcc
CPPFLAGS=-D_WIN32_WINNT=0x600 -isystem$(ROOT64)\x86_64-w64-mingw32\include\ddk
CPPFLAGS32=-D_WIN32_WINNT=0x600 -isystem$(ROOT32)\include\ddk
OSSRCS=src/win/inject.c src/win/dbg.c
LDOBJS=$(ROOT64)\x86_64-w64-mingw32\lib\CRT_noglob.o
INSTALLDIR=$(APPDATA)\local\bin
endif

OS=$(shell uname -s)

ifeq ($(OS), Darwin)
PLAT=darwin
CPPFLAGS= -D_GNU_SOURCE -D_BSD_SOURCE=1 -D_DARWIN_NO_64_BIT_INODE=1
LDLIBS=
INSTALLDIR=$(HOME)/.local/bin
OSSRCS=src/unix/proc.c src/unix/shm.c
endif

ifeq ($(OS), Linux)
PLAT=unix
CPPFLAGS= #-D_GNU_SOURCE -D_BSD_SOURCE=1
LDLIBS=-ldl -lrt
INSTALLDIR=$(HOME)/.local/bin
endif

CFLAGS+= -g -std=c99 -Wall -O2 -fomit-frame-pointer -fno-stack-protector -MMD

SRCS=src/fsatrace.c $(OSSRCS)

all: fsatrace$(EXE) lib

fsatrace$(EXE): $(patsubst %.c,%.o,$(SRCS))
	$(CC) $(LDLAGS) $(LDOBJS) $^ $(LDLIBS) -o $@

dumpargs$(EXE): dumpargs.o
	$(CC) $(LDLAGS) $^ $(LDLIBS) -o $@

install: fsatrace$(EXE) libinstall
	cp $< $(INSTALLDIR)

clean: cleanlib
	rm -f fsatrace$(EXE) $(patsubst %.c,%.o,$(SRCS)) $(patsubst %.c,%.d,$(SRCS))

test: all
	./fsatrace$(EXE) wrmdqt - -- cp /bin/ls /tmp/foo
	./fsatrace$(EXE) wrmdqt - -- mv /tmp/foo /tmp/bar
	./fsatrace$(EXE) wrmdqt - -- touch /tmp/bar
	./fsatrace$(EXE) wrmdqt - -- rm /tmp/bar
	./fsatrace$(EXE) wrmdqt - -- sh -c "cp /bin/ls /tmp/foo && mv /tmp/foo /tmp/bar && rm /tmp/bar"
	./fsatrace$(EXE) wrmdqt - -- cc -c -D_GNU_SOURCE -D_BSD_SOURCE=1 -std=c99 -Wall src/fsatrace.c -o /tmp/fsatrace.o

htest: all
	cd test && stack test


-include $(patsubst %.c,%.d,$(SRCS))

include $(PLAT).mk
