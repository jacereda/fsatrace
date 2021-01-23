ifeq ($(OS), Windows_NT)

PLAT=win
EXE=.exe
ROOT64=$(shell stack path --programs)\..\x86_64-windows\ghc-8.6.5\mingw
ROOT32=$(shell stack path --programs)\..\i386-windows\ghc-8.6.5\mingw
CC=$(ROOT64)\bin\gcc
CC32=$(ROOT32)\bin\gcc
CPPFLAGS=-D_WIN32_WINNT=0x600 -DIS32=0 -isystem$(ROOT64)\x86_64-w64-mingw32\include\ddk
CPPFLAGS32=-D_WIN32_WINNT=0x600 -DIS32=1 -isystem$(ROOT32)\i686-w64-mingw32\include\ddk
OSSRCS=src/win/inject.c src/win/dbg.c
LDOBJS=$(ROOT64)\x86_64-w64-mingw32\lib\CRT_noglob.o
INSTALLDIR=$(APPDATA)\local\bin

else

PLAT=unix
CPPFLAGS=-D_GNU_SOURCE -D_DEFAULT_SOURCE=1
LDFLAGS=-g

OS=$(shell uname -s)
LS=$(shell which ls)

ifeq ($(OS),Linux)
LDLIBS=-ldl -lrt
endif

INSTALLDIR=$(HOME)/.local/bin

endif

CFLAGS+= -g -std=c99 -Wall -O2 -fomit-frame-pointer -fno-stack-protector -MMD

SRCS=src/fsatrace.c src/$(PLAT)/proc.c src/$(PLAT)/shm.c $(OSSRCS)

all: fsatrace$(EXE) lib fsatest$(EXE) fsatest32$(EXE)

fsatrace$(EXE): $(patsubst %.c,%.o,$(SRCS))
	$(CC) $(LDFLAGS) $(LDOBJS) $^ $(LDLIBS) -o $@

fsatest$(EXE): src/fsatest.o
	$(CC) $^ -o $@

dumpargs$(EXE): dumpargs.o
	$(CC) $(LDFLAGS) $^ $(LDLIBS) -o $@

install: fsatrace$(EXE) libinstall
	cp $< $(INSTALLDIR)

clean: cleanlib
	rm -f fsatrace$(EXE) fsatest$(EXE) fsatest32$(EXE) $(patsubst %.c,%.o,$(SRCS)) $(patsubst %.c,%.d,$(SRCS))

TEST_CC_CMD=$(CC) -c -D_GNU_SOURCE -D_DEFAULT_SOURCE=1 -std=c99 src/fsatrace.c -o /tmp/fsatrace.o
test: all
	./fsatrace$(EXE) ewrmdqt - -- cp $(LS) /tmp/foo
	./fsatrace$(EXE) ewrmdqt - -- mv -f /tmp/foo /tmp/bar
	./fsatrace$(EXE) ewrmdqt - -- gzip -f /tmp/bar
	./fsatrace$(EXE) ewrmdqt - -- touch /tmp/bar
	./fsatrace$(EXE) ewrmdqt - --- sh -c "for _ in range {1..10}; do touch /tmp/bar; done"
	./fsatrace$(EXE) ewrmdqt - -- rm -f /tmp/bar
	./fsatrace$(EXE) ewrmdqt - -- $(TEST_CC_CMD)
	./fsatrace$(EXE) ewrmdqt - -- sh -c "cp $(LS) /tmp/foo && mv -f /tmp/foo /tmp/bar && rm -f /tmp/bar"
	./fsatrace$(EXE) ewrmdqt - -- sh -c "cp $(LS) /tmp/foo && mv -f /tmp/foo /tmp/bar && rm -f /tmp/bar" # twice, when dst exists it might use another path
	# Test buffer size overwrite, since default buffer size is not large enough
	# for the amount of output from the command.
	env FSAT_BUF_SIZE=2000000 ./fsatrace$(EXE) ewrmdqt /dev/null -- sh -c "for _ in {1..100}; do $(TEST_CC_CMD); done"

htest: all
	cd test && stack install && stack test

benchmark: all
	for _ in {1..5}; do time -f 'Elapsed wall time: %E' env FSAT_BUF_SIZE=4000000 ./fsatrace$(EXE) ewrmdqt /dev/null -- sh -c "for _ in {1..200}; do $(TEST_CC_CMD); done"; done


-include $(patsubst %.c,%.d,$(SRCS))

include $(PLAT).mk
