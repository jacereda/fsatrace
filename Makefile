ifeq ($(OS), Windows_NT)
PLAT=win
ROOT64=$(HOMEPATH)\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.2\mingw
ROOT32=$(HOMEPATH)\AppData\Local\Programs\stack\i386-windows\ghc-7.10.2\mingw
CC=$(ROOT64)\bin\gcc
CC32=$(ROOT32)\bin\gcc
OSSRCS=win/inject.c win/dbg.c
LIBS=kernel32.lib
else
PLAT=unix
endif

CFLAGS=-Wall -MMD

SRCS=fsatrace.c $(PLAT)/proc.c $(PLAT)/shm.c $(OSSRCS)

all: fsatrace$(EXE) lib

fsatrace$(EXE): $(patsubst %.c,%.o,$(SRCS))

clean: cleanlib
	rm -f fsatrace$(EXE) $(patsubst %.c,%.o,$(SRCS)) $(patsubst %.c,%.d,$(SRCS))


-include $(patsubst %.c,%.d,$(SRCS))
include $(PLAT).mk
