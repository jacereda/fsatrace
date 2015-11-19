ifeq ($(OS), Windows_NT)
PLAT=win
CC=$(HOMEPATH)\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.2\mingw\bin\gcc
else
PLAT=unix
endif

CFLAGS=-Wall -MMD

SRCS=fsatrace.c $(PLAT)/proc.c $(PLAT)/shm.c

all: fsatrace$(EXE) lib

fsatrace$(EXE): $(patsubst %.c,%.o,$(SRCS))

clean: cleanlib
	rm -f fsatrace$(EXE) $(patsubst %.c,%.o,$(SRCS)) $(patsubst %.c,%.d,$(SRCS))


-include $(patsubst %.c,%.d,$(SRCS))
include $(PLAT).mk
