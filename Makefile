ifeq ($(OS), Windows_NT)

PLAT=win
ROOT64=$(HOMEPATH)\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.2\mingw
ROOT32=$(HOMEPATH)\AppData\Local\Programs\stack\i386-windows\ghc-7.10.2\mingw
CC=$(ROOT64)\bin\gcc
CC32=$(ROOT32)\bin\gcc
CPPFLAGS=-D_WIN32_WINNT=0x600 -isystem$(ROOT64)\x86_64-w64-mingw32\include\ddk
CPPFLAGS32=-D_WIN32_WINNT=0x600 -isystem$(ROOT32)\include\ddk
OSSRCS=win/inject.c win/dbg.c
LIBS=kernel32.lib

else

PLAT=unix
CPPFLAGS=-D_GNU_SOURCE -D_BSD_SOURCE=1

OS=$(shell uname -s)
ifeq ($(OS),Linux)
LDLIBS=-ldl -lrt
endif

endif

CFLAGS+= -std=c99 -Wall -O2 -fomit-frame-pointer -fno-stack-protector -MMD

SRCS=fsatrace.c $(PLAT)/proc.c $(PLAT)/shm.c $(OSSRCS)

all: fsatrace$(EXE) lib

fsatrace$(EXE): $(patsubst %.c,%.o,$(SRCS))

clean: cleanlib
	rm -f fsatrace$(EXE) $(patsubst %.c,%.o,$(SRCS)) $(patsubst %.c,%.d,$(SRCS))


-include $(patsubst %.c,%.d,$(SRCS))

include $(PLAT).mk
