ROOT64=$(HOMEPATH)\AppData\Local\Programs\stack\x86_64-windows\ghc-7.10.2\mingw
ROOT32=$(HOMEPATH)\AppData\Local\Programs\stack\i386-windows\ghc-7.10.2\mingw
CC64=$(ROOT64)\bin\gcc
CC32=$(ROOT32)\bin\gcc
CFLAGS=-O2 -Wall -D_WIN32_WINNT=0x600 -MMD
CFLAGS64=$(CFLAGS) -I$(ROOT64)\x86_64-w64-mingw32\include\ddk
CFLAGS32=$(CFLAGS) -I$(ROOT32)\include\ddk
LD64=$(CC64)
LD32=$(CC32)
LDFLAGS64=
LDFLAGS32=
LIBS=-lntdll -lpsapi -lkernel32 -lmsvcrt

all: fsatrace.exe fsatracehelper.exe fsatrace64.dll fsatrace32.dll


%32.o: %.c Makefile
	$(CC32) -c $(CFLAGS32) $< -o $@


%.o: %.c Makefile
	$(CC64) -c $(CFLAGS64) $< -o $@


fsatrace.exe: ..\fsatrace.o inject.o dbg.o shm.o proc.o
	$(LD64) $(LDFLAGS64) $^ -o $@ $(LIBS)


fsatracehelper.exe: fsatracehelper32.o
	$(LD32) $(LDFLAGS32) $^ -o $@ $(LIBS)


fsatrace64.dll: fsatracedll.o inject.o patch.o hooks.o emit.o handle.o utf8.o dbg.o
	$(LD64) -shared $(LDFLAGS64) $^ -o $@ $(LIBS)


fsatrace32.dll: fsatracedll32.o inject32.o patch32.o hooks32.o emit32.o handle32.o utf832.o dbg32.o
	$(LD32) -shared $(LDFLAGS32) $^ -o $@ $(LIBS)


clean:
	rm -f *.obj *.o ../*.o *.dll *.exe *.d


-include $(shell echo *.d)
