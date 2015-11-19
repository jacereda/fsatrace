HELPER_SRCS=win/fsatracehelper.c
SRCS32=win/fsatracedll.c win/inject.c win/patch.c win/hooks.c win/emit.c win/handle.c win/utf8.c win/dbg.c win/inject.c
SRCS64=$(SRCS32) win/inject.c
HELPER_OBJ=win/fsatracehelper32.o
OBJS64=$(patsubst %.c,%.o,$(SRCS64))
OBJS32=$(patsubst %.c,%32.o,$(SRCS32))
DEPS64=$(patsubst %.o,%.d,$(OBJS64))
DEPS32=$(patsubst %.o,%.d,$(OBJS32) $(HELPER_OBJ))
LIBS=-lntdll -lpsapi -lkernel32 -lmsvcrt

lib: fsatrace32.dll fsatrace64.dll fsatracehelper.exe

%32.o: %.c
	$(CC32) -c $(CPPFLAGS32) $(CFLAGS) $< -o $@

fsatracehelper.exe: $(HELPER_OBJ)
	$(CC32) $< -o $@

fsatrace64.dll: $(OBJS64)
	$(CC) -shared $(LDFLAGS64) $^ -o $@ $(LIBS)

fsatrace32.dll: $(OBJS32)
	$(CC32) -shared $(LDFLAGS32) $^ -o $@ $(LIBS)

libinstall: fsatracehelper.exe fsatrace64.dll fsatrace32.dll
	cp fsatracehelper.exe fsatrace64.dll fsatrace32.dll $(INSTALLDIR)

cleanlib:
	rm -f fsatracehelper.exe fsatrace64.dll fsatrace32.dll $(OBJS64) $(OBJS32) $(DEPS64) $(DEPS32)

-include $(DEPS64)
-include $(DEPS32)
