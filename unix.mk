SOSRCS=src/unix/fsatraceso.c src/emit.c src/unix/shm.c src/unix/proc.c

lib: fsatrace.so

%.os: %.c
	$(CC) -c -fPIC $(CPPFLAGS) $(CFLAGS) $< -o $@

fsatrace.so: $(patsubst %.c,%.os,$(SOSRCS))
	$(CC) -shared $(LFLAGS) $^ -o $@ $(LDLIBS)

libinstall: fsatrace.so
	cp fsatrace.so $(INSTALLDIR)

cleanlib:
	rm -f fsatrace.so $(patsubst %.c,%.d,$(SOSRCS)) $(patsubst %.c,%.os,$(SOSRCS))

-include $(patsubst %.c,%.d,$(SOSRCS))
