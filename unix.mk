SOSRCS=unix/fsatraceso.c

lib: fsatrace.so

%.os: %.c
	$(CC) -c -fPIC $(CPPFLAGS) $(CFLAGS) $< -o $@

fsatrace.so: $(patsubst %.c,%.os,$(SOSRCS))
	$(CC) -shared $(CPPFLAGS) $(CFLAGS) -o fsatrace.so unix/fsatraceso.c $(LDFLAGS) 

cleanlib:
	rm -f fsatrace.so $(patsubst %.c,%.d,$(SOSRCS)) $(patsubst %.c,%.os,$(SOSRCS))

-include $(patsubst %.c,%.d,$(SOSRCS))
