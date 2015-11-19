lib: fsatrace.so

fsatrace.so: unix/fsatraceso.o
	$(CC) -shared $(LDFLAGS) unix/fsatraceso.o -o fsatrace.so

cleanlib:
	rm -f fsatrace.so unix/fsatraceso.d unix/fsatraceso.o

-include $(patsubst %.c,%.d,$(SRCS))
