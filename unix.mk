lib: fsatrace.so

fsatrace.so: unix/fsatraceso.c
	$(CC) $(LDFLAGS) unix/fsatraceso.c -o fsatrace.so

cleanlib:
	rm -f fsatrace.so fsatraceso.d


-include $(patsubst %.c,%.d,$(SRCS))
