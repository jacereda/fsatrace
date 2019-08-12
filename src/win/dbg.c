#include <windows.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include "../emit.h"

void pr(const char *fmt, ...) {
	size_t sofar = 0;
	char buf[8192];
	va_list ap;
	va_start(ap, fmt);
	sofar += vsnprintf(buf+sofar, sizeof(buf)-sofar, fmt, ap);
	if (1)
			emitOp(true, 'X', buf, 0);
	else {
			printf("%d:%s\n", getpid(), buf);
			fflush(stdout);
	}
	va_end(ap);
}

void fatal(const char * fmt, ...) {
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "Fatal: ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
	fflush(stderr);
	ExitProcess(777);
}

void _lassert(const char * e, const char * f, unsigned l) {
	pr("%s:d: Assertion failed '%s'", f, l, e);
    ExitProcess(777);
}
