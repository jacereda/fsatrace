#include <windows.h>
#include <stdio.h>

void
pr(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vprintf(fmt, ap);
	va_end(ap);
}

void
fatal(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	fprintf(stderr, "Fatal: ");
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	va_end(ap);
	fflush(stderr);
	ExitProcess(777);
}
/*
void _assert(const char * e, const char * f, unsigned l) {
    pr("%s:d: Assertion failed '%s'");
    ExitProcess(777);
}
*/
