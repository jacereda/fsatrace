#include <windows.h>

#include "../proc.h"
#include "inject.h"
#include <stdio.h>

void
procPath(char *fullpath)
{
    extern IMAGE_DOS_HEADER __ImageBase;
	GetModuleFileNameA((HMODULE)&__ImageBase, fullpath, PATH_MAX);
}

static int
quoted(const char * a)
{
	int r = 0;
	int c;
	if (*a == '"')
		return 0;
	while (!r && (c = *a++)) {
		r += c == ' ';
		r += c == '"';
		r += c == '\t';
		r += c == '\n';
		}
	return r;
}

static size_t
aquote(char * dst, size_t sz, const char * a)
{
	char * d = dst;
	int c;
	if (!quoted(a))
		return snprintf(dst, sz, "%s ", a);
	*d++ = '"';
	while ((c = *a++)) {
		if (c == '"')
			*d++ = '\\';
		*d++ = c;
	}
    *d++ = '"';
	*d++ = ' ';
	return d - dst;
}

static void
ajoin(char * dst, size_t sz, unsigned nargs, const char * const * args)
{
	size_t sofar = 0;
	unsigned i;
	for (i = 0; i < nargs; i++)
		sofar += aquote(dst + sofar, sz - sofar, args[i]);
	dst[sofar] = 0;
}


#define CHK(code, x) do { if (err == ERR_PROC_OK && (x)) err = code; } while (0)

enum procerr
procRun(unsigned nargs, const char * const * args, int * rc)
{
    STARTUPINFOA si;
    PROCESS_INFORMATION pi;
	enum procerr err = ERR_PROC_OK;
	DWORD drc;
	char arg[0x80000];
	ajoin(arg, sizeof(arg), nargs, args);
	memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    CHK(ERR_PROC_FORK, !CreateProcessA(0, arg, 0, 0, 0, CREATE_SUSPENDED, 0, 0, &si, &pi));
	if (err == ERR_PROC_OK)
		injectProcess(pi.hProcess);
	CHK(ERR_PROC_EXEC, -1 == ResumeThread(pi.hThread));
	CHK(ERR_PROC_WAIT, WAIT_OBJECT_0 != WaitForSingleObject(pi.hThread, INFINITE));
	CHK(ERR_PROC_WAIT, !GetExitCodeProcess(pi.hProcess, &drc));
	CHK(ERR_PROC_FORK, !CloseHandle(pi.hProcess));
	if (err == ERR_PROC_OK)
		*rc = drc;
	return err;
}
