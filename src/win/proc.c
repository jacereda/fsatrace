#include <windows.h>

#include "../proc.h"
#include "inject.h"
#include <stdio.h>

#define MAXCMD 65536

void
argvToCommandLine(LPWSTR dst, unsigned n, LPWSTR *args)
{
#define EMITN(x, n)                          \
	do {                                 \
		unsigned _i;                 \
		for (_i = 0; _i < (n); _i++) \
			dst[sofar++] = x;    \
	} while (0)
#define EMIT(x) EMITN(x, 1)
#define EMITALL                                   \
	do {                                      \
		unsigned _i = 0;                  \
		while (arg[_i])                   \
			dst[sofar++] = arg[_i++]; \
	} while (0)
	unsigned sofar = 0;
	int	 cmdexe =
	    !_wcsnicmp(args[0], L"\"cmd", 4) || !_wcsnicmp(args[0], L"cmd", 3);
	int quoted = 0;
	int nextquoted = 1;
	for (unsigned j = 0; j < n; j++) {
		LPWSTR	 arg = args[j];
		unsigned i = 0;
		if (j)
			EMIT(' ');
		if ((cmdexe || !quoted) && arg[0] == '/') {
			//			EMITALL;
			//			quoted = 0;
			//			continue;
			quoted = 0;
			nextquoted = 0;
			cmdexe = 0;
		}
		if (arg[0] == '>' || arg[0] == '|')
			quoted = 0;
		if (quoted)
			EMIT('"');
		while (arg[i]) {
			unsigned ns = 0;
			switch (arg[i]) {
			case '"':
				EMIT('\\');
				EMIT('"');
				break;
			case '\\':
				while (arg[i] == '\\') {
					ns++;
					i++;
				}
				switch (arg[i]) {
				case 0:
					EMITN('\\', 2 * ns);
					break;
				case '"':
					EMITN('\\', 2 * ns + 1);
					EMIT('"');
					break;
				default:
					EMITN('\\', ns);
					EMIT(arg[i]);
				}
				break;
			default:
				EMIT(arg[i]);
			}
			i += arg[i] != 0;
		}
		if (quoted)
			EMIT('"');
		quoted = nextquoted;
		nextquoted = 1;
	}
	EMIT(0);
}

void
procPath(char *fullpath)
{
	extern IMAGE_DOS_HEADER __ImageBase;
	GetModuleFileNameA((HMODULE)&__ImageBase, fullpath, PATH_MAX);
}

void
procDumpArgs(unsigned xnargs, char *const xaargs[])
{
	LPWSTR	cl = GetCommandLineW();
	int	i;
	int	argc;
	LPWSTR *wargv = CommandLineToArgvW(cl, &argc);
	WCHAR	args[MAXCMD];
	fprintf(stderr, "original=%ls\n", cl);
	for (i = 4; i < argc; i++)
		fprintf(stderr, "argv[%d]=%ls\n", i - 4, wargv[i]);
	argvToCommandLine(args, argc - 4, wargv + 4);
	fprintf(stderr, "composed=%ls\n", args);
	LocalFree(wargv);
}

#define CHK(code, x)                           \
	do {                                   \
		if (err == ERR_PROC_OK && (x)) \
			err = code;            \
	} while (0)

enum procerr
procRun(unsigned xnargs, char *const xargs[], int *rc)
{
	STARTUPINFOW	    si;
	PROCESS_INFORMATION pi;
	enum procerr	    err = ERR_PROC_OK;
	DWORD		    drc = 0;
	WCHAR		    args[MAXCMD];
	LPWSTR		    cl = GetCommandLineW();
	int		    argc;
	LPWSTR *	    wargv = CommandLineToArgvW(cl, &argc);
	argvToCommandLine(args, argc - 4, wargv + 4);
	LocalFree(wargv);
	memset(&si, 0, sizeof(si));
	si.cb = sizeof(si);
	CHK(ERR_PROC_FORK,
	    !CreateProcessW(
		0, args, 0, 0, 0, CREATE_SUSPENDED, 0, 0, &si, &pi));
	if (err == ERR_PROC_OK)
		injectProcess(pi.hProcess);
	CHK(ERR_PROC_EXEC, -1 == ResumeThread(pi.hThread));
	CHK(ERR_PROC_WAIT,
	    WAIT_OBJECT_0 != WaitForSingleObject(pi.hThread, INFINITE));
	CHK(ERR_PROC_WAIT,
	    WAIT_OBJECT_0 != WaitForSingleObject(pi.hProcess, INFINITE));
	CHK(ERR_PROC_WAIT, !GetExitCodeProcess(pi.hProcess, &drc));
	CHK(ERR_PROC_FORK, !CloseHandle(pi.hThread));
	CHK(ERR_PROC_FORK, !CloseHandle(pi.hProcess));
	if (err == ERR_PROC_OK)
		*rc = drc;
	return err;
}
