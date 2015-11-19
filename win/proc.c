#include <windows.h>

#include "../proc.h"
#include "inject.h"

void
procPath(char *fullpath)
{
    extern IMAGE_DOS_HEADER __ImageBase;
	GetModuleFileNameA((HMODULE)&__ImageBase, fullpath, PATH_MAX);
}

#define CHK(code, x) do { if (err == ERR_PROC_OK && (x)) err = code; } while (0)

enum procerr
procRun(const char * unused0, char ** unused1, int * rc)
{
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
	enum procerr err = ERR_PROC_OK;
    LPWSTR cmd;
	LPWSTR cl = GetCommandLineW();
    cmd = wcsstr(cl, L"-- ");
    if (cmd)
        cmd += 3;
    else {
        cmd = wcsstr(cl, L"--\" ");
        if (cmd)
            cmd += 4;
    }
	CHK(ERR_PROC_EXEC, !cmd);
	memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    CHK(ERR_PROC_FORK, !CreateProcessW(0, cmd, 0, 0, 0, CREATE_SUSPENDED, 0, 0, &si, &pi));
	if (err == ERR_PROC_OK)
		injectProcess(pi.hProcess);
	CHK(ERR_PROC_EXEC, -1 == ResumeThread(pi.hThread));
	CHK(ERR_PROC_WAIT, WAIT_OBJECT_0 != WaitForSingleObject(pi.hThread, INFINITE));
	CHK(ERR_PROC_WAIT, !GetExitCodeProcess(pi.hProcess, rc));
	CHK(ERR_PROC_FORK, !CloseHandle(pi.hProcess));
	return err;
}
