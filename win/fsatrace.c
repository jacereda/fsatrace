#include <stdio.h>
#include <assert.h>
#include <windows.h>
#include "fsatrace.h"

int main(int argc, char **argv) {
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
    FILE *of;
    DWORD rc;
    HANDLE mf;
    char *buf;
    char *out;
    LPWSTR cmd;
    if (argc < 4 || strcmp(argv[2], "--")) {
        fprintf(stderr, "Usage: %s <output> -- <cmdline>\n", argv[0]);
        return EXIT_FAILURE;
    }
    out = argv[1];
    mf = CreateFileMappingA(INVALID_HANDLE_VALUE, 0, PAGE_READWRITE,
                            0, LOGSZ, out);
    SetEnvironmentVariableA(ENVOUT, out);
    buf = MapViewOfFile(mf, FILE_MAP_ALL_ACCESS, 0, 0, LOGSZ);
    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    cmd = wcsstr(GetCommandLineW(), L"-- ");
    CreateProcessW(0, cmd+3, 0, 0, 0, CREATE_SUSPENDED, 0, 0, &si, &pi);
    inject(pi.hProcess);
    ResumeThread(pi.hThread);
    WaitForSingleObject(pi.hThread, INFINITE);
    GetExitCodeProcess(pi.hProcess, &rc);
    CloseHandle(pi.hProcess);
    if (!rc) {
	if (0 == strcmp(out, "-"))
	    of = stdout;
	else
	    fopen_s(&of, out, "a+");
	fprintf(of, "%s", buf + sizeof(LONG));
	if (of != stdout)
	    fclose(of);
    }
    UnmapViewOfFile(buf);
    CloseHandle(mf);
    return rc;
}
