#include <windows.h>
#include "dbg.h"
#include "inject.h"
#include "fsatrace.h"

int main(int argc, char **argv) {
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;
    DWORD rc;
    HANDLE mf;
    char *buf;
    char *out;
    LPWSTR cmd;
    if (argc < 4 || strcmp(argv[2], "--"))
        fatal("Usage: %s <output> -- <cmdline>", argv[0]);
    out = argv[1];
    mf = CreateFileMappingA(INVALID_HANDLE_VALUE, 0, PAGE_READWRITE,
                            0, LOGSZ, out);
    SetEnvironmentVariableA(ENVOUT, out);
    buf = MapViewOfFile(mf, FILE_MAP_ALL_ACCESS, 0, 0, LOGSZ);
    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    cmd = GetCommandLineW();
    cmd = wcsstr(GetCommandLineW(), L"-- ");
    if (!CreateProcessW(0, cmd+3, 0, 0, 0, CREATE_SUSPENDED, 0, 0, &si, &pi))
        fatal("unable to create process");
    injectProcess(pi.hProcess);
    ResumeThread(pi.hThread);
    WaitForSingleObject(pi.hThread, INFINITE);
    GetExitCodeProcess(pi.hProcess, &rc);
    CloseHandle(pi.hProcess);
    if (!rc) {
        HANDLE of;
        int con = 0 == strcmp(out, "-");
        if (con)
            of = GetStdHandle(STD_OUTPUT_HANDLE);
        else 
            of = CreateFileA(out, 
                             GENERIC_WRITE, 
                             0,
                             0,
                             CREATE_ALWAYS,
                             FILE_ATTRIBUTE_NORMAL,
                             0);
        WriteFile(of, buf + 4, *(DWORD*)buf, 0, 0);
        if (!con)
            CloseHandle(of);
    }
    UnmapViewOfFile(buf);
    CloseHandle(mf);
    return rc;
}
