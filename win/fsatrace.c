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
    LPWSTR cl;
    LPWSTR cmd;
    char shname[8192];
    unsigned i;
    if (argc < 4 || strcmp(argv[2], "--"))
        fatal("Usage: %s <output> -- <cmdline>", argv[0]);
    out = argv[1];
    for (i = 0; out[i]; i++)
        shname[i] = out[i] == '\\' || out[i] == ':'? '/' : out[i];
    shname[i] = 0;
    CHK(0 != (mf = CreateFileMappingA(INVALID_HANDLE_VALUE, 0, PAGE_READWRITE,
                                      0, LOGSZ, shname)));
    CHK(SetEnvironmentVariableA(ENVOUT, shname));
    CHK(0 != (buf = MapViewOfFile(mf, FILE_MAP_ALL_ACCESS, 0, 0, LOGSZ)));
    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);
    CHK(0 != (cl = GetCommandLineW()));
    cmd = wcsstr(cl, L" -- ");
    if (cmd)
        cmd += 4;
    else {
        cmd = wcsstr(cl, L" \"--\" ");
        if (cmd)
            cmd += 6;
    }
    CHK(0 != cmd);
    CHK(CreateProcessW(0, cmd, 0, 0, 0, CREATE_SUSPENDED, 0, 0, &si, &pi));
    injectProcess(pi.hProcess);
    CHK(-1 != ResumeThread(pi.hThread));
    CHK(WAIT_OBJECT_0 == WaitForSingleObject(pi.hThread, INFINITE));
    CHK(GetExitCodeProcess(pi.hProcess, &rc));
    CHK(CloseHandle(pi.hProcess));
    if (!rc) {
        HANDLE of;
        DWORD written;
        int con = 0 == strcmp(out, "-");
        if (con)
            CHK(INVALID_HANDLE_VALUE != (of = GetStdHandle(STD_OUTPUT_HANDLE)));
        else
            CHK(INVALID_HANDLE_VALUE != (of = CreateFileA(out,
                                                          GENERIC_WRITE,
                                                          0,
                                                          0,
                                                          CREATE_ALWAYS,
                                                          FILE_ATTRIBUTE_NORMAL,
                                                          0)));
        CHK(WriteFile(of, buf + 4, *(DWORD*)buf, &written, 0));
        CHK(written == *(DWORD*)buf);
        if (!con)
            CHK(CloseHandle(of));
    }
    CHK(UnmapViewOfFile(buf));
    CHK(CloseHandle(mf));
    return rc;
}
