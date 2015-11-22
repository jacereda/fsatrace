#include <stdint.h>
#include <windows.h>
#include <limits.h>

#include <shellapi.h>
#include "dbg.h"
#include "fsatrace.h"

WINBASEAPI DWORD WINAPI GetProcessIdOfThread(HANDLE Thread);

void injectProcess(HANDLE proc) {
    HANDLE tid;
    BOOL is32;
    FARPROC addr;
    LPVOID arg;
    char dll[PATH_MAX];
    char *ext = 0;
    DWORD rc;
    extern IMAGE_DOS_HEADER __ImageBase;
    ASSERT(proc);
    memset(dll, 0, sizeof(dll));
    CHK(GetModuleFileNameA((HMODULE)&__ImageBase, dll, sizeof(dll)));
    if (!ext)
        ext = strstr(dll, ".exe");
    if (!ext)
        ext = strstr(dll, ".dll");
    if (!ext)
        ext = dll + strlen(dll);
    CHK(IsWow64Process(proc, &is32));
    CHK(0 != (arg = VirtualAllocEx(proc, 0, strlen(dll) + 1,
                                   MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE)));
    if (strcmp(ext, ".dll"))
        memcpy(ext, is32 ? "32.dll" : "64.dll", 6);
    if (is32) {
        STARTUPINFO si;
        PROCESS_INFORMATION pi;
        const char * helpername = "fsatracehelper.exe";
        char helper[PATH_MAX];
        char * p;
        memset(&si, 0, sizeof(si));
        memset(&pi, 0, sizeof(pi));
        si.cb = sizeof(si);
        memcpy(helper, dll, strlen(dll)+1);
        p = strrchr(helper, '\\');
        memcpy(p+1, helpername, strlen(helpername)+1);
        CHK(CreateProcessA(0, helper, 0, 0, 0, 0, 0, 0, &si, &pi));
        CHK(WAIT_OBJECT_0 == WaitForSingleObject(pi.hProcess, INFINITE));
        CHK(GetExitCodeProcess(pi.hProcess, &rc));
        addr = (FARPROC)(uintptr_t)rc;
    }
    else
        addr = GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
    CHK(addr);
    CHK(WriteProcessMemory(proc, arg, dll, strlen(dll) + 1, NULL));
    CHK(0 != (tid = CreateRemoteThread(proc, 0, 0,
                                       (LPTHREAD_START_ROUTINE)addr, arg, 0, 0)));
    CHK(-1 != ResumeThread(tid));
    CHK(WAIT_OBJECT_0 == WaitForSingleObject(tid, INFINITE));
    CHK(CloseHandle(tid));
}

void injectThread(HANDLE th) {
    HANDLE h;
    CHK(0 != (h = OpenProcess(PROCESS_ALL_ACCESS, TRUE,
                              GetProcessIdOfThread(th))));
    injectProcess(h);
    CHK(CloseHandle(h));
}
