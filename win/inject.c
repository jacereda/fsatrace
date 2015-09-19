#include <windows.h>
#include <stdio.h>
#include "fsatracewin.h"

void inject(HANDLE proc) {
    HANDLE tid;
    BOOL is32;
    FARPROC addr;
    LPVOID arg;
    char dll[MAX_PATH];
    char *ext = 0;
    extern IMAGE_DOS_HEADER __ImageBase;
    memset(dll, 0, sizeof(dll));
    GetModuleFileNameA((HMODULE)&__ImageBase, dll, sizeof(dll));
    if (!ext)
        ext = strstr(dll, ".exe");
    if (!ext)
        ext = strstr(dll, ".dll");
    if (!ext)
        ext = dll + strlen(dll);
    IsWow64Process(proc, &is32);
    arg = VirtualAllocEx(proc, 0, strlen(dll) + 1,
                         MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (strcmp(ext, ".dll"))
        memcpy(ext, is32 ? "32.dll" : "64.dll", 6);
    if (is32)
        addr = (FARPROC)(uintptr_t)system("fsatracehelper");
    else
        addr = GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
    WriteProcessMemory(proc, arg, dll, strlen(dll) + 1, NULL);
    tid = CreateRemoteThread(proc, 0, 0,
                             (LPTHREAD_START_ROUTINE)addr, arg, 0, 0);
    ResumeThread(tid);
    WaitForSingleObject(tid, INFINITE);
    CloseHandle(tid);
}
