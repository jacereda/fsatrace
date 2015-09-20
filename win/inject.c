#include <stdint.h>
#include <windows.h>
#include <shellapi.h>
#include "dbg.h"
#include "fsatrace.h"

WINBASEAPI DWORD WINAPI GetProcessIdOfThread(HANDLE Thread);

void injectProcess(HANDLE proc) {
    HANDLE tid;
    BOOL is32;
    FARPROC addr;
    LPVOID arg;
    char dll[MAX_PATH];
    char *ext = 0;
    BOOL r;
    DWORD rc;
    extern IMAGE_DOS_HEADER __ImageBase;
    ASSERT(proc);
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
    if (is32) {
        STARTUPINFO si;
        PROCESS_INFORMATION pi;
        memset(&si, 0, sizeof(si));
        memset(&pi, 0, sizeof(pi));
        si.cb = sizeof(si);
        r = CreateProcessA(0, "fsatracehelper.exe", 0, 0, 0, 0, 0, 0, &si, &pi);
        ASSERT(r);
        rc = WaitForSingleObject(pi.hProcess, INFINITE);
        ASSERT(rc == 0);
        r = GetExitCodeProcess(pi.hProcess, &rc);
        ASSERT(r);
        addr = (FARPROC)(uintptr_t)rc;
    }
    else
        addr = GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
    ASSERT(addr);
    r = WriteProcessMemory(proc, arg, dll, strlen(dll) + 1, NULL);
    ASSERT(r);
    tid = CreateRemoteThread(proc, 0, 0,
                             (LPTHREAD_START_ROUTINE)addr, arg, 0, 0);
    ASSERT(tid);
    ResumeThread(tid);
    rc = WaitForSingleObject(tid, INFINITE);
    ASSERT(rc == 0);
    r = CloseHandle(tid);
    ASSERT(r);
}

void injectThread(HANDLE th) {
    HANDLE h = OpenProcess(PROCESS_ALL_ACCESS, TRUE,
                           GetProcessIdOfThread(th));
    injectProcess(h);
    CloseHandle(h);

}
