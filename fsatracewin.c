#include <windows.h>
 
int main(int argc, char ** argv) {
  char buf[4096];
  char * ext;
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  FARPROC addr;
  LPVOID arg;
  HANDLE tid;
  BOOL is32;
  char * cmd = GetCommandLine()+strlen(argv[0]) + 2;
  (void)argc;
  memset(buf, 0, sizeof(buf));
  GetModuleFileNameA(0, buf, sizeof(buf));
  ext = strstr(buf, ".exe");
  if (!ext)
    ext = buf + strlen(buf);
  memset(&si, 0, sizeof(si));
  CreateProcess(0, cmd, 0, 0, 0, CREATE_SUSPENDED, 0, 0, &si, &pi);
  IsWow64Process(pi.hProcess, &is32);
  arg = VirtualAllocEx(pi.hProcess, 0, strlen(buf)+1, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
  if (is32) {
    addr = (FARPROC)(uintptr_t)system("fsatracehelper");
    memcpy(ext, "32.dll", 6);
  } else {
    addr = GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
    memcpy(ext, "64.dll", 6);
  }
  WriteProcessMemory(pi.hProcess, arg, buf, strlen(buf)+1, NULL);
  tid = CreateRemoteThread(pi.hProcess, 0, 0, (LPTHREAD_START_ROUTINE)addr, arg, 0, 0);
  ResumeThread(tid);
  WaitForSingleObject(tid, INFINITE);
  ResumeThread(pi.hThread);
  WaitForSingleObject(pi.hThread, INFINITE);
  CloseHandle(pi.hProcess);
  return 0;
}


