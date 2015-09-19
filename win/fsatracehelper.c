#include <windows.h>

int main() {
  return (int)(uintptr_t)GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA");
}
