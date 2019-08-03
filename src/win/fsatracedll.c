#include <windows.h>
#include <psapi.h>
#include <stdbool.h>
#include "../emit.h"
#include "patch.h"
#include "dbg.h"
#include "hooks.h"
#include "utf8.h"

static void * resolve(const char * name) {
	void * ret;
	static HANDLE dll = 0;
	if (!dll)
		CHK((dll = GetModuleHandleA("ntdll.dll")));
	CHK((ret = GetProcAddress(dll, name)));
	return ret;
}

INT APIENTRY DllMain(HMODULE hDLL, DWORD Reason, LPVOID Reserved) {
	(void)hDLL;
	(void)Reserved;
	switch (Reason) {
	case DLL_PROCESS_ATTACH:
		emitInit();
		{
			// DLLs that were loaded before we got hooked, so mark them as a read dependency
			DWORD cb = 0;
			wchar_t winBuf[PATH_MAX];
			char utfBuf[PATH_MAX];
			HANDLE hProcess = GetCurrentProcess();
			HMODULE modules[8000]; // Pick a huge value to make sure we pick up everything
			if (EnumProcessModules(hProcess, modules, sizeof(modules), &cb)) {
				// If the buffer we passed was too small, just ignore it
				if (sizeof(modules) >= cb) {
					for (int i = 0; i < cb / sizeof(HMODULE); i++) {
						DWORD res = GetModuleFileNameExW(hProcess, modules[i], winBuf, PATH_MAX);
						if (res != 0)
							emitOp('r', utf8PathFromWide(utfBuf, winBuf, res), 0);
					}
				}
			}
		}
		patchInit();
		hooksInit(resolve);
		break;
	case DLL_PROCESS_DETACH:
		patchTerm();
		emitTerm();
		break;
	}
	return TRUE;
}
