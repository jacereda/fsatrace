#include <windows.h>
#include <psapi.h>
#include <stdbool.h>
#include "../emit.h"
#include "patch.h"
#include "dbg.h"
#include "hooks.h"
#include "utf8.h"

static void * resolve(void* dll, const char * name) {
	void * ret;
	CHK((ret = GetProcAddress(dll, name)));
	return ret;
}

INT APIENTRY DllMain(HMODULE hDLL, DWORD Reason, LPVOID Reserved) {
	(void)hDLL;
	(void)Reserved;
	switch (Reason) {
	case DLL_PROCESS_ATTACH:
		CHK(0==emitInit());
		if (1){
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
						emitOp(res != 0, 'r', utf8PathFromWide(utfBuf, winBuf, res), 0);
					}
				}
			}
		}
		patchInit();
		HANDLE dll;
		CHK((dll = GetModuleHandleA("ntdll.dll")));
		hooksInit(resolve, dll);
		break;
	case DLL_PROCESS_DETACH:
		patchTerm();
		CHK(0==emitTerm());
		break;
	}
	return TRUE;
}
