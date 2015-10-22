#include <windows.h>
#include "emit.h"
#include "patch.h"
#include "dbg.h"
#include "hooks.h"

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
