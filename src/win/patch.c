#include <stdint.h>
#include <string.h>
#include <windows.h>
#include <psapi.h>
#include "dbg.h"
#include "patch.h"

static CRITICAL_SECTION s_cs;

static IMAGE_IMPORT_DESCRIPTOR *imports(IMAGE_DOS_HEADER *dh) {
	char *base = (char *)dh;
	IMAGE_NT_HEADERS *nth = (IMAGE_NT_HEADERS *)(base + dh->e_lfanew);
	IMAGE_DATA_DIRECTORY *impdd =
		nth->OptionalHeader.DataDirectory + IMAGE_DIRECTORY_ENTRY_IMPORT;
	ASSERT(dh->e_lfanew);
	ASSERT(nth);
	ASSERT(impdd);
	return impdd && impdd->VirtualAddress
		? (IMAGE_IMPORT_DESCRIPTOR *)(base + impdd->VirtualAddress)
		: 0;
}

static IMAGE_THUNK_DATA *lookup2(char *base,
                                 IMAGE_THUNK_DATA *td,
                                 IMAGE_THUNK_DATA *otd,
                                 const char *nm) {
	while (otd->u1.AddressOfData) {
		IMAGE_IMPORT_BY_NAME *name =
			(IMAGE_IMPORT_BY_NAME *)(otd->u1.AddressOfData + base);
		if (otd->u1.Ordinal & IMAGE_ORDINAL_FLAG)
			dbg("   ordinal1\n");
		else {
			dbg("  name: %s %p\n", name->Name, td->u1.Function);
			if (0 == strcmp((char*)name->Name, nm))
				return td;
		}
		otd++;
		td++;
	}
	return 0;
}

static IMAGE_THUNK_DATA *lookup(IMAGE_DOS_HEADER *dh, const char *nm) {
	char *base = (char *)dh;
	IMAGE_IMPORT_DESCRIPTOR *id = imports(dh);
	if (!id) return 0;
	while (id->Name) {
		if (id->FirstThunk && id->OriginalFirstThunk) {
			IMAGE_THUNK_DATA *d =
				lookup2(base,
					(IMAGE_THUNK_DATA*)(id->FirstThunk + base),
					(IMAGE_THUNK_DATA*)(id->OriginalFirstThunk + base),
					nm);
			dbg(" import %s\n", id->Name + base);
			if (d)
				return d;
		}
		id++;
	}
	return 0;
}

static void modpatch(IMAGE_DOS_HEADER *dh,
                     void *orig,
                     void *hook,
                     void **preal,
                     const char *nm) {
	IMAGE_THUNK_DATA *td = lookup(dh, nm);
	if (td && orig == (void *)td->u1.Function) {
		DWORD o;
		dbg("   patching %s %p %p -> %p\n", nm, td->u1.Function, orig, hook);
		*preal = (void *)td->u1.Function;
		CHK(VirtualProtect(td, sizeof(*td), PAGE_EXECUTE_READWRITE, &o));
		td->u1.Function = (uintptr_t)hook;
		CHK(VirtualProtect(td, sizeof(*td), o, &o));
	}
}

void patchInstall(void *orig, void *hook, void **preal, const char *nm) {
	HMODULE mod[4096];
	DWORD n;
	DWORD i;
	extern IMAGE_DOS_HEADER __ImageBase;
	CHK(EnumProcessModules(GetCurrentProcess(), mod, sizeof(mod), &n));
	n /= sizeof(HMODULE);
	dbg("orig %s %p\n", nm, orig);
	for (i = 0; i < n; i++) {
		HMODULE m = mod[i];
		char mname[4096];
		CHK(GetModuleFileNameA(m, mname, sizeof(mname)));
		dbg("module %s\n", mname);
		if (m != (HMODULE)&__ImageBase)
			modpatch((IMAGE_DOS_HEADER *)m, orig, hook, preal, nm);
	}
	dbg("modules patched\n");
}

int patchInstalled(DWORD pid) {
	static uint8_t pids[0x1000] = {0};
	unsigned index = pid >> 2;
	unsigned byte = (index >> 3) & 0xfff;
	unsigned mask = index & 7;
	int ret;
	EnterCriticalSection(&s_cs);
	ret = pids[byte] & mask;
	pids[byte] |= mask;
	LeaveCriticalSection(&s_cs);	
	return ret;
}

void patchInit() {
	InitializeCriticalSection(&s_cs);
}

void patchTerm() {
	DeleteCriticalSection(&s_cs);
}
