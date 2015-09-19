#include <windows.h>
#include <winternl.h>
#include <psapi.h>
#include <assert.h>
#include <stdio.h>
#include "fsatracewin.h"


#undef TRACE
//#define dbg pr
#define dbg(...)

#ifdef TRACE
#define D semit(__FUNCTION__ "\n")
#else
#define D
#endif

static HANDLE s_mf;
static char *s_buf;
static DWORD s_hooked;
#define HOOK(n) static NTSTATUS(NTAPI *o##n)()
HOOK(NtCreateFile);
HOOK(NtOpenFile);
HOOK(NtDeleteFile);
HOOK(NtSetInformationFile);
HOOK(NtResumeThread);
#undef HOOK


typedef struct _FILE_STANDARD_INFORMATION {
    LARGE_INTEGER AllocationSize;
    LARGE_INTEGER EndOfFile;
    ULONG         NumberOfLinks;
    BOOLEAN       DeletePending;
    BOOLEAN       Directory;
} FILE_STANDARD_INFORMATION, *PFILE_STANDARD_INFORMATION;

NTSTATUS NTAPI NtQueryInformationFile(
    _In_  HANDLE                 FileHandle,
    _Out_ PIO_STATUS_BLOCK       IoStatusBlock,
    _Out_ PVOID                  FileInformation,
    _In_  ULONG                  Length,
    _In_  FILE_INFORMATION_CLASS FileInformationClass
    );

static void semit(const char *ss) {
    const char *s = ss ? ss : "<null>";
    LONG l = (LONG)strlen(s);
    char *dst = s_buf + sizeof(LONG) + InterlockedAdd((LONG *)s_buf, l) - l;
    memcpy(dst, s, l);
}

static void pr(const char *fmt, ...) {
    char buf[8192];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf_s(buf, sizeof(buf), _TRUNCATE, fmt, ap);
    va_end(ap);
    semit(buf);
}

static void emit(const char *e, const char *p0) {
    if (p0) {
        semit(e);
        semit("|");
        semit(p0);
        semit("\n");
    }
}

static void emit2(const char *e, const char *p0, const char *p1) {
    semit(e);
    semit("|");
    semit(p0);
    semit("|");
    semit(p1);
    semit("\n");
}

static char *str(char *buf, const PWSTR s, unsigned sl) {
    int l = WideCharToMultiByte(CP_UTF8, 0, s, sl, buf, MAX_PATH, 0, 0);
    buf[l] = 0;
    if (!buf[0])
        return 0;
    if (buf[0] == '\\' && !strchr(buf, ':'))
        return 0;
    if (strncmp(buf, "\\\\?\\", 4) == 0 || strncmp(buf, "\\??\\", 4) == 0)
        return buf + 4;
    return buf;
}

static char * sstr(char * buf, const PWSTR s, unsigned sl) {
    char * r = str(buf, s, sl);
    return r? r : "<none>";
}

static const char *fop(ULONG co, ACCESS_MASK am) {
    const char *op;
    if (0)
        ;
    else if (co & FILE_DIRECTORY_FILE)
        op = 0;
    else if (co & FILE_DELETE_ON_CLOSE)
        op = "d";
    else if (am & GENERIC_WRITE)
        op = "w";
    else if (am & GENERIC_READ)
        op = "r";
    else
        op = 0;
    return op;
}

void femit(HANDLE h, const char *op) {
    if (op) {
        IO_STATUS_BLOCK sb;
        FILE_STANDARD_INFORMATION si;
        NtQueryInformationFile(h, &sb, &si, sizeof(si),
                               5 // FileStandardInformation
            );
        if (!si.Directory) {
            char buf[MAX_PATH];
            WCHAR name[MAX_PATH];
            DWORD len = GetFinalPathNameByHandleW(
                h, name, sizeof(name) / sizeof(name[0]), FILE_NAME_NORMALIZED);
            emit(op, str(buf, name, len));
        }
    }
}

static NTSTATUS NTAPI hNtCreateFile(PHANDLE ph,
                                    ACCESS_MASK am,
                                    POBJECT_ATTRIBUTES oa,
                                    PIO_STATUS_BLOCK sb,
                                    PLARGE_INTEGER as,
                                    ULONG fa,
                                    ULONG sa,
                                    ULONG cd,
                                    ULONG co,
                                    PVOID bu,
                                    ULONG le) {
    NTSTATUS r;
    D;
    r = oNtCreateFile(ph, am, oa, sb, as, fa, sa, cd, co, bu, le);
    if (NT_SUCCESS(r)) {
#ifdef TRACE
        char buf[MAX_PATH];
        pr("creat %x %x %x %x %x %s\n",
           am, co, fa, sa, cd,
           sstr(buf, oa->ObjectName->Buffer, oa->ObjectName->Length));
#endif
        femit(*ph, fop(co, am));
    }
    return r;
}

static NTSTATUS NTAPI hNtOpenFile(PHANDLE ph,
                                  ACCESS_MASK am,
                                  POBJECT_ATTRIBUTES oa,
                                  PIO_STATUS_BLOCK sb,
                                  ULONG sa,
                                  ULONG oo) {
    NTSTATUS r;
    D;
    r = oNtOpenFile(ph, am, oa, sb, sa, oo);
    if (NT_SUCCESS(r)) {
#ifdef TRACE
        char buf[MAX_PATH];
        pr("open %x %x %s\n", am, oo,
           sstr(buf, oa->ObjectName->Buffer, oa->ObjectName->Length));
#endif
        femit(*ph, fop(oo, am));
    }
    return r;
}

static NTSTATUS NTAPI hNtDeleteFile(POBJECT_ATTRIBUTES oa) {
    NTSTATUS r;
    char buf[MAX_PATH];
    D;
    r = oNtDeleteFile(oa);
    if (NT_SUCCESS(r))
        emit("d", str(buf, oa->ObjectName->Buffer, oa->ObjectName->Length));
    return r;
}

static NTSTATUS NTAPI hNtSetInformationFile(HANDLE fh,
                                            PIO_STATUS_BLOCK sb,
                                            PVOID fi,
                                            ULONG ln,
                                            FILE_INFORMATION_CLASS ic) {
    NTSTATUS r;
    char buf[MAX_PATH];
    char buf2[MAX_PATH];
    PFILE_RENAME_INFO ri = (PFILE_RENAME_INFO)fi;
    WCHAR oname[MAX_PATH];
    DWORD olen = GetFinalPathNameByHandleW(
        fh, oname, sizeof(oname) / sizeof(oname[0]), FILE_NAME_OPENED);
    D;
    r = oNtSetInformationFile(fh, sb, fi, ln, ic);
    if (NT_SUCCESS(r)) {
        switch (ic) {
        case 4: // FileBasicInformation
            emit("w", str(buf, oname, olen));
            break;
        case 10: // FileRenameInformation
            emit2("m", 
                  str(buf2, ri->FileName,
                      ri->FileNameLength / sizeof(ri->FileName[0])),
		  str(buf, oname, olen)
		  );
            break;
        case 13: // FileDispositionInformation
            emit("d", str(buf, oname, olen));
            break;
        case 19: // FileAllocationInformation
            emit("w", str(buf, oname, olen));
            break;
        }
    }
    return r;
}

static NTSTATUS NTAPI hNtResumeThread(HANDLE th, PULONG sc) {
    NTSTATUS r;
    D;
    assert(s_hooked);
    if (!TlsGetValue(s_hooked)) {
        TlsSetValue(s_hooked, (void *)1);
        HANDLE h = OpenProcess(PROCESS_ALL_ACCESS, TRUE,
                               GetProcessIdOfThread(th));
        inject(h);
        CloseHandle(h);
        TlsSetValue(s_hooked, (void *)0);
    }
    r = oNtResumeThread(th, sc);
    return r;
}

static IMAGE_IMPORT_DESCRIPTOR *imports(IMAGE_DOS_HEADER *dh) {
    char *base = (char *)dh;
    IMAGE_NT_HEADERS *nth = (IMAGE_NT_HEADERS *)(base + dh->e_lfanew);
    IMAGE_DATA_DIRECTORY *impdd =
        nth->OptionalHeader.DataDirectory + IMAGE_DIRECTORY_ENTRY_IMPORT;
    assert(dh->e_lfanew);
    assert(nth);
    assert(impdd);
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
            if (0 == strcmp(name->Name, nm))
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
        VirtualProtect(td, sizeof(*td), PAGE_EXECUTE_READWRITE, &o);
        td->u1.Function = (uintptr_t)hook;
        VirtualProtect(td, sizeof(*td), o, &o);
    }
}

static void patch(void *orig, void *hook, void **preal, const char *nm) {
    HMODULE mod[4096];
    DWORD n;
    extern IMAGE_DOS_HEADER __ImageBase;
    EnumProcessModules(GetCurrentProcess(), mod, sizeof(mod), &n);
    n /= sizeof(HMODULE);
    dbg("orig %s %p\n", nm, orig);
    for (DWORD i = 0; i < n; i++) {
        HMODULE m = mod[i];
        char mname[4096];
        GetModuleFileNameA(m, mname, sizeof(mname));
        dbg("module %s\n", mname);
        if (m != (HMODULE)&__ImageBase)
            modpatch((IMAGE_DOS_HEADER *)m, orig, hook, preal, nm);
    }
    dbg("modules patched\n");
}

static void hook() {
    FARPROC addr;
    HANDLE dll = GetModuleHandle("ntdll.dll");
    assert(dll);
#define HOOK(n)                                     \
    addr = (void *)GetProcAddress(dll, #n);         \
    assert(addr);                                   \
    patch(addr, (void *)h##n, (void **) &o##n, #n)

    HOOK(NtCreateFile);
    HOOK(NtOpenFile);
    HOOK(NtDeleteFile);
    HOOK(NtSetInformationFile);
    HOOK(NtResumeThread);
#undef HOOK
}

INT APIENTRY DllMain(HMODULE hDLL, DWORD Reason, LPVOID Reserved) {
    (void)hDLL;
    (void)Reserved;
    char out[MAX_PATH];
    switch (Reason) {
    case DLL_PROCESS_ATTACH:
        GetEnvironmentVariableA(ENVOUT, out, sizeof(out));
        s_mf = OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, out);
        assert(s_mf);
        s_buf = MapViewOfFile(s_mf, FILE_MAP_ALL_ACCESS, 0, 0, LOGSZ);
        assert(s_buf);
        s_hooked = TlsAlloc();
        assert(s_hooked);
        hook();
        dbg("pattach\n");
        break;
    case DLL_PROCESS_DETACH:
        TlsFree(s_hooked);
        UnmapViewOfFile(s_buf);
        CloseHandle(s_mf);
        break;
    }
    return TRUE;
}
