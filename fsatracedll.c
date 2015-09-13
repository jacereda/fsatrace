#include <windows.h>
#include <winternl.h>
#include <psapi.h>
#include <stdio.h>

static FILE *file;

static char gbuf[0x100000];
static size_t gsofar = 0;

static void pr(const char * fmt, ...) {
  va_list ap;
  DWORD err = GetLastError();
  va_start(ap,fmt);
  vfprintf(file, fmt, ap);
  fflush(file);
  va_end(ap);
  SetLastError(err);
}

//#define dbg pr
#define dbg(...)
#define D dbg(__FUNCTION__ "\n")

static inline void semit(const char * s) {
  //  pr("%s",s); return;
  char * dst = gbuf + gsofar;
  size_t l = strlen(s);
  gsofar += l;
  memcpy(dst, s, l);
}

static void emit(const char * e, const char * p0) {
  semit(e);
  semit("|");
  semit(p0);
  semit("\n");
}

static void emit2(const char * e, const char * p0, const char * p1) {
  semit(e);
  semit("|");
  semit(p0);
  semit("|");
  semit(p1);
  semit("\n");
}

static char * str(char * buf, PWSTR s, unsigned sl)
{
  int l = WideCharToMultiByte(CP_UTF8, 0, s, sl, buf, MAX_PATH, 0, 0);
  buf[l] = 0;
	return strncmp(buf, "\\\\?\\", 4) == 0 || strncmp(buf, "\\??\\", 4) == 0? buf + 4 : buf;
}

#define ORIG(n) static NTSTATUS (WINAPI *o##n)()
ORIG(NtCreateFile);
ORIG(NtOpenFile);
ORIG(NtDeleteFile);
ORIG(NtSetInformationFile);
#undef ORIG

NTSTATUS WINAPI hNtCreateFile(PHANDLE ph
                              , ACCESS_MASK am
                              , POBJECT_ATTRIBUTES oa
                              , PIO_STATUS_BLOCK sb
                              , PLARGE_INTEGER as
                              , ULONG fa
                              , ULONG sa
                              , ULONG cd
                              , ULONG co
                              , PVOID bu
                              , ULONG le) {
  NTSTATUS r;
  char buf[MAX_PATH];
  D;
  r = oNtCreateFile(ph, am, oa, sb, as, fa, sa, cd, co, bu, le);
  if (!r) {
    const char * op = 0;
    if (am & GENERIC_WRITE)
      op = "w";
    else if (am & GENERIC_READ)
      op = "r";
    //    else
    //      op = "u";
    if (op)
      emit(op, str(buf, oa->ObjectName->Buffer, oa->ObjectName->Length));
  }
  return r;
}

NTSTATUS WINAPI hNtOpenFile(PHANDLE ph
                            , ACCESS_MASK am
                            , POBJECT_ATTRIBUTES oa
                            , PIO_STATUS_BLOCK sb
                            , ULONG sa
                            , ULONG oo)
{
  NTSTATUS r;
  char buf[MAX_PATH];
  D;
  r = oNtOpenFile(ph, am, oa, sb, sa, oo);
  if (!r) {
    const char * op = 0;
    if (oo & FILE_DELETE_ON_CLOSE)
      op = "d";
    else if (am & GENERIC_WRITE)
      op = "w";
    else if (am & GENERIC_READ)
      op = "r";
    //    else
    //      op = "u";
    if (op)
      emit(op, str(buf, oa->ObjectName->Buffer, oa->ObjectName->Length));
  }
  return r;
}

NTSTATUS WINAPI hNtDeleteFile(POBJECT_ATTRIBUTES oa) {
  NTSTATUS r;
  char buf[MAX_PATH];
  D;
  r = oNtDeleteFile(oa);
  if (!r)
    emit("d", str(buf, oa->ObjectName->Buffer, oa->ObjectName->Length));
  return r;
}


NTSTATUS WINAPI hNtSetInformationFile(HANDLE h
                                      , PIO_STATUS_BLOCK b
                                      , PVOID p
                                      , ULONG l
                                      , FILE_INFORMATION_CLASS c) {
  NTSTATUS r;
  char buf[MAX_PATH];
  char buf2[MAX_PATH];
  PFILE_RENAME_INFO ri = (PFILE_RENAME_INFO)p;
  //  PFILE_DISPOSITION_INFO di = (PFILE_DISPOSITION_INFO)p;
  wchar_t oname[MAX_PATH];
  DWORD olen = GetFinalPathNameByHandleW(h, oname, sizeof(oname)/sizeof(oname[0]), FILE_NAME_OPENED);
  D;
  //  snprintf(buf, sizeof(buf), "%d", c); semit(buf);
  r = oNtSetInformationFile(h, b, p, l, c);
  if (!r)
    switch (c) {
    case 4: // FileBasicInformation
      emit("w", str(buf, oname, olen));
      break;
    case 10: // FileRenameInformation
      emit2("m", str(buf, oname, olen), str(buf2, ri->FileName, ri->FileNameLength/sizeof(ri->FileName[0])));
      break;
    case 13: // FileDispositionInformation
      emit("d", str(buf, oname, olen));
      break;
    case 19: // FileAllocationInformation
      emit("w", str(buf, oname, olen));
      break;
    }
  return r;
}


static IMAGE_IMPORT_DESCRIPTOR * imports(IMAGE_DOS_HEADER * dh) {
  char * base = (char*)dh;
	IMAGE_NT_HEADERS * nth = dh->e_lfanew? (IMAGE_NT_HEADERS*)(base + dh->e_lfanew) : 0;
	IMAGE_DATA_DIRECTORY * impdd = nth? nth->OptionalHeader.DataDirectory + IMAGE_DIRECTORY_ENTRY_IMPORT : 0;
  return impdd && impdd->VirtualAddress? (IMAGE_IMPORT_DESCRIPTOR*) (base + impdd->VirtualAddress) : 0;
}

static IMAGE_THUNK_DATA * lookup2(char * base, IMAGE_THUNK_DATA * td, IMAGE_THUNK_DATA * otd, const char * fname) {
  while (otd->u1.AddressOfData) {
    IMAGE_IMPORT_BY_NAME* name = (IMAGE_IMPORT_BY_NAME*) (otd->u1.AddressOfData + base);
    if (otd->u1.Ordinal & IMAGE_ORDINAL_FLAG)
      dbg("   ordinal1\n");
    else {
      dbg("  name: %s %p\n", name->Name, td->u1.Function);
      if (0 == strcmp(name->Name, fname))
        return td;
    }
    otd++;
    td++;
  }
  return 0;
}


static IMAGE_THUNK_DATA * lookup(IMAGE_DOS_HEADER * dh, const char * fname) {
  IMAGE_IMPORT_DESCRIPTOR * id = imports(dh);
  char * base = (char*)dh;
  if (id)
    while (id->Name) {
      if (id->FirstThunk && id->OriginalFirstThunk) {
        dbg(" import %s\n", id->Name + base);
        IMAGE_THUNK_DATA * d = lookup2(base,
                                       (IMAGE_THUNK_DATA*) (id->FirstThunk + base),
                                       (IMAGE_THUNK_DATA*) (id->OriginalFirstThunk + base),
                                       fname);
        if (d) 
          return d;
      }
      id++;
    }
  return 0;
}

static void modpatch(IMAGE_DOS_HEADER * dh, void * orig, void * hook, void ** preal, const char * fname) {
  IMAGE_THUNK_DATA * td = lookup(dh, fname);
  if (td) {
    if (orig == (void*)td->u1.Function) {
      dbg("   patching %s %p %p -> %p\n", fname, td->u1.Function, orig, hook);
      DWORD o;
      *preal = (void*)td->u1.Function;
      VirtualProtect(td, sizeof(*td), PAGE_EXECUTE_READWRITE, &o);
      td->u1.Function = (uintptr_t)hook;
      VirtualProtect(td, sizeof(*td), o, &o);
    }
    }
  }

static void patch(void * orig, void * hook, void ** preal, const char * fname) {
  HMODULE mod[4096];
  DWORD n;
  extern IMAGE_DOS_HEADER __ImageBase;
  EnumProcessModules(GetCurrentProcess(), mod, sizeof(mod), &n);
  n /= sizeof(HMODULE);
  dbg("orig %p\n", orig);
  for (DWORD i = 0; i < n; i++) {
    HMODULE m = mod[i];
    char mname[4096];
    GetModuleFileNameA(m, mname, sizeof(mname));
    dbg("module %s\n", mname);
    if (m != (HMODULE)&__ImageBase)
      modpatch((IMAGE_DOS_HEADER*)m, orig, hook, preal, fname);
  }
  dbg("modules patched\n");
}
 
INT APIENTRY DllMain(HMODULE hDLL, DWORD Reason, LPVOID Reserved) {
  (void)hDLL;
  (void)Reserved;
  switch(Reason) {
  case DLL_PROCESS_ATTACH:
    fopen_s(&file, "c:\\users\\ric\\fsatrace\\temp.txt", "a+");
    dbg("pattach\n");
    HANDLE ntdll = LoadLibrary("ntdll.dll");
#define PATCH(n) patch((void*)GetProcAddress(ntdll, #n), (void*)h##n, (void**)&o##n, #n)
    PATCH(NtCreateFile);
    PATCH(NtOpenFile);
    PATCH(NtDeleteFile);
    PATCH(NtSetInformationFile);
#undef PATCH
    break;
  case DLL_PROCESS_DETACH:
    dbg("pdetach\n");
    pr("%s", gbuf);
    fclose(file);
    break;
  case DLL_THREAD_ATTACH:
    dbg("tattach\n");
    break;
  case DLL_THREAD_DETACH:
    dbg("tdetach\n");
    break;
  }
  return TRUE;
}

