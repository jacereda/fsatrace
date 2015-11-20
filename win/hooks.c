#if defined _MSC_VER
#include <windows.h>
#include <winternl.h>
#else
#include <ntifs.h>
#include <ntddk.h>
#endif
#undef ASSERT
#include "dbg.h"
#include "../emit.h"
#include "handle.h"
#include "utf8.h"
#include "patch.h"
#include "inject.h"
#include "hooks.h"

#ifndef MAX_PATH
#define MAX_PATH 4096
#endif

#define HOOK(n) static NTSTATUS(NTAPI *o##n)()
HOOK(NtCreateFile);
HOOK(NtOpenFile);
HOOK(NtDeleteFile);
HOOK(NtSetInformationFile);
HOOK(NtResumeThread);
#undef HOOK

#if defined _MSC_VER
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

enum { FileBasicInformation = 4,
       FileRenameInformation = 10,
       FileDispositionInformation = 13,
       FileAllocationInformation = 19,
};

#endif


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

static void femit(HANDLE h, const char *op) {
    if (op) {
        IO_STATUS_BLOCK sb;
        FILE_STANDARD_INFORMATION si;
        NtQueryInformationFile(h, &sb, &si, sizeof(si),
                               5 // FileStandardInformation
            );
        if (!si.Directory) {
            char buf[MAX_PATH];
            emitOp(*op, handlePath(buf, h), 0);
        }
    }
}

static char * sstr(char * buf, const PWSTR s, unsigned sl) {
    char * r = utf8PathFromWide(buf, s, sl);
    return r? r : "<none>";
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
        emitOp('d', sstr(buf, oa->ObjectName->Buffer, oa->ObjectName->Length), 0);
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
#ifdef _MSC_VER
    PFILE_RENAME_INFO ri = (PFILE_RENAME_INFO)fi;
#else
    PFILE_RENAME_INFORMATION ri = (PFILE_RENAME_INFORMATION)fi;
#endif
    char * opath = handlePath(buf, fh);
    D;
    r = oNtSetInformationFile(fh, sb, fi, ln, ic);
    if (NT_SUCCESS(r)) {
        switch (ic) {
        case FileBasicInformation:
            emitOp('w', opath, 0);
            break;
        case FileRenameInformation:
            emitOp('m',
                  sstr(buf2, ri->FileName,
                      ri->FileNameLength / sizeof(ri->FileName[0])),
                  opath);
            break;
        case FileDispositionInformation:
            emitOp('d', opath, 0);
            break;
        case FileAllocationInformation:
            emitOp('w', opath, 0);
            break;
        default:
            break;
        }
    }
    return r;
}

static NTSTATUS NTAPI hNtResumeThread(HANDLE th, PULONG sc) {
    NTSTATUS r;
    D;
    if (!patchInstalled())
        injectThread(th);
    r = oNtResumeThread(th, sc);
    return r;
}


void hooksInit(void *(*resolve)(const char *)) {
    void * addr;
#define HOOK(n)                                     \
    addr = resolve(#n);    \
    patchInstall(addr, (void *)h##n, (void **) &o##n, #n)

    HOOK(NtCreateFile);
    HOOK(NtOpenFile);
    HOOK(NtDeleteFile);
    HOOK(NtSetInformationFile);
    HOOK(NtResumeThread);
#undef HOOK
}

