#include <stdbool.h>
#if defined _MSC_VER
#include <windows.h>
#endif
#include <winternl.h>
#include <limits.h>
#include <processthreadsapi.h>

#undef ASSERT
#include "dbg.h"
#include "../emit.h"
#include "handle.h"
#include "utf8.h"
#include "patch.h"
#include "inject.h"
#include "hooks.h"

#define HOOK(n) static NTSTATUS(NTAPI *o##n)()
HOOK(NtCreateFile);
HOOK(NtOpenFile);
HOOK(NtDeleteFile);
HOOK(NtSetInformationFile);
HOOK(NtQueryFullAttributesFile);
HOOK(NtQueryInformationFile);
HOOK(NtResumeThread);
#undef HOOK

#if defined _MSC_VER
typedef struct _FILE_STANDARD_INFORMATION {
	LARGE_INTEGER AllocationSize;
	LARGE_INTEGER EndOfFile;
	ULONG	      NumberOfLinks;
	BOOLEAN	      DeletePending;
	BOOLEAN	      Directory;
} FILE_STANDARD_INFORMATION, *PFILE_STANDARD_INFORMATION;

NTSTATUS NTAPI NtQueryInformationFile(_In_ HANDLE FileHandle,
    _Out_ PIO_STATUS_BLOCK IoStatusBlock, _Out_ PVOID FileInformation,
    _In_ ULONG Length, _In_ FILE_INFORMATION_CLASS FileInformationClass);

enum {
	FileBasicInformation = 4,
	FileRenameInformation = 10,
	FileDispositionInformation = 13,
	FileAllocationInformation = 19,
};

#endif

static const int
fop(ULONG co, ACCESS_MASK am)
{
	int op;
	if (0)
		;
	else if (co & FILE_DIRECTORY_FILE)
		op = 0;
	else if (co & FILE_DELETE_ON_CLOSE)
		op = 'd';
	else if (am & GENERIC_WRITE)
		op = 'w';
	else if (am & GENERIC_READ)
		op = 'r';
	else
		op = 0;
	return op;
}

static void
femit(HANDLE h, int op)
{
	if (op) {
		IO_STATUS_BLOCK		  sb;
		FILE_STANDARD_INFORMATION si;
		oNtQueryInformationFile(h, &sb, &si, sizeof(si),
		    5 // FileStandardInformation
		);
		if (!si.Directory) {
			char  buf[PATH_MAX];
			char *p = handlePath(buf, h);
			emitOp(op, p, 0);
		}
	}
}

static NTSTATUS NTAPI
hNtCreateFile(PHANDLE ph, ACCESS_MASK am, POBJECT_ATTRIBUTES oa,
    PIO_STATUS_BLOCK sb, PLARGE_INTEGER as, ULONG fa, ULONG sa, ULONG cd,
    ULONG co, PVOID bu, ULONG le)
{
	NTSTATUS r;
	D;
	r = oNtCreateFile(ph, am, oa, sb, as, fa, sa, cd, co, bu, le);
	if (NT_SUCCESS(r)) {
#ifdef TRACE
		char buf[PATH_MAX];
		pr("creat %x %x %x %x %x %s\n", am, co, fa, sa, cd,
		    utf8PathFromWide(buf, oa->ObjectName->Buffer,
			oa->ObjectName->Length / 2));
#endif
		femit(*ph, fop(co, am));
	}
	return r;
}

static NTSTATUS NTAPI
hNtOpenFile(PHANDLE ph, ACCESS_MASK am, POBJECT_ATTRIBUTES oa,
    PIO_STATUS_BLOCK sb, ULONG sa, ULONG oo)
{
	NTSTATUS r;
	D;
	r = oNtOpenFile(ph, am, oa, sb, sa, oo);
	if (NT_SUCCESS(r)) {
#ifdef TRACE
		char buf[PATH_MAX];
		pr("open %x %x %s\n", am, oo,
		    utf8PathFromWide(buf, oa->ObjectName->Buffer,
			oa->ObjectName->Length / 2));
#endif
		femit(*ph, fop(oo, am));
	}
	return r;
}

static NTSTATUS NTAPI
hNtDeleteFile(POBJECT_ATTRIBUTES oa)
{
	NTSTATUS r;
	char	 buf[PATH_MAX];
	D;
	r = oNtDeleteFile(oa);
	if (NT_SUCCESS(r))
		emitOp('d',
		    utf8PathFromWide(buf, oa->ObjectName->Buffer,
			oa->ObjectName->Length / 2),
		    0);
	return r;
}

static NTSTATUS NTAPI
hNtSetInformationFile(HANDLE fh, PIO_STATUS_BLOCK sb, PVOID fi, ULONG ln,
    FILE_INFORMATION_CLASS ic)
{
	NTSTATUS r;
	char	 buf[PATH_MAX];
	char	 buf2[PATH_MAX];
#ifdef _MSC_VER
	PFILE_RENAME_INFO ri = (PFILE_RENAME_INFO)fi;
#else
	PFILE_RENAME_INFORMATION ri = (PFILE_RENAME_INFORMATION)fi;
#endif
	char *opath = handlePath(buf, fh);
	D;
	r = oNtSetInformationFile(fh, sb, fi, ln, ic);
	if (NT_SUCCESS(r)) {
		switch (ic) {
		case FileBasicInformation:
			emitOp('t', opath, 0);
			break;
		case FileRenameInformation:
			emitOp(opath ? 'm' : 'M',
			    utf8PathFromWide(buf2, ri->FileName,
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

static NTSTATUS NTAPI
hNtQueryInformationFile(HANDLE fh, PIO_STATUS_BLOCK sb, PVOID fi, ULONG ln,
    FILE_INFORMATION_CLASS ic)
{
	NTSTATUS r;
	char	 buf[PATH_MAX];
	D;
	r = oNtQueryInformationFile(fh, sb, fi, ln, ic);
	if (NT_SUCCESS(r)) {
		switch (ic) {
		case FileAllInformation:
		case FileNetworkOpenInformation:
			emitOp('q', handlePath(buf, fh), 0);
			break;
		default:
			break;
		}
	}
	return r;
}

static NTSTATUS NTAPI
hNtQueryFullAttributesFile(
    POBJECT_ATTRIBUTES oa, PFILE_NETWORK_OPEN_INFORMATION oi)
{
	NTSTATUS r;
	D;
	r = oNtQueryFullAttributesFile(oa, oi);
	if (NT_SUCCESS(r)) {
		char buf[PATH_MAX];
		emitOp('q',
		    utf8PathFromWide(buf, oa->ObjectName->Buffer,
			oa->ObjectName->Length / 2),
		    0);
	}
	return r;
}

static NTSTATUS NTAPI
hNtResumeThread(HANDLE th, PULONG sc)
{
	NTSTATUS r;
	DWORD	 pid;
	D;
	pid = GetProcessIdOfThread(th);
	if (!patchInstalled(pid))
		injectPID(pid);
	r = oNtResumeThread(th, sc);
	return r;
}

void
hooksInit(void *(*resolve)(const char *))
{
	void *addr;
#define HOOK(n)             \
	addr = resolve(#n); \
	patchInstall(addr, (void *)h##n, (void **)&o##n, #n)

	HOOK(NtCreateFile);
	HOOK(NtOpenFile);
	HOOK(NtDeleteFile);
	HOOK(NtSetInformationFile);
	HOOK(NtQueryFullAttributesFile);
	HOOK(NtQueryInformationFile);
	HOOK(NtResumeThread);
#undef HOOK
}
