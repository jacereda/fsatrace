#include <stdbool.h>
#include <windows.h>
#include <winternl.h>
#include <limits.h>
#include <processthreadsapi.h>
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
       FileStandardInformation = 5,
       FileRenameInformation = 10,
       FileDispositionInformation = 13,
       FileAllocationInformation = 19,
};

#endif


static volatile unsigned g_nesting = 0;

static const bool g_debug = false;

#define ORIG(x)												\
		do {												\
				g_nesting++;								\
				if (g_debug) pr("%s %d", #x, g_nesting);	\
				r = o##x;									\
				if (g_nesting > 1)							\
						goto done;							\
		} while(0)
#define DONE							 \
		done:							 \
		do {							 \
				--g_nesting;			 \
				if (g_debug) pr("done"); \
				return r;				 \
		} while (0)

static const int fop(ACCESS_MASK am, ULONG co) {
	int op;
	if (0)
		;
	else if (co & FILE_DIRECTORY_FILE)
		op = '?';
	else if (co & FILE_DELETE_ON_CLOSE)
		op = 'd';
	else if (am & GENERIC_WRITE)
		op = 'w';
	else if (am & GENERIC_READ)
		op = 'r';
	else
		op = '?';
	return op;
}

static const char * oaPath(char * buf, const POBJECT_ATTRIBUTES oa) {
	size_t l = 0;
	if (!oa) return NULL;
	if (oa->RootDirectory) {
		const char * p = handlePath(buf, oa->RootDirectory);
		if (p) {
				l = strlen(p);
				buf[l++] = '/';
				buf[l] = 0;
		}
	}
	return utf8PathFromWide(buf+l, oa->ObjectName->Buffer, oa->ObjectName->Length/2);
}

static void femit(bool ok, HANDLE h, const POBJECT_ATTRIBUTES oa, int op) {
	if (op) {
		IO_STATUS_BLOCK sb;
		FILE_STANDARD_INFORMATION si;
		oNtQueryInformationFile(h, &sb, &si, sizeof(si), FileStandardInformation);
		if (!si.Directory) {
			char buf[PATH_MAX];
			const char * p = oaPath(buf, oa);
			if (!p)
					p = handlePath(buf, h);
			if (!p)
					p = "unknown?";
			emitOp(ok, op, p, 0);
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
	if (ph)
		*ph=INVALID_HANDLE_VALUE;
	ORIG(NtCreateFile(ph, am, oa, sb, as, fa, sa, cd, co, bu, le));
	femit(NT_SUCCESS(r), ph? *ph : INVALID_HANDLE_VALUE, oa, fop(am, co));
	DONE;
}

static NTSTATUS NTAPI hNtOpenFile(PHANDLE ph,
                                  ACCESS_MASK am,
                                  POBJECT_ATTRIBUTES oa,
                                  PIO_STATUS_BLOCK sb,
                                  ULONG sa,
                                  ULONG oo) {
	NTSTATUS r;
	if (ph)
		*ph=INVALID_HANDLE_VALUE;
	ORIG(NtOpenFile(ph, am, oa, sb, sa, oo));
	femit(NT_SUCCESS(r), ph? *ph : INVALID_HANDLE_VALUE, oa, fop(am, oo));
	DONE;
}

static NTSTATUS NTAPI hNtDeleteFile(POBJECT_ATTRIBUTES oa) {
	NTSTATUS r;
	char buf[PATH_MAX];
	ORIG(NtDeleteFile(oa));
	emitOp(NT_SUCCESS(r), 'd', oaPath(buf, oa), 0);
	DONE;
}

static NTSTATUS NTAPI hNtSetInformationFile(HANDLE fh,
                                            PIO_STATUS_BLOCK sb,
                                            PVOID fi,
                                            ULONG ln,
                                            FILE_INFORMATION_CLASS ic) {
	NTSTATUS r;
	char buf[PATH_MAX];
#ifdef _MSC_VER
	PFILE_RENAME_INFO ri = (PFILE_RENAME_INFO)fi;
#else
	PFILE_RENAME_INFORMATION ri = (PFILE_RENAME_INFORMATION)fi;
#endif
	const char * opath = handlePath(buf, fh);
	bool ok;
	ORIG(NtSetInformationFile(fh, sb, fi, ln, ic));
	ok = NT_SUCCESS(r);
	switch (ic) {
	case FileBasicInformation:
		emitOp(ok, 't', opath, 0);
		break;
	case FileRenameInformation: {
		char buf2[PATH_MAX];
		emitOp(ok, opath? 'm' : 'M',
		       utf8PathFromWide(buf2, ri->FileName,
								ri->FileNameLength / sizeof(ri->FileName[0])),
		       opath);
	}   break;
	case FileDispositionInformation:
		emitOp(ok, 'd', opath, 0);
		break;
	case FileAllocationInformation:
		emitOp(ok, 'w', opath, 0);
		break;
	default:
		emitOp(ok, '?', opath, 0);
		break;
	}
	DONE;
}

static NTSTATUS NTAPI hNtQueryInformationFile(HANDLE fh,
                                              PIO_STATUS_BLOCK sb,
                                              PVOID fi,
                                              ULONG ln,
                                              FILE_INFORMATION_CLASS ic) {
	NTSTATUS r;
	char buf[PATH_MAX];
	const char * opath;
	bool ok;
	ORIG(NtQueryInformationFile(fh, sb, fi, ln, ic));
	ok = NT_SUCCESS(r);
	opath = handlePath(buf, fh);
	switch (ic) {
	case FileAllInformation:
	case FileNetworkOpenInformation:
		emitOp(ok, 'q', opath, 0);
		break;
	default:
		emitOp(ok, '?', opath, 0);
		break;
	}
	DONE;
}


static NTSTATUS NTAPI hNtQueryFullAttributesFile(POBJECT_ATTRIBUTES oa, PFILE_NETWORK_OPEN_INFORMATION oi) {
	NTSTATUS r;
	char buf[PATH_MAX];
	ORIG(NtQueryFullAttributesFile(oa, oi));
	emitOp(NT_SUCCESS(r), 'q', oaPath(buf, oa), 0);
	DONE;
}

static NTSTATUS NTAPI hNtResumeThread(HANDLE th, PULONG sc) {
	NTSTATUS r;
	DWORD pid;
	pid = GetProcessIdOfThread(th);
	if (g_debug) pr("patch %d", pid);
	if (!patchInstalled(pid))
		injectPID(pid);
	ORIG(NtResumeThread(th, sc));
	DONE;
}


void hooksInit(void *(*resolve)(void*, const char *), void* h) {
#define HOOK(n) patchInstall(resolve(h, #n), (void *)h##n, (void **) &o##n, #n)
	HOOK(NtCreateFile);
	HOOK(NtOpenFile);
	HOOK(NtDeleteFile);
	HOOK(NtSetInformationFile);
	HOOK(NtQueryFullAttributesFile);
	HOOK(NtQueryInformationFile);
	HOOK(NtResumeThread);
#undef HOOK
}
