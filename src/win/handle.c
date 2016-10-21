#include <windows.h>
#include <limits.h>

#include "utf8.h"
#include "dbg.h"
#include "handle.h"

char * handlePath(char * dst, HANDLE h) {
	WCHAR wbuf[PATH_MAX];
	DWORD len;
	CHK(h);
	len = GetFinalPathNameByHandleW(h, wbuf, PATH_MAX, FILE_NAME_NORMALIZED);
	CHK(len >= 0 && len < PATH_MAX);
	return utf8PathFromWide(dst, wbuf, len);
}
