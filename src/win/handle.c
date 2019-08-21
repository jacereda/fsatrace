#include <windows.h>
#include <limits.h>
#include <stdbool.h>

#include "utf8.h"
#include "dbg.h"
#include "handle.h"

const char * handlePath(char * dst, HANDLE h) {
	WCHAR wbuf[PATH_MAX];
	DWORD len;
	if (h && h != INVALID_HANDLE_VALUE)
		len = GetFinalPathNameByHandleW(h, wbuf, PATH_MAX, FILE_NAME_NORMALIZED);
	else
		len = 0;
	CHK(len >= 0 && len < PATH_MAX);
	return utf8PathFromWide(dst, wbuf, len);
}
