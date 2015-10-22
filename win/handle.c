#include <windows.h>
#include "utf8.h"
#include "dbg.h"
#include "handle.h"

char * handlePath(char * dst, HANDLE h) {
    WCHAR wbuf[MAX_PATH];
    DWORD len;
    CHK(h);
    len = GetFinalPathNameByHandleW(h, wbuf, MAX_PATH, FILE_NAME_NORMALIZED);
    return utf8PathFromWide(dst, wbuf, len);
}
