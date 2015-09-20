#include <windows.h>
#include "utf8.h"
#include "dbg.h"
#include "handle.h"

char * handlePath(char * dst, HANDLE h) {
    char * ret;
    WCHAR wbuf[MAX_PATH];
    DWORD len = GetFinalPathNameByHandleW(h, wbuf, MAX_PATH, FILE_NAME_NORMALIZED);
    ret = utf8PathFromWide(dst, wbuf, len);
    return ret;
}
