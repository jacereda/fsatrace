#include <string.h>
#include <windows.h>
#include "utf8.h"

char * utf8PathFromWide(char *buf, const PWSTR s, unsigned sl){
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
