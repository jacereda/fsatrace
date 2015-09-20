#include <windows.h>
#include <winbase.h>
#include "emit.h"
#include "fsatrace.h"
#include "dbg.h"

static HANDLE s_mf;
static char *s_buf;

static void semit(const char *ss) {
    const char *s = ss ? ss : "<null>";
    size_t l = strlen(s);
    char * dst = s_buf + 4 + InterlockedExchangeAdd((volatile LONG*)s_buf, (LONG)l);
    memcpy(dst, s, l);
}

void emitOp(const char *e, const char *p0) {
    if (p0) {
        semit(e);
        semit("|");
        semit(p0);
        semit("\n");
    }
}

void emitOp2(const char *e, const char *p0, const char *p1) {
    semit(e);
    semit("|");
    semit(p0);
    semit("|");
    semit(p1);
    semit("\n");
}

void emitInit() {
    char out[MAX_PATH];
    GetEnvironmentVariableA(ENVOUT, out, sizeof(out));
    s_mf = OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, out);
    ASSERT(s_mf);
    s_buf = MapViewOfFile(s_mf, FILE_MAP_ALL_ACCESS, 0, 0, LOGSZ);
    ASSERT(s_buf);
}

void emitTerm() {
    UnmapViewOfFile(s_buf);
    CloseHandle(s_mf);
}
