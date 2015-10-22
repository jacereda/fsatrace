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
    CHK(GetEnvironmentVariableA(ENVOUT, out, sizeof(out)));
    CHK(0 != (s_mf = OpenFileMappingA(FILE_MAP_ALL_ACCESS, FALSE, out)));
    CHK(0 != (s_buf = MapViewOfFile(s_mf, FILE_MAP_ALL_ACCESS, 0, 0, LOGSZ)));
}

void emitTerm() {
    CHK(UnmapViewOfFile(s_buf));
    CHK(CloseHandle(s_mf));
}
