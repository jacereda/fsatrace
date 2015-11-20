#include <assert.h>
#include <limits.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
//#include <emmintrin.h>

#include "fsatrace.h"
#include "shm.h"
#include "emit.h"

static struct shm shm;

int emitInit() {
	return shmInit(&shm, getenv(ENVOUT), LOGSZ, 0);
}

int emitTerm() {
	return shmTerm(&shm, 0);
}

void emitOp(int c, const char *p1, const char *p2)
{
	char           *dst = shm.buf + sizeof(unsigned);
	unsigned       *psofar = (unsigned *)shm.buf;
	unsigned	sofar;
	unsigned	sz;
	unsigned	s1;
	unsigned	s2;
	char           *p;
	if (!shm.buf)
		return;
	s1 = strlen(p1);
	sz = s1 + 3;
	if (p2) {
		s2 = strlen(p2);
		sz += s2 + 1;
	}
	sofar = __sync_fetch_and_add(psofar, sz);
	p = dst + sofar;
	*p++ = c;
	*p++ = '|';
	memcpy(p, p1, s1);
	p += s1;
	if (p2) {
		*p++ = '|';
		memcpy(p, p2, s2);
		p += s2;
	}
	*p++ = '\n';
}
