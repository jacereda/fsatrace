#include <assert.h>
#include <limits.h>
#include <stdint.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
//#include <emmintrin.h>

#include "fsatrace.h"
#include "shm.h"
#include "emit.h"

static struct shm shm;

int emitInit() {
	assert(!shm.buf);
	return shmInit(&shm, getenv(ENVOUT), LOGSZ, 0);
}

int emitTerm() {
	assert(shm.buf);
	return shmTerm(&shm, 0);
}

void emitOp(int c, const char *p1, const char *p2)
{
	char           *dst = shm.buf + 4 + 256;
	char *opts = shm.buf+4;
	uint32_t       *psofar = (uint32_t *)shm.buf;
	uint32_t	sofar;
	uint32_t	sz;
	uint32_t	s1;
	uint32_t	s2;
	char           *p;
	if (!shm.buf || !opts[c])
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
