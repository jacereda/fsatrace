#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>

#include "fsatrace.h"
#include "shm.h"
#include "emit.h"
static struct shm shm;

static const char * mygetenv(const char * v) {
	const char * out = getenv(v);
	if (!out) {
		extern char ** environ;
		size_t l = strlen(ENVOUT);
		char ** p = environ;
		while (*p) {
			char * s = *p;
			if (0 == strncmp(s, ENVOUT, l)) {
				if (s[l] == '=') {
					out = s+l+1;
					break;
				}
			}
			p++;
		}
	}
	return out; 
		
}

int emitInit() {
 	const char * out = mygetenv(ENVOUT);
	assert(!shm.buf);
	return out? shmInit(&shm, out, LOGSZ, 0) : 1;
}

int emitTerm() {
	return shm.buf? shmTerm(&shm, 0) : 1;
}

void emitOp(int oc, const char *op1, const char *p2)
{
	char           *dst = shm.buf + 4 + 256;
	char *opts = shm.buf+4;
	uint32_t       *psofar = (uint32_t *)shm.buf;
	uint32_t	sofar;
	uint32_t	sz;
	uint32_t	s1;
	uint32_t	s2;
	char           *p;
	const char * p1;
	int c;
	if (!shm.buf || !opts[tolower(oc)])
		return;
	p1 = op1? op1 : "<unknown>";
	c = op1? oc : toupper(oc);
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
