#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>

#include "fsatrace.h"
#include "shm.h"
#include "emit.h"
static struct shm shm;

static const bool emitpid = false;

static const char * mygetenv(const char * v) {
	const char * out = getenv(v);
	if (!out) {
		extern char ** environ;
		size_t l = strlen(v);
		char ** p = environ;
		while (*p) {
			char * s = *p;
			if (0 == strncmp(s, v, l)) {
				if (s[l] == '=') {
					out = s+l+1;
					break;
				}
			}
			p++;
		}
	}
	if (!out) {
			static char buf[PATH_MAX];
			const char * path = getenv("PATH");
			unsigned i = 0;
			while (path[i] != ';') {
					buf[i] = path[i];
					i++;
			}
			buf[i] = 0;
			out = buf;
	}
	return out;
}

int emitInit() {
 	const char * out = mygetenv(ENVOUT);
	assert(out);
	assert(!shm.buf);
	return out? shmInit(&shm, out, LOGSZ, 0) : 1;
}

int emitTerm() {
	return shm.buf? shmTerm(&shm, 0) : 1;
}

void emitOp(bool ok, int oc, const char *op1, const char *p2)
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
	char pid[32];
	size_t pidsz;
	if (!shm.buf || !opts[tolower(oc)] || (!ok && !opts['!']))
		return;
	if (emitpid)
		pidsz = snprintf(pid, sizeof(pid), "%d", getpid());
	p1 = op1? op1 : "<unknown>";
	c = op1? oc : toupper(oc);
	s1 = strlen(p1);
	sz = s1 + 3;
	if (p2) {
		s2 = strlen(p2);
		sz += s2 + 1;
	}
	if (!ok)
		sz++;
	if (emitpid)
		sz += pidsz + 1;
	sofar = __sync_fetch_and_add(psofar, sz);
	p = dst + sofar;
	if (!ok)
		*p++ = '!';
	*p++ = c;
	*p++ = '|';
	if (emitpid) {
		memcpy(p, pid, pidsz);
		p += pidsz;
		*p++ = ':';
	}
	memcpy(p, p1, s1);
	p += s1;
	if (p2) {
		*p++ = '|';
		memcpy(p, p2, s2);
		p += s2;
	}
	*p++ = '\n';
}
