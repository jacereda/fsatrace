#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdbool.h>
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>

#include "fsatrace.h"
#include "shm.h"
#include "emit.h"
static struct shm shm;

static const char *
mygetenv(const char *v)
{
	const char *out = getenv(v);
	if (!out) {
		extern char **environ;
		size_t	      l = strlen(v);
		char **	      p = environ;
		while (*p) {
			char *s = *p;
			if (0 == strncmp(s, v, l)) {
				if (s[l] == '=') {
					out = s + l + 1;
					break;
				}
			}
			p++;
		}
	}
#ifdef _WIN32
	// Workaround, bash distributed with ghc 8.6.5 seems to discard most
	// environment variables, pass environment variables as the first few
	// PATH components.
	if (!out) {
		const char *path = getenv("PATH");
		unsigned    i = 0;
		if (strcmp(v, ENVBUFSIZE) == 0) {
			static char buf[64];
			unsigned    j = 0;
			// Buffer size is the second positional value.
			while (path[i++] != ';')
				;
			while (path[i] != ';')
				buf[j++] = path[i++];
			buf[j] = 0;
			out = buf;
		}
		if (strcmp(v, ENVOUT) == 0) {
			static char buf[PATH_MAX];
			unsigned    j = 0;
			while (path[i] != ';')
				buf[j++] = path[i++];
			buf[j] = 0;
			out = buf;
		}
	}
#endif
	return out;
}

int
emitInit()
{
	const char *out = mygetenv(ENVOUT);
	const char *raw_buf_size = mygetenv(ENVBUFSIZE);
	size_t	    buf_size = atol(raw_buf_size);
	assert(out);
	assert(buf_size > 0);
	assert(!shm.buf);
	return out ? shmInit(&shm, out, buf_size, 0) : 1;
}

int
emitTerm()
{
	return shm.buf ? shmTerm(&shm, 0) : 1;
}

void
emitOp(int oc, const char *op1, const char *p2)
{
	char *	    dst = shm.buf + 4 + 256;
	char *	    opts = shm.buf + 4;
	uint32_t *  psofar = (uint32_t *)shm.buf;
	uint32_t    sofar;
	uint32_t    sz;
	uint32_t    s1;
	uint32_t    s2;
	char *	    p;
	const char *p1;
	int	    c;
	if (!shm.buf || !opts[tolower(oc)])
		return;
	p1 = op1 ? op1 : "<unknown>";
	c = op1 ? oc : toupper(oc);
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
