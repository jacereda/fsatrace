#define open Oopen
#define rename Orename
#define unlink Ounlink
#define fopen Ofopen

#include <sys/types.h>
#include <sys/stat.h>
#include <limits.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>

#undef open
#undef rename
#undef unlink
#undef fopen

static int	s_fd = -1;
static void	init() __attribute((constructor));
static void	term() __attribute((destructor));

#define HOOKn(rt, n, args) static rt (*o##n) args;
#define HOOK1(rt, n, t0, c, e) HOOKn (rt, n, (t0))
#define HOOK2(rt, n, t0, t1, c, e) HOOKn (rt, n, (t0, t1))
#define HOOK3(rt, n, t0, t1, t2, c, e) HOOKn (rt, n, (t0, t1, t2))
#include "hooks.h"
#undef HOOK3
#undef HOOK2
#undef HOOK1
#undef HOOKn

static int
good(const char *s, int sz)
{
	int		i;
	int		bad = 0;

	for (i = 0; i < sz; i++)
		bad += s[i] == 0;
	return !bad;
}

static void
swrite(const char *p, int sz)
{
	int		written;

	assert(s_fd >= 0);
	if (s_fd < 0)
		return;
	written = write(s_fd, p, sz);
	if (written != sz)
		fprintf(stderr, "Unable to write: '%s' %d/%d %d %d\n",
			p, sz, written, errno, s_fd);
	assert(written == sz);
	if (!good(p, sz))
		fprintf(stderr, "BAD: %s\n", p);
	assert(good(p, sz));
}

static void
init()
{
	const char     *libcname =
#if defined __APPLE__
	"libc.dylib"
#elif defined __NetBSD__
	"libc.so"
#else
	"libc.so.6"
#endif
	               ;
	void           *libc = dlopen(libcname, RTLD_LAZY | RTLD_GLOBAL);
	const char     *target = getenv("FSAT_OUT");

#define HOOKn(n) o##n = dlsym(libc, #n);
#define HOOK1(rt, n, t0, c, e) HOOKn(n)
#define HOOK2(rt, n, t0, t1, c, e) HOOKn(n)
#define HOOK3(rt, n, t0, t1, t2, c, e) HOOKn(n)
#include "hooks.h"
#undef HOOK1
#undef HOOK2
#undef HOOK3
#undef HOOKn
	assert(target);
	if (target)
		s_fd = oopen(target, O_CREAT | O_WRONLY | O_APPEND, 0777);
	if (s_fd < 0)
		fprintf(stderr, "Unable to open output file '%s'\n", target);
	assert(s_fd >= 0);
}

static void
term()
{
	close(s_fd);
}

static inline void
iemit(int c, const char *p1, const char *p2)
{
	char		buf       [10000];
	int		sz = 0;
	char		ap        [PATH_MAX];
	realpath(p1, ap);
	sz += snprintf(buf, sizeof(buf) - 1 - sz, "%c|%s", c, ap);
	if (p2) {
		realpath(p2, ap);
		sz += snprintf(buf + sz, sizeof(buf) - 1 - sz, "|%s", ap);
	}
	sz += snprintf(buf + sz, sizeof(buf) - 1 - sz, "\n");
	assert(sz < sizeof(buf) - 1);
	buf[sz] = 0;
	swrite(buf, sz);
}

static void emit(int c, const char * p1) {
  iemit(c, p1, 0);
}

static void emit2(int c, const char * p1, const char * p2)
{
  iemit(c, p1, p2);
}


#define HOOKn(rt, n, args, cargs, c, e)		\
rt n args {						\
	rt r = o##n cargs;				\
	if (c) 						\
		e;			\
	return r;					\
}
#define HOOK1(rt, n, t0, c, e)			\
	HOOKn(rt, n, (t0 a0), (a0), c, e)
#define HOOK2(rt, n, t0, t1, c, e)		\
	HOOKn(rt, n, (t0 a0, t1 a1), (a0, a1), c, e)
#define HOOK3(rt, n, t0, t1, t2, c, e)		\
	HOOKn(rt, n, (t0 a0, t1 a1, t2 a2), (a0, a1, a2), c, e)
#include "hooks.h"
#undef HOOK1
#undef HOOK2
#undef HOOK3
