#define open Oopen
#define open64 Oopen64
#define rename Orename
#define unlink Ounlink
#define fopen Ofopen

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <limits.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#include "fsatraceunix.h"

#undef open
#undef open64
#undef rename
#undef unlink
#undef fopen

static int	s_fd;
static char    *s_buf;

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
	int		g;
	char           *dst = s_buf + sizeof(size_t);
	size_t         *psofar = (size_t *) s_buf;
	size_t		sofar;
	if (!s_buf)
		return;
	sofar = __sync_fetch_and_add(psofar, sz);
	memcpy(dst + sofar, p, sz);
	g = good(p, sz);
	if (!g)
		fprintf(stderr, "BAD: %s\n", p);
	assert(g);
}

static void
__attribute((constructor(101)))
init()
{
	const char     *shname = getenv(ENVOUT);
	s_fd = shm_open(shname, O_RDWR, 0666);
	s_buf = mmap(0, LOGSZ, PROT_READ | PROT_WRITE, MAP_SHARED, s_fd, 0);
	assert(s_fd >= 0);
}

static void
__attribute((destructor(101)))
term()
{
	munmap(s_buf, LOGSZ);
	close(s_fd);
}

static inline void
iemit(int c, const char *p1, const char *p2)
{
	char		buf       [10000];
	int		sz = 0;

	sz += snprintf(buf, sizeof(buf) - 1 - sz, "%c|%s", c, p1);
	if (p2)
		sz += snprintf(buf + sz, sizeof(buf) - 1 - sz, "|%s", p2);
	sz += snprintf(buf + sz, sizeof(buf) - 1 - sz, "\n");
	assert(sz < sizeof(buf) - 1);
	buf[sz] = 0;
	swrite(buf, sz);
}

static void
emit(int c, const char *p1)
{
	char		ap        [PATH_MAX];
	iemit(c, realpath(p1, ap), 0);
}

int
rename(const char *p1, const char *p2)
{
	int		r;
	char		b1        [PATH_MAX];
	char		b2        [PATH_MAX];
	char           *rp1 = realpath(p1, b1);
	static int      (*orename) (const char *, const char *)= 0;
	if (!orename)
		orename = dlsym(RTLD_NEXT, "rename");
	assert(orename);
	r = orename(p1, p2);
	if (!r)
		iemit('m', realpath(p2, b2), rp1);
	return r;
}

int
unlink(const char *p)
{
	int		r;
	char		b         [PATH_MAX];
	char           *rp = realpath(p, b);
	static int      (*ounlink) (const char *)= 0;
	if (!ounlink)
		ounlink = dlsym(RTLD_NEXT, "unlink");
	assert(ounlink);
	r = ounlink(p);
	if (!r)
		iemit('d', rp, 0);
	return r;
}

#define HOOKn(rt, n, args, cargs, c, e)			\
  rt n args {						\
    rt r;						\
    static rt (*o##n) args = 0;				\
    if (!o##n) o##n = dlsym(RTLD_NEXT, #n);		\
    assert(o##n);					\
    r = o##n cargs;					\
      if (c)						\
	e;						\
      return r;						\
  }

#define HOOK1(rt, n, t0, c, e)				\
  HOOKn(rt, n, (t0 a0), (a0), c, e)
#define HOOK2(rt, n, t0, t1, c, e)			\
  HOOKn(rt, n, (t0 a0, t1 a1), (a0, a1), c, e)
#define HOOK3(rt, n, t0, t1, t2, c, e)			\
  HOOKn(rt, n, (t0 a0, t1 a1, t2 a2), (a0, a1, a2), c, e)
#include "hooks.h"
#undef HOOK1
#undef HOOK2
#undef HOOK3
