#define open Oopen
#define open64 Oopen64
#define openat Oopenat
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
#undef openat
#undef rename
#undef unlink
#undef fopen

static int	s_fd;
static char    *s_buf;
static const int wmode = O_RDWR | O_WRONLY | O_APPEND | O_CREAT | O_TRUNC;

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

FILE           *
fopen(const char *p, const char *m)
{
	FILE           *r;
	static FILE    *(*ofopen) (const char *, const char *)= 0;
	if (!ofopen)
		ofopen = dlsym(RTLD_NEXT, "fopen");
	assert(ofopen);
	r = ofopen(p, m);
	if (r)
		emit(strchr(m, 'r') ? 'w' : 'r', p);
	return r;
}

int
open(const char *p, int f, mode_t m)
{
	int		r;
	static int      (*oopen) (const char *, int, mode_t)= 0;
	if (!oopen)
		oopen = dlsym(RTLD_NEXT, "open");
	assert(oopen);
	r = oopen(p, f, m);
	if (r >= 0)
		emit(f & wmode ? 'w' : 'r', p);
	return r;
}

int
open64(const char *p, int f, mode_t m)
{
	int		r;
	static int      (*oopen64) (const char *, int, mode_t)= 0;
	if (!oopen64)
		oopen64 = dlsym(RTLD_NEXT, "open64");
	assert(oopen64);
	r = oopen64(p, f, m);
	if (r >= 0)
		emit(f & wmode ? 'w' : 'r', p);
	return r;
}

int
openat(int fd, const char *p, int f, mode_t m)
{
	int		r;
	if (fd != AT_FDCWD) {
		static int      (*oopenat) (int, const char *, int, mode_t)= 0;
		if (!oopenat)
			oopenat = dlsym(RTLD_NEXT, "openat");
		assert(oopenat);
		r = oopenat(fd, p, f, m);
		if (r >= 0)
			iemit(f & wmode ? 'W' : 'R', p, 0);
	} else
		r = open(p, f, m);
	return r;
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
renameat(int fd1, const char *p1, int fd2, const char *p2)
{
	int		r;
	if (fd1 != AT_FDCWD || fd2 != AT_FDCWD) {
		static int      (*orenameat) (const char *, const char *)= 0;
		if (!orenameat)
			orenameat = dlsym(RTLD_NEXT, "renameat");
		assert(orenameat);
		r = orenameat(p1, p2);
		if (!r)
			iemit('R', p2, p1);
	} else
		r = rename(p1, p2);
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

int
unlinkat(int fd, const char *p, int f)
{
	int		r;
	if (fd != AT_FDCWD) {
		static int      (*ounlinkat) (int fd, const char *p, int f);
		if (!ounlinkat)
			ounlinkat = dlsym(RTLD_NEXT, "unlinkat");
		assert(ounlinkat);
		r = ounlinkat(fd, p, f);
		if (!r)
			iemit('D', p, 0);
		assert(0);
	} else if (f & AT_REMOVEDIR)
		r = rmdir(p);
	else
		r = unlink(p);
	return r;
}
