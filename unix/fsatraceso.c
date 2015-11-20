#define open Oopen
#define open64 Oopen64
#define openat Oopenat
#define openat64 Oopenat64
#define rename Orename
#define unlink Ounlink
#define fopen Ofopen
#define fopen64 Ofopen64

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

#include "../emit.h"
#include "../fsatrace.h"

#undef open
#undef open64
#undef openat
#undef openat64
#undef rename
#undef unlink
#undef fopen
#undef fopen64

static const int wmode = O_RDWR | O_WRONLY | O_APPEND | O_CREAT | O_TRUNC;

static void
__attribute((constructor(101)))
init()
{
	int err = emitInit();
	if (err)
		fprintf(stderr, "init err: %x\n", err);
}

static void
__attribute((destructor(101)))
term()
{
	int err = emitTerm();
	if (err)
		fprintf(stderr, "term err: %x\n", err);
}

static void
emit(int c, const char *p1)
{
	char		ap        [PATH_MAX];
	emitOp(c, realpath(p1, ap), 0);
}

static void
resolv(void **p, const char *n)
{
	if (!*p)
		*p = dlsym(RTLD_NEXT, n);
	assert(*p);
}

#define R(f) resolv((void**)&o##f, #f)

FILE           *
fopen(const char *p, const char *m)
{
	FILE           *r;
	static FILE    *(*ofopen) (const char *, const char *)= 0;
	R(fopen);
	r = ofopen(p, m);
	if (r)
		emit(strchr(m, 'r') ? 'r' : 'w', p);
	return r;
}

FILE           *
fopen64(const char *p, const char *m)
{
	FILE           *r;
	static FILE    *(*ofopen64) (const char *, const char *)= 0;
	R(fopen64);
	r = ofopen64(p, m);
	if (r)
		emit(strchr(m, 'r') ? 'r' : 'w', p);
	return r;
}

int
open(const char *p, int f, mode_t m)
{
	int		r;
	static int      (*oopen) (const char *, int, mode_t)= 0;
	R(open);
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
	R(open64);
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
		R(openat);
		r = oopenat(fd, p, f, m);
		if (r >= 0)
			emitOp(f & wmode ? 'W' : 'R', p, 0);
	} else
		r = open(p, f, m);
	return r;
}

int
openat64(int fd, const char *p, int f, mode_t m)
{
	int		r;
	if (fd != AT_FDCWD) {
		static int      (*oopenat64) (int, const char *, int, mode_t)= 0;
		R(openat64);
		r = oopenat64(fd, p, f, m);
		if (r >= 0)
			emitOp(f & wmode ? 'W' : 'R', p, 0);
	} else
		r = open64(p, f, m);
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
	R(rename);
	r = orename(p1, p2);
	if (!r)
		emitOp('m', realpath(p2, b2), rp1);
	return r;
}

int
renameat(int fd1, const char *p1, int fd2, const char *p2)
{
	int		r;
	if (fd1 != AT_FDCWD || fd2 != AT_FDCWD) {
		static int      (*orenameat) (const char *, const char *)= 0;
		R(renameat);
		r = orenameat(p1, p2);
		if (!r)
			emitOp('R', p2, p1);
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
	R(unlink);
	r = ounlink(p);
	if (!r)
		emitOp('d', rp, 0);
	return r;
}

int
unlinkat(int fd, const char *p, int f)
{
	int		r;
	if (fd != AT_FDCWD) {
		static int      (*ounlinkat) (int fd, const char *p, int f);
		R(unlinkat);
		r = ounlinkat(fd, p, f);
		if (!r)
			emitOp('D', p, 0);
		assert(0);
	} else if (f & AT_REMOVEDIR)
		r = rmdir(p);
	else
		r = unlink(p);
	return r;
}
