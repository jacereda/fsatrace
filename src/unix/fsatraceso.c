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

#define D // do { char b[PATH_MAX]; procPath(b); fprintf(stderr, "%s:%d %s\n", b, getpid(), __FUNCTION__); fflush(stderr); } while (0)
#define DD // do { char b[PATH_MAX]; procPath(b); fprintf(stderr, "%s:%d /%s\n", b, getpid(), __FUNCTION__); fflush(stderr); } while (0)

#define SE //int _oerrno = errno
#define RE //errno = _oerrno

static void
__attribute((constructor(101)))
init()
{
	int		err;
	D;
	err = emitInit();
	if (err)
		fprintf(stderr, "init err: %x\n", err);
	DD;
}

static void
__attribute((destructor(101)))
term()
{
	int		err;
	err = emitTerm();
	D;
	if (err)
		fprintf(stderr, "term err: %x\n", err);
	DD;
}

static void
emit(int c, const char *p1)
{
	char		ap        [PATH_MAX];
	char           *rp;
	SE;
	rp = realpath(p1, ap);
	emitOp(c, rp ? rp : p1, 0);
	RE;
}

static void
fdemit(int c, int fd)
{
	char		ap        [PATH_MAX];
	int		ok;
	SE;
	D;
#ifdef F_GETPATH
	ok = -1 != fcntl(fd, F_GETPATH, ap);
#else
	{
	char		fdpath    [100];
	D;
	snprintf(fdpath, sizeof(fdpath), "/proc/self/fd/%d", fd);
	ok = -1 != readlink(fdpath, ap, sizeof(ap));
	}
#endif
	if (ok)
		emitOp(c, ap, 0);
	DD;
	RE;
}

static void
resolv(void **p, const char *n)
{
	if (!*p) {
		SE;
		*p = dlsym(RTLD_NEXT, n);
		RE;
	}
	assert(*p);
}

#define R(f) resolv((void**)&o##f, #f)

FILE           *
fopen(const char *p, const char *m)
{
	FILE           *r;
	static FILE    *(*ofopen) (const char *, const char *)= 0;
	D;
	R(fopen);
	r = ofopen(p, m);
	if (r)
		emit(strchr(m, 'r') ? 'r' : 'w', p);
	DD;
	return r;
}

FILE           *
fopen64(const char *p, const char *m)
{
	FILE           *r;
	static FILE    *(*ofopen64) (const char *, const char *)= 0;
	D;
	R(fopen64);
	r = ofopen64(p, m);
	if (r)
		emit(strchr(m, 'r') ? 'r' : 'w', p);
	DD;
	return r;
}

int
open(const char *p, int f, mode_t m)
{
	int		r;
	static int      (*oopen) (const char *, int, mode_t)= 0;
	D;
	R(open);
	r = oopen(p, f, m);
	if (r >= 0)
		emit(f & wmode ? 'w' : 'r', p);
	DD;
	return r;
}

int
open64(const char *p, int f, mode_t m)
{
	int		r;
	static int      (*oopen64) (const char *, int, mode_t)= 0;
	D;
	R(open64);
	r = oopen64(p, f, m);
	if (r >= 0)
		emit(f & wmode ? 'w' : 'r', p);
	DD;
	return r;
}

int
openat(int fd, const char *p, int f, mode_t m)
{
	int		r;
	D;
	if (fd != AT_FDCWD) {
		static int      (*oopenat) (int, const char *, int, mode_t)= 0;
		R(openat);
		r = oopenat(fd, p, f, m);
		if (r >= 0)
			emitOp(f & wmode ? 'W' : 'R', p, 0);
	} else
		r = open(p, f, m);
	DD;
	return r;
}

int
openat64(int fd, const char *p, int f, mode_t m)
{
	int		r;
	D;
	if (fd != AT_FDCWD) {
		static int      (*oopenat64) (int, const char *, int, mode_t)= 0;
		R(openat64);
		r = oopenat64(fd, p, f, m);
		if (r >= 0)
			emitOp(f & wmode ? 'W' : 'R', p, 0);
	} else
		r = open64(p, f, m);
	DD;
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
	D;
	R(rename);
	r = orename(p1, p2);
	if (!r)
		emitOp('m', realpath(p2, b2), rp1);
	DD;
	return r;
}

int
renameat(int fd1, const char *p1, int fd2, const char *p2)
{
	int		r;
	D;
	if (fd1 != AT_FDCWD || fd2 != AT_FDCWD) {
		static int      (*orenameat) (const char *, const char *)= 0;
		R(renameat);
		r = orenameat(p1, p2);
		if (!r)
			emitOp('R', p2, p1);
	} else
		r = rename(p1, p2);
	DD;
	return r;
}

int
unlink(const char *p)
{
	int		r;
	char		b         [PATH_MAX];
	char           *rp;
	static int      (*ounlink) (const char *)= 0;
	D;
	R(unlink);
	rp = realpath(p, b);
	r = ounlink(p);
	if (!r)
		emitOp('d', rp, 0);
	DD;
	return r;
}

int
unlinkat(int fd, const char *p, int f)
{
	int		r;
	D;
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
	DD;
	return r;
}

int
futimes(int fd, const struct timeval t[2])
{
	int		r;
	static int      (*ofutimes) (int, const struct timeval[2]);
	D;
	R(futimes);
	r = ofutimes(fd, t);
	if (!r)
		fdemit('t', fd);
	DD;
	return r;
}

int
utimes(const char *p, const struct timeval t[2])
{
	int		r;
	static int      (*outimes) (const char *, const struct timeval[2]);
	D;
	R(utimes);
	r = outimes(p, t);
	if (!r)
		emit('t', p);
	DD;
	return r;
}

#ifdef __APPLE__
#define SUF "$INODE64"
int
fstat(int fd, struct stat *buf)
{
	int		r;
	static int      (*ofstat) (int, struct stat *)= 0;
	static int __thread nested = 0;
	D;
	resolv((void **)&ofstat, "fstat" SUF);
	r = ofstat(fd, buf);
	if (!nested++ && !r)
		fdemit('q', fd);
	nested--;
	DD;
	return r;
}

int
stat(const char *path, struct stat *buf)
{
	int		r;
	static int      (*ostat) (const char *restrict, struct stat *restrict)= 0;
	static int __thread nested = 0;
	D;
	resolv((void **)&ostat, "stat" SUF);
	r = ostat(path, buf);
	if (!nested++ && !r)
		emit('q', path);
	nested--;
	DD;
	return r;
}

int
lstat(const char *restrict path, struct stat *buf)
{
	int		r;
	static int      (*olstat) (const char *restrict, struct stat *restrict)= 0;
	static int __thread nested = 0;
	D;
	resolv((void **)&olstat, "lstat" SUF);
	r = olstat(path, buf);
	if (!nested++ && !r)
		emit('q', path);
	nested--;
	DD;
	return r;
}

int
fstatat(int fd, const char *path, struct stat *buf, int flag)
{
	int		r;
	D;
	if (fd != AT_FDCWD) {
		static int      (*ofstatat) (int, const char *, struct stat *, int)= 0;
		resolv((void **)&ofstatat, "fstatat" SUF);
		r = ofstatat(fd, path, buf, flag);
		if (!r)
			emit('Q', path);
	} else if (flag & AT_SYMLINK_NOFOLLOW)
		r = stat(path, buf);
	else
		r = lstat(path, buf);
	DD;
	return r;

}
#endif

#ifdef __linux__
int
__fxstat(int v, int fd, struct stat *restrict buf)
{
	int		r;
	static int      (*o__fxstat) (int, int, struct stat *restrict)= 0;
	D;
	R(__fxstat);
	r = o__fxstat(v, fd, buf);
	if (!r)
		fdemit('q', fd);
	DD;
	return r;
}

int
__xstat(int v, const char *restrict path, struct stat *restrict buf)
{
	int		r;
	static int      (*o__xstat) (int, const char *restrict, struct stat *restrict)= 0;
	D;
	R(__xstat);
	r = o__xstat(v, path, buf);
	if (!r)
		emit('q', path);
	DD;
	return r;
}

int
__xlstat(int v, const char *restrict path, struct stat *restrict buf)
{
	int		r;
	static int      (*o__xlstat) (int, const char *restrict, struct stat *restrict)= 0;
	D;
	R(__xlstat);
	r = o__xlstat(v, path, buf);
	if (!r)
		emit('q', path);
	DD;
	return r;
}

int
__fxstatat(int v, int fd, const char *path, struct stat *buf, int flag)
{
	int		r;
	D;
	if (fd != AT_FDCWD) {
		static int      (*o__fxstatat) (int, int, const char *, struct stat *restrict, int)= 0;
		R(__fxstatat);
		r = o__fxstatat(v, fd, path, buf, flag);
		if (!r)
			emit('Q', path);
	} else if (flag & AT_SYMLINK_NOFOLLOW)
		r = __xstat(v, path, buf);
	else
		r = __xlstat(v, path, buf);
	DD;
	return r;
}

#endif
