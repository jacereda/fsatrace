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
#include <stdbool.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#include <stdbool.h>

#include "../emit.h"
#include "../fsatrace.h"
#include "../proc.h"

#undef open
#undef open64
#undef openat
#undef openat64
#undef rename
#undef unlink
#undef fopen
#undef fopen64

static const int wmode = O_RDWR | O_WRONLY | O_APPEND | O_CREAT | O_TRUNC;
static const bool debug = false;

#define D do { char b[PATH_MAX]; if (!debug) break; procPath(b); fprintf(stderr, "%s:%d %s\n", b, getpid(), __FUNCTION__); fflush(stderr); } while (0)
#define DD do { char b[PATH_MAX]; if (!debug) break; procPath(b); fprintf(stderr, "%s:%d /%s ->%lld\n", b, getpid(), __FUNCTION__, (long long)r); fflush(stderr); } while (0)
#define DP do { char b[PATH_MAX]; if (!debug) break; procPath(b); fprintf(stderr, "%s:%d %s %s\n", b, getpid(), __FUNCTION__, p); fflush(stderr); } while (0)

#define SE int _oerrno = errno
#define RE errno = _oerrno

static void
err(const char *msg, int err)
{
	extern const char *__progname;
	if (debug)
		fprintf(stderr, "%s %s error: %x\n", __progname, msg, err);
}

static void
emit(bool ok, int c, const char *p1)
{
	char		ap        [PATH_MAX];
	SE;
	emitOp(ok, c, realpath(p1, ap), 0);
	RE;
}

static void
fdemit(bool opok, int c, int fd)
{
	char		ap        [PATH_MAX];
	int		ok;
	SE;
#ifdef F_GETPATH
	ok = -1 != fcntl(fd, F_GETPATH, ap);
#else
	{
		ssize_t		written;
		char		fdpath    [100];
		snprintf(fdpath, sizeof(fdpath), "/proc/self/fd/%d", fd);
		written = readlink(fdpath, ap, sizeof(ap));
		ok = written >= 0 && written < sizeof(ap);
		if (ok)
			ap[written] = 0;
	}
#endif
	emitOp(opok, c, ok ? ap : 0, 0);
	RE;
}

static void
__attribute((constructor(101)))
init()
{
	int		r;
	D;
	r = emitInit();
	if (r)
		err("init", r);
	else {
		char		b         [PATH_MAX];
		procPath(b);
		emit(true, 'r', b);
	}
	DD;
}

static void
__attribute((destructor(101)))
term()
{
	int		r;
	r = emitTerm();
	D;
	if (r)
		err("term", r);
	DD;
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
	DP;
	R(fopen);
	r = ofopen(p, m);
	emit(r != NULL, strchr(m, 'r') ? 'r' : 'w', p);
	DD;
	return r;
}

FILE           *
fopen64(const char *p, const char *m)
{
	FILE           *r;
	static FILE    *(*ofopen64) (const char *, const char *)= 0;
	DP;
	R(fopen64);
	r = ofopen64(p, m);
	emit(r != NULL, strchr(m, 'r') ? 'r' : 'w', p);
	DD;
	return r;
}

int
open(const char *p, int f, mode_t m)
{
	int		r;
	static int      (*oopen) (const char *, int, mode_t)= 0;
	DP;
	R(open);
	r = oopen(p, f, m);
	emit(r >= 0, f & wmode ? 'w' : 'r', p);
	DD;
	return r;
}

int
open64(const char *p, int f, mode_t m)
{
	int		r;
	static int      (*oopen64) (const char *, int, mode_t)= 0;
	DP;
	R(open64);
	r = oopen64(p, f, m);
	emit(r >= 0, f & wmode ? 'w' : 'r', p);
	DD;
	return r;
}

int
openat(int fd, const char *p, int f, mode_t m)
{
	int		r;
	DP;
	if (fd != AT_FDCWD) {
		static int      (*oopenat) (int, const char *, int, mode_t)= 0;
		R(openat);
		r = oopenat(fd, p, f, m);
		emitOp(r >= 0, f & wmode ? 'W' : 'R', p, 0);
	} else
		r = open(p, f, m);
	DD;
	return r;
}

int
openat64(int fd, const char *p, int f, mode_t m)
{
	int		r;
	DP;
	if (fd != AT_FDCWD) {
		static int      (*oopenat64) (int, const char *, int, mode_t)= 0;
		R(openat64);
		r = oopenat64(fd, p, f, m);
		emitOp(r >= 0, f & wmode ? 'W' : 'R', p, 0);
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
	char           *rp1 = realpath(p1, b1);
	char		b2        [PATH_MAX];
	char           *rp2 = realpath(p2, b2);
	static int      (*orename) (const char *, const char *)= 0;
	D;
	R(rename);
	r = orename(p1, p2);
	emitOp(!r, rp1 ? 'm' : 'M', rp2, rp1);
	DD;
	return r;
}

int
renamex_np(const char *p1, const char *p2, unsigned fl)
{
	int		r;
	char		b1        [PATH_MAX];
	char           *rp1 = realpath(p1, b1);
	char		b2        [PATH_MAX];
	char           *rp2 = realpath(p2, b2);
	static int      (*orenamex_np) (const char *, const char *, unsigned)= 0;
	D;
	R(renamex_np);
	r = orenamex_np(p1, p2, fl);
	emitOp(!r, rp1 ? 'm' : 'M', rp2, rp1);
	DD;
	return r;
}

int
renameat(int fd1, const char *p1, int fd2, const char *p2)
{
	int		r;
	D;
	if (fd1 != AT_FDCWD || fd2 != AT_FDCWD) {
		static int      (*orenameat) (int, const char *, int, const char *)= 0;
		R(renameat);
		r = orenameat(fd1, p1, fd2, p2);
		emitOp(!r, 'R', p2, p1);
	} else
		r = rename(p1, p2);
	DD;
	return r;
}

int
renameatx_np(int fd1, const char *p1, int fd2, const char *p2, unsigned fl)
{
	int		r;
	D;
	if (fd1 != AT_FDCWD || fd2 != AT_FDCWD) {
		static int      (*orenameatx_np) (int, const char *, int, const char *, unsigned)= 0;
		R(renameatx_np);
		r = orenameatx_np(fd1, p1, fd2, p2, fl);
		emitOp(!r, 'R', p2, p1);
	} else
		r = renamex_np(p1, p2, fl);
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
	DP;
	R(unlink);
	rp = realpath(p, b);
	r = ounlink(p);
	emitOp(!r, 'd', rp, 0);
	DD;
	return r;
}

int
unlinkat(int fd, const char *p, int f)
{
	int		r;
	DP;
	if (fd != AT_FDCWD) {
		static int      (*ounlinkat) (int fd, const char *p, int f);
		R(unlinkat);
		r = ounlinkat(fd, p, f);
		emitOp(!r, 'D', p, 0);
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
	fdemit(!r, 't', fd);
	DD;
	return r;
}

int
utimes(const char *p, const struct timeval t[2])
{
	int		r;
	static int      (*outimes) (const char *, const struct timeval[2]);
	DP;
	R(utimes);
	r = outimes(p, t);
	emit(!r, 't', p);
	DD;
	return r;
}

#ifdef __APPLE__
#define SUF "$INODE64"

static volatile int __thread nested = 0;

int
fstat(int fd, struct stat *buf)
{
	int		r;
	static int      (*ofstat) (int, struct stat *)= 0;
	D;
	resolv((void **)&ofstat, "fstat" SUF);
	nested++;
	r = ofstat(fd, buf);
	if (nested == 1)
		fdemit(!r, 'q', fd);
	nested--;
	DD;
	return r;
}

int
stat(const char *p, struct stat *buf)
{
	int		r;
	static int      (*ostat) (const char *restrict, struct stat *restrict)= 0;
	DP;
	resolv((void **)&ostat, "stat" SUF);
	nested++;
	r = ostat(p, buf);
	if (nested == 1)
		emit(!r, 'q', p);
	nested--;
	DD;
	return r;
}

int
access(const char *p, int m)
{
	int		r;
	static int      (*oaccess) (const char *, int)= 0;
	DP;
	R(access);
	nested++;
	r = oaccess(p, m);
	if (nested == 1)
		emit(!r'q', p);
	nested--;
	DD;
	return r;
}

int
lstat(const char *restrict p, struct stat *buf)
{
	int		r;
	static int      (*olstat) (const char *restrict, struct stat *restrict)= 0;
	DP;
	resolv((void **)&olstat, "lstat" SUF);
	nested++;
	r = olstat(p, buf);
	if (nested == 1)
		emit(!r, 'q', p);
	nested--;
	DD;
	return r;
}

int
fstatat(int fd, const char *p, struct stat *buf, int flag)
{
	int		r;
	DP;
	if (fd != AT_FDCWD) {
		static int      (*ofstatat) (int, const char *, struct stat *, int)= 0;
		resolv((void **)&ofstatat, "fstatat" SUF);
		r = ofstatat(fd, p, buf, flag);
		emit(!r, 'Q', p);
	} else if (flag & AT_SYMLINK_NOFOLLOW)
		r = stat(p, buf);
	else
		r = lstat(p, buf);
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
	fdemit(!r, 'q', fd);
	DD;
	return r;
}

int
__xstat(int v, const char *restrict p, struct stat *restrict buf)
{
	int		r;
	static int      (*o__xstat) (int, const char *restrict, struct stat *restrict)= 0;
	DP;
	R(__xstat);
	r = o__xstat(v, p, buf);
	emit(!r, 'q', p);
	DD;
	return r;
}

int
__xlstat(int v, const char *restrict p, struct stat *restrict buf)
{
	int		r;
	static int      (*o__xlstat) (int, const char *restrict, struct stat *restrict)= 0;
	DP;
	R(__xlstat);
	r = o__xlstat(v, p, buf);
	emit(!r, 'q', p);
	DD;
	return r;
}

int
__fxstatat(int v, int fd, const char *p, struct stat *buf, int flag)
{
	int		r;
	DP;
	if (fd != AT_FDCWD) {
		static int      (*o__fxstatat) (int, int, const char *, struct stat *restrict, int)= 0;
		R(__fxstatat);
		r = o__fxstatat(v, fd, p, buf, flag);
		emit(!r, 'Q', p);
	} else if (flag & AT_SYMLINK_NOFOLLOW)
		r = __xstat(v, p, buf);
	else
		r = __xlstat(v, p, buf);
	DD;
	return r;
}


static void
ts2tv(struct timeval *tv, const struct timespec *ts)
{
	tv->tv_sec = ts->tv_sec;
	tv->tv_usec = ts->tv_nsec / 1000;
}

int
utimensat(int fd, const char *p, const struct timespec ts[2], int flags)
{
	int		r;
	static int      (*outimensat) (int, const char *, const struct timespec[2], int);
	DP;
	if (fd != AT_FDCWD
	    || flags == AT_SYMLINK_NOFOLLOW
	    || ts[0].tv_nsec == UTIME_NOW
	    || ts[0].tv_nsec == UTIME_OMIT
	    || ts[1].tv_nsec == UTIME_NOW
	    || ts[1].tv_nsec == UTIME_OMIT
		) {
		R(utimensat);
		r = outimensat(fd, p, ts, flags);
		emit(!r, 'T', p);
	} else {
		struct timeval	tv[2];
		ts2tv(tv + 0, ts + 0);
		ts2tv(tv + 1, ts + 1);
		r = utimes(p, tv);
	}
	DD;
	return r;

}

int
futimens(int fd, const struct timespec ts[2])
{
	int		r;
	static int      (*ofutimens) (int, const struct timespec[2]);
	D;
	R(futimens);
	r = ofutimens(fd, ts);
	fdemit(!r, 't', fd);
	DD;
	return r;
}

#endif
