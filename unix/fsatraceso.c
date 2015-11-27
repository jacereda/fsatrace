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
#include "../fsatrace.h"

#undef open
#undef open64
#undef openat
#undef openat64
#undef rename
#undef unlink
#undef fopen
#undef fopen64

static int	s_fd;
static char    *s_buf;
static const int wmode = O_RDWR | O_WRONLY | O_APPEND | O_CREAT | O_TRUNC;

#define D fprintf(stderr, "%s\n", __FUNCTION__)

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

static void
iemit(int c, const char *p1, const char *p2)
{
	char           *dst = s_buf + sizeof(unsigned);
	unsigned       *psofar = (unsigned *)s_buf;
	unsigned	sofar;
	unsigned	sz;
	unsigned	s1;
	unsigned	s2;
	char           *p;
	if (!s_buf)
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

static void
emit(int c, const char *p1)
{
	char		ap        [PATH_MAX];
	char           *rp = realpath(p1, ap);
	iemit(c, rp ? rp : p1, 0);
}

static void
fdemit(int c, int fd)
{
	char		ap        [PATH_MAX];
#ifdef F_GETPATH
	D;
	if (-1 != fcntl(fd, F_GETPATH, ap))
#else
	ssize_t ret;
	char fdpath[100];
	snprintf(fdpath, sizeof(fdpath), "/proc/self/fd/%d", fd);
	ret = readlink(fdpath, ap, sizeof(ap));
	if (ret != -1)
#endif
	  iemit(c, ap, 0);

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
			iemit(f & wmode ? 'W' : 'R', p, 0);
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
			iemit(f & wmode ? 'W' : 'R', p, 0);
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
		iemit('m', realpath(p2, b2), rp1);
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
	R(unlink);
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
		R(unlinkat);
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

int
futimes(int fd, const struct timeval t[2])
{
	int r;
	static int (*ofutimes)(int, const struct timeval[2]);
	R(futimes);
	r = ofutimes(fd, t);
	if (!r)
		fdemit('t', fd);
	return r;
}

int
utimes(const char * p, const struct timeval t[2])
{
	int r;
	static int (*outimes)(const char *, const struct timeval[2]);
	R(utimes);
	r = outimes(p, t);
	if (!r)
		emit('t', p);
	return r;
}


#ifdef __APPLE__
#define SUF "$INODE64"

int
fstat(int fd, struct stat * buf)
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
	return r;
}

int
stat(const char *restrict path, struct stat *restrict buf)
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
	return r;
}

int
lstat(const char *restrict path, struct stat * buf)
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
	return r;

}
#endif

#ifdef __linux__
int
__fxstat(int v, int fd, struct stat *restrict buf)
{
	int		r;
	static int      (*o__fxstat) (int, int, struct stat *restrict)= 0;
	R(__fxstat);
	r = o__fxstat(v, fd, buf);
	if (!r)
		fdemit('q', fd);
	return r;
}

int
__xstat(int v, const char *restrict path, struct stat *restrict buf)
{
	int		r;
	static int      (*o__xstat) (int, const char *restrict, struct stat *restrict)= 0;
	R(__xstat);
	r = o__xstat(v, path, buf);
	if (!r)
		emit('q', path);
	return r;
}

int
__xlstat(int v, const char *restrict path, struct stat *restrict buf)
{
	int		r;
	static int      (*o__xlstat) (int, const char *restrict, struct stat *restrict)= 0;
	R(__xlstat);
	r = o__xlstat(v, path, buf);
	if (!r)
		emit('q', path);
	return r;
}

int
__fxstatat(int v, int fd, const char *path, struct stat *buf, int flag)
{
	int		r;
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
	return r;
}

#endif
