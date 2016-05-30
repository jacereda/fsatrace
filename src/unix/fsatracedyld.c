#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <fts.h>
#include <limits.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <dlfcn.h>
#include <dirent.h>

#include "../emit.h"
#include "../fsatrace.h"

static const int wmode = O_RDWR | O_WRONLY | O_APPEND | O_CREAT | O_TRUNC;

#define D			/* do { char b[PATH_MAX]; procPath(b);      \
				 * fprintf(stderr, "%s:%d %s\n", b,         \
				 * getpid(), __FUNCTION__); fflush(stderr); \
				 * } while (0) */
#define DD			/* do { char b[PATH_MAX]; procPath(b);      \
				 * fprintf(stderr, "%s:%d %s\n", b,         \
				 * getpid(), __FUNCTION__); fflush(stderr); \
				 * } while (0) */

#define SE  // int _oerrno = errno
#define RE  // errno = _oerrno

#define INTERPOSE(_replacment, _replacee)                  \
    __attribute__((used)) static struct {                  \
        const void *replacment;                            \
        const void *replacee;                              \
    } _interpose_##_replacee                               \
        __attribute__((section("__DATA,__interpose"))) = { \
            (const void *)(unsigned long)&_replacment,     \
            (const void *)(unsigned long)&_replacee}

static void 
__attribute((constructor(101))) init()
{
	int		err;
	D;
	err = emitInit();
	if (err)
		fprintf(stderr, "init err: %x\n", err);
	DD;
}

static void 
__attribute((destructor(101))) term()
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
	SE;
	emitOp(c, realpath(p1, ap), 0);
	RE;
}

static void
fdemit(int c, int fd)
{
	char		ap        [PATH_MAX];
	int		ok;
	SE;
	D;
	ok = -1 != fcntl(fd, F_GETPATH, ap);
	emitOp(c, ok ? ap : 0, 0);
	DD;
	RE;
}

FILE           *
myfopen(const char *p, const char *m)
{
	FILE           *r;
	D;
	r = fopen(p, m);
	if (r)
		emit(strchr(m, 'r') ? 'r' : 'w', p);
	DD;
	return r;
}
INTERPOSE(myfopen, fopen);

int
myopen(const char *p, int f, mode_t m)
{
	int		r;
	D;
	r = open(p, f, m);
	if (r >= 0)
		emit(f & wmode ? 'w' : 'r', p);
	DD;
	return r;
}
INTERPOSE(myopen, open);

int
myopenat(int fd, const char *p, int f, mode_t m)
{
	int		r;
	D;
	if (fd != AT_FDCWD) {
		r = openat(fd, p, f, m);
		if (r >= 0)
			emitOp(f & wmode ? 'W' : 'R', p, 0);
	} else
		r = open(p, f, m);
	DD;
	return r;
}

INTERPOSE(myopenat, openat);

int
myrename(const char *p1, const char *p2)
{
	int		r;
	char		b1        [PATH_MAX];
	char           *rp1 = realpath(p1, b1);
	D;
	r = rename(p1, p2);
	if (!r) {
		char		b2        [PATH_MAX];
		char           *rp2 = realpath(p2, b2);
		emitOp(rp1 ? 'm' : 'M', rp2, rp1);
	}
	DD;
	return r;
}
INTERPOSE(myrename, rename);

int
myrenameat(int fd1, const char *p1, int fd2, const char *p2)
{
	int		r;
	D;
	if (fd1 != AT_FDCWD || fd2 != AT_FDCWD) {
		r = renameat(fd1, p1, fd2, p2);
		if (!r)
			emitOp('R', p2, p1);
	} else
		r = rename(p1, p2);
	DD;
	return r;
}
INTERPOSE(myrenameat, renameat);

int
myunlink(const char *p)
{
	int		r;
	char		b         [PATH_MAX];
	char           *rp;
	D;
	rp = realpath(p, b);
	r = unlink(p);
	if (!r)
		emitOp('d', rp, 0);
	DD;
	return r;
}
INTERPOSE(myunlink, unlink);

int
myunlinkat(int fd, const char *p, int f)
{
	int		r;
	D;
	if (fd != AT_FDCWD) {
		r = unlinkat(fd, p, f);
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
INTERPOSE(myunlinkat, unlinkat);

int
myfutimes(int fd, const struct timeval t[2])
{
	int		r;
	D;
	r = futimes(fd, t);
	if (!r)
		fdemit('t', fd);
	DD;
	return r;
}
INTERPOSE(myfutimes, futimes);

int
myutimes(const char *p, const struct timeval t[2])
{
	int		r;
	D;
	r = utimes(p, t);
	if (!r)
		emit('t', p);
	DD;
	return r;
}
INTERPOSE(myutimes, utimes);

int
myfstat(int fd, struct stat *buf)
{
	int		r;
	static int __thread nested = 0;
	D;
	r = fstat(fd, buf);
	if (!nested++ && !r)
		fdemit('q', fd);
	nested--;
	DD;
	return r;
}
INTERPOSE(myfstat, fstat);

int
mystat(const char *path, struct stat *buf)
{
	int		r;
	static int __thread nested = 0;
	D;
	r = stat(path, buf);
	if (!nested++ && !r)
		emit('q', path);
	nested--;
	DD;
	return r;
}
INTERPOSE(mystat, stat);

int
mylstat(const char *restrict path, struct stat *buf)
{
	int		r;
	static int __thread nested = 0;
	D;
	r = lstat(path, buf);
	if (!nested++ && !r)
		emit('q', path);
	nested--;
	DD;
	return r;
}
INTERPOSE(mylstat, lstat);

int
myfstatat(int fd, const char *path, struct stat *buf, int flag)
{
	int		r;
	D;
	if (fd != AT_FDCWD) {
		r = fstatat(fd, path, buf, flag);
		if (!r)
			emit('Q', path);
	} else if (flag & AT_SYMLINK_NOFOLLOW)
		r = stat(path, buf);
	else
		r = lstat(path, buf);
	DD;
	return r;
}
INTERPOSE(myfstatat, fstatat);

DIR            *
myopendir(const char *p)
{
	DIR            *r;
	r = opendir(p);
	if (r)
		emit('l', p);
	return r;
}
INTERPOSE(myopendir, opendir);

DIR            *
myfdopendir(int fd)
{
	DIR            *r;
	r = fdopendir(fd);
	if (r)
		fdemit('l', fd);
	return r;
}
INTERPOSE(myfdopendir, fdopendir);

struct dirent  *
myreaddir(DIR * p)
{
	struct dirent  *r;
	r = readdir(p);
	if (r)
		fdemit('l', p->__dd_fd);
	return r;
}
INTERPOSE(myreaddir, readdir);
