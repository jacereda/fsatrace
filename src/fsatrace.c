#include <sys/types.h>
#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <fcntl.h>
#include <limits.h>
#ifdef _MSC_VER
#include <io.h>
#define ssize_t size_t
#define basename(x) strrchr(x, '\\')+1
#else
#include <unistd.h>
#include <libgen.h>
#endif

#include "fsatrace.h"
#include "shm.h"
#include "proc.h"

static void
errv(const char *fmt, const char *pref, va_list ap)
{
	char		fullpath  [PATH_MAX];
	procPath(fullpath);
	fprintf(stderr, "%s(%d): %s", basename(fullpath), getpid(), pref);
	vfprintf(stderr, fmt, ap);
	fprintf(stderr, "\n");
	fflush(stderr);
}

static void
error(const char *fmt,...)
{
	va_list		ap;
	va_start(ap, fmt);
	errv(fmt, "error: ", ap);
	va_end(ap);
}

static void
fatal(const char *fmt,...)
{
	va_list		ap;
	va_start(ap, fmt);
	errv(fmt, "fatal error: ", ap);
	va_end(ap);
	exit(EXIT_FAILURE);
}

static void
aerror(unsigned n, char *const *l, const char *fmt,...)
{
	va_list		ap;
	va_start(ap, fmt);
	errv(fmt, "error: ", ap);
	va_end(ap);
	procDumpArgs(n, l);
}

static void
dump(const char *path, char *p, size_t sz)
{
	int		fd;
	ssize_t		r = 0;
	if (strcmp(path, "-"))
		fd = open(path, O_CREAT | O_WRONLY | O_TRUNC, 0777);
	else
		fd = 1;
	if (fd >= 0)
		r = write(fd, p, sz);
	else
		error("Unable to open output file '%s'", path);
	if (r != sz)
		error("Short write (%d/%d)", r, sz);
	if (fd != 1)
		close(fd);
}

static void
uniq(char *d, size_t * tot, const char *s)
{
	const char	*end;
	const char	*last = "";
	size_t		sz;
	size_t		lastsz = 0;

	while ((end = strchr(s, '\n'))) {
		sz = end - s;
		if (sz != lastsz || strncmp(s, last, lastsz)) {
			memcpy(d, s, sz + 1);
			*tot += sz + 1;
			d += sz + 1;
			last = s;
			lastsz = sz;
		}
		s = end + 1;
	}

	strcpy(d, s);
	*tot += strlen(s);
}

int
main(int argc, char *const argv[])
{
	int		err;
	int		rc = EXIT_FAILURE;
	const char     *out;
	struct shm	shm;
	size_t		sz = 0;
	char		envout    [65536];
	static char	buf [LOGSZ];
	char           *const *args = argv + 4;
	unsigned	nargs = argc - 4;
	const unsigned char *opts;
	char           *bopts;
	int		verr;
	if (argc < 5 || (strcmp(argv[3], "--") && strcmp(argv[3], "---")))
		fatal(" usage: %s <options> <output> -- <cmdline>\n"
		      "  where <options> is a combination of the following characters:\n"
		      "   v: print args vector\n"
		      "   e: print verbose errors\n"
		      "   r: dump read operations\n"
		      "   w: dump write operations\n"
		      "   m: dump file move operations\n"
		      "   d: dump file delete operations\n"
		      "   q: dump file stat operations\n"
		      ,argv[0]);
	out = argv[2];
	if ((err = shmInit(&shm, out, LOGSZ, 1)))
		fatal("allocating shared memory (%d)", err);
	snprintf(envout, sizeof(envout), ENVOUT "=%s", out);
	putenv(envout);
#ifdef _WIN32
	// Workaround, bash distributed with ghc 8.6.5 seems to discard most
	// environment variables, pass FSAT_OUT as the first PATH component.
	snprintf(envout, sizeof(envout), "PATH=%s;%s", out, getenv("PATH"));
#endif
	putenv(envout);
	fflush(stdout);
	opts = (const unsigned char *)argv[1];
	bopts = shm.buf + 4;
	while (*opts)
		bopts[*opts++] = 1;

	if (bopts['v'])
		procDumpArgs(nargs, args);
	verr = bopts['e'];
	switch (procRun(nargs, args, &rc)) {
	case ERR_PROC_FORK:
		if (verr)
			aerror(nargs, args, "forking process");
		break;
	case ERR_PROC_EXEC:
		if (verr)
			aerror(nargs, args, "executing command: %d", rc);
		break;
	case ERR_PROC_SIGNALED:
		if (verr)
			aerror(nargs, args, "process signaled: %d", rc);
		break;
	case ERR_PROC_STOPPED:
		if (verr)
			aerror(nargs, args, "process stopped: %d", rc);
		break;
	case ERR_PROC_UNKNOWN:
		if (verr)
			aerror(nargs, args, "unknow process error");
		break;
	case ERR_PROC_WAIT:
		if (verr)
			aerror(nargs, args, "waiting for command completion");
		break;
	default:
		if (rc) {
			if (verr)
				aerror(nargs, args, "command failed with code %d", rc);
		}
		if (strcmp(argv[3], "---")) {
			uniq(buf, &sz, shm.buf + 4 + 256);
			dump(out, buf, sz);
		} else
			dump(out, shm.buf + 4, *(uint32_t *) shm.buf);

	}
	if ((err = shmTerm(&shm, 1)))
		error("freeing shared memory (%d)", err);
	return rc;
}
