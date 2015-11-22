#include <sys/types.h>
#include <inttypes.h>
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
	int		i;
	va_list		ap;
	va_start(ap, fmt);
	errv(fmt, "error: ", ap);
	va_end(ap);
	for (i = 0; i < n; i++)
		fprintf(stderr, "argv[%d]=%s\n", i, l[i]);
}

static void
slurp(char *p, size_t sz, const char *path)
{
	int		fd;
	ssize_t		r = 0;
	fd = open(path, O_RDONLY);
	if (fd >= 0)
		r = read(fd, p, sz);
	if (r)
		p[r] = 0;
	if (fd >= 0)
		close(fd);
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
uniq(char *d, size_t * tot, const char *s, const char *last, size_t lastsz)
{
	const char     *end = strchr(s, '\n');
	size_t		sz = end - s;
	if (!end) {
		strcpy(d, s);
		*tot += strlen(s);
	} else if (sz != lastsz || strncmp(s, last, lastsz)) {
		memcpy(d, s, sz + 1);
		*tot += sz + 1;
		uniq(d + sz + 1, tot, end + 1, s, sz);
	} else
		uniq(d, tot, end + 1, last, lastsz);
}

static unsigned
lines(char *s, char **args)
{
	unsigned	nargs = 0;
	int		c;
	char           *start = s;
	while ((c = *s++))
		if (c == '\n') {
			args[nargs++] = start;
			s[-1] = 0;
			start = s;
		}
	return nargs;
}

int
main(int argc, char *const argv[])
{
	int		err;
	int		rc = EXIT_FAILURE;
	const char     *out;
	struct shm	shm;
	size_t		sz = 0;
	char		envout    [PATH_MAX];
	static char	buf [LOGSZ];
	char           *const *args = argv + 4;
	unsigned	nargs = argc - 4;
	const unsigned char *opts;
	char           *bopts;
	if (argc < 5 || (strcmp(argv[3], "--") && strcmp(argv[3], "---")))
		fatal(" usage: %s <options> <output> -- <cmdline>\n"
		      "  where <options> is a combination of the following characters:\n"
		      "   v: print args vector\n"
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

	opts = (const unsigned char *)argv[1];
	bopts = shm.buf + 4;
	while (*opts)
		bopts[*opts++] = 1;

	if (argv[4][0] == '@') {
		const int	MAXARGS = 0x8000;
		size_t		bsz = sizeof(buf) - MAXARGS * sizeof(char *);
		slurp(buf, bsz, argv[4] + 1);
		args = (char *const *)(buf + bsz);
		nargs = lines(buf, (char **)(buf + bsz));
	}
	if (bopts['v'])
		aerror(nargs, args, "verbose");

	switch (procRun(nargs, args, &rc)) {
	case ERR_PROC_FORK:
		aerror(nargs, args, "forking process");
		break;
	case ERR_PROC_EXEC:
		aerror(nargs, args, "executing command");
		break;
	case ERR_PROC_WAIT:
		aerror(nargs, args, "waiting for command completion:");
		break;
	default:
		if (rc)
			aerror(nargs, args, "command failed with code %d", rc);
		else if (strcmp(argv[3], "---")) {
			uniq(buf, &sz, shm.buf + 4 + 256, "", 0);
			dump(out, buf, sz);
		} else
			dump(out, shm.buf + 4, *(uint32_t *) shm.buf);

	}
	if ((err = shmTerm(&shm, 1)))
		error("freeing shared memory (%d)", err);
	return rc;
}
