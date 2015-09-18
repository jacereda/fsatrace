#include <sys/types.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <limits.h>
#include <spawn.h>
#include "fsatraceunix.h"

extern char   **environ;

static void
dump(const char *path, char *p)
{
	int		fd;
	ssize_t		r;
	size_t		sz;
	if (strcmp(path, "-"))
		fd = open(path, O_CREAT | O_WRONLY | O_TRUNC, 0777);
	else
		fd = 1;
	if (fd < 0)
		fprintf(stderr, "Unable to open output file '%s'\n", path);
	sz = *(size_t *) p;
	r = write(fd, p + sizeof(size_t), sz);
	assert(r == sz);
	if (fd != 1)
		close(fd);
}

int
main(int argc, char **argv)
{
	void           *buf;
	int		fd;
	int		r;
	char		so        [PATH_MAX];
	int		rc = EXIT_FAILURE;
	int		child;
	const char     *out;
	if (argc < 4 || strcmp(argv[2], "--")) {
		fprintf(stderr, "Usage: %s <output> -- <cmdline>\n", argv[0]);
		return rc;
	}
	out = argv[1];
	shm_unlink(out);
	fd = shm_open(out, O_CREAT | O_RDWR, 0666);
	r = ftruncate(fd, LOGSZ);
	assert(!r);
	buf = mmap(0, LOGSZ, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
	snprintf(so, sizeof(so), "%s.so", argv[0]);
#ifdef __APPLE__
	setenv("DYLD_INSERT_LIBRARIES", so, 1);
	setenv("DYLD_FORCE_FLAT_NAMESPACE", "1", 1);
#else
	setenv("LD_PRELOAD", so, 1);
#endif
	setenv(ENVOUT, out, 1);
	posix_spawnp(&child, argv[3], 0, 0, argv + 3, environ);
	waitpid(child, &rc, 0);
	dump(out, buf);
	munmap(buf, LOGSZ);
	close(fd);
	shm_unlink(out);
	return rc;
}
