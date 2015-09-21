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
#include "fsatrace.h"

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

unsigned long
hash(unsigned char *str)
{
  unsigned long h = 5381;
  int c;
  while ((c = *str++))
    h = ((h << 5) + h) + c;
  return h;
}

int
main(int argc, char **argv)
{
	void           *buf;
	int		fd;
	int		r;
	char		so        [PATH_MAX];
	char		shname    [PATH_MAX];
	int		rc = EXIT_FAILURE;
	int		child;
	const char     *out;
	if (argc < 4 || strcmp(argv[2], "--")) {
		fprintf(stderr, "Usage: %s <output> -- <cmdline>\n", argv[0]);
		return rc;
	}
	out = argv[1];
	snprintf(shname, sizeof(shname), "/%ld", hash((unsigned char *)out));
	for (size_t i = 0, l = strlen(shname); i < l; i++)
		if (shname[i] == '/')
			shname[i] = '_';
	shm_unlink(shname);
	fd = shm_open(shname, O_CREAT | O_RDWR, 0666);
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
	setenv(ENVOUT, shname, 1);
	child = fork();
	if (!child) {
		execvp(argv[3], argv + 3);
		assert(0);
	}
	r = wait(&rc);
	assert(r >= 0);
	rc = WEXITSTATUS(rc);
	if (!rc)
		dump(out, buf);
	munmap(buf, LOGSZ);
	close(fd);
	shm_unlink(shname);
	return rc;
}
