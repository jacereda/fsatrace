#include <assert.h>
#include <sys/mman.h>
#include <sys/wait.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#include "../shm.h"

struct priv {
	int    fd;
	size_t sz;
};

static unsigned long
hash(unsigned char *str)
{
	unsigned long h = 5381;
	int	      c;
	while ((c = *str++))
		h = ((h << 5) + h) + c;
	return h;
}

#ifdef __linux__
#include <stdlib.h>
// On Linux, use memfd_create() to generate the shared memory region.
// This ensures that is will be properly reclaimed when all references
// to it are destroyed, unlike /dev/shm segments which persist when
// the program crashes.
int
shmInit(struct shm *shm, const char *key, size_t sz, int root)
{
	int	     err = 0;
	struct priv *p = (struct priv *)shm->storage;
	if (root) {
		char *shname = shm->name;
		assert(key);
		assert(sizeof(shm->storage) > sizeof(*p));
		snprintf(shname, sizeof(shm->name), "/fsatrace%ld",
		    hash((unsigned char *)key));
		for (size_t i = 0, l = strlen(shname); i < l; i++)
			if (shname[i] == '/')
				shname[i] = '_';
		// IMPORTANT: Do not set MFD_CLOEXEC to ensure the file
		// descriptor will be copied during exec().
		err += (-1 == (p->fd = memfd_create(shname, 0))) << 0;
		err += (root && (-1 == ftruncate(p->fd, sz))) << 1;
		err += (MAP_FAILED ==
			   (shm->buf = mmap(0, sz, PROT_READ | PROT_WRITE,
				MAP_SHARED, p->fd, 0)))
		    << 2;
		// Save fd as environment variable to be passed through exec().
		char fd_val[8];
		snprintf(fd_val, sizeof(fd_val), "%d", p->fd);
		err += (-1 == setenv("FSATRACE_SHM_FD", fd_val, 1)) << 3;
		p->sz = sz;
	} else {
		const char *fd_str = getenv("FSATRACE_SHM_FD");
		assert(fd_str);
		p->fd = atoi(fd_str);
		p->sz = sz;
		err += (MAP_FAILED ==
			   (shm->buf = mmap(0, sz, PROT_READ | PROT_WRITE,
				MAP_SHARED, p->fd, 0)))
		    << 2;
	}
	return err;
}

int
shmTerm(struct shm *shm, int root)
{
	struct priv *p = (struct priv *)shm->storage;
	int	     err = 0;
	err += (-1 == munmap(shm->buf, p->sz)) << 0;
	err += (-1 == close(p->fd)) << 1;
	return err;
}

#else // !__linux__
int
shmInit(struct shm *shm, const char *key, size_t sz, int root)
{
	int	     err = 0;
	struct priv *p = (struct priv *)shm->storage;
	char *	     shname = shm->name;
	assert(key);
	assert(sizeof(shm->storage) > sizeof(*p));
	snprintf(shname, sizeof(shm->name), "/fsatrace%ld",
	    hash((unsigned char *)key));
	for (size_t i = 0, l = strlen(shname); i < l; i++)
		if (shname[i] == '/')
			shname[i] = '_';
	if (root)
		shm_unlink(shname); // Just in case, it might have crashed on a
				    // previous execution
	err +=
	    (-1 == (p->fd = shm_open(shname, (root * O_CREAT) | O_RDWR, 0666)))
	    << 0;
	err += (root && (-1 == ftruncate(p->fd, sz))) << 1;
	err += (MAP_FAILED ==
		   (shm->buf = mmap(
			0, sz, PROT_READ | PROT_WRITE, MAP_SHARED, p->fd, 0)))
	    << 2;
	p->sz = sz;
	return err;
}

int
shmTerm(struct shm *shm, int root)
{
	struct priv *p = (struct priv *)shm->storage;
	int	     err = 0;
	err += (-1 == munmap(shm->buf, p->sz)) << 0;
	err += (-1 == close(p->fd)) << 1;
	err += (root && (-1 == shm_unlink(shm->name))) << 2;
	return err;
}
#endif // !__linux__
