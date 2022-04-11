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

int
shmInit(struct shm *shm, const char *key, size_t sz, int root)
{
	struct priv *p = (struct priv *)shm->storage;
	char *	     shname = shm->name;
	assert(key);
	assert(sizeof(shm->storage) > sizeof(*p));
	snprintf(shname, sizeof(shm->storage), "/fsatrace%ld",
	    hash((unsigned char *)key));
	for (size_t i = 0, l = strlen(shname); i < l; i++)
		if (shname[i] == '/')
			shname[i] = '_';
	if (root)
		shm_unlink(shname); // Just in case, it might have crashed on a
				    // previous execution
	if (-1 ==
	    (p->fd =
		    shm_open(shname, (root * O_CREAT) | O_RDWR | O_EXCL, 0666)))
		return 1 << 0;
	if (root && (-1 == ftruncate(p->fd, sz)))
		return 1 << 1;
	if (MAP_FAILED ==
	    (shm->buf =
		    mmap(0, sz, PROT_READ | PROT_WRITE, MAP_SHARED, p->fd, 0)))
		return 1 << 2;
	p->sz = sz;
	return 0;
}

int
shmTerm(struct shm *shm, int root)
{
	struct priv *p = (struct priv *)shm->storage;
	if (-1 == munmap(shm->buf, p->sz))
		return 1 << 0;
	if (-1 == close(p->fd))
		return 1 << 1;
	if (root && (-1 == shm_unlink(shm->name)))
		return 1 << 2;
	return 0;
}
