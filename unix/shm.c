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
	int		fd;
	size_t		sz;
};

static unsigned long
hash(unsigned char *str)
{
	unsigned long	h = 5381;
	int		c;
	while ((c = *str++))
		h = ((h << 5) + h) + c;
	return h;
}

int
shmInit(struct shm *shm, const char *key, size_t sz)
{
	int		err = 0;
	struct priv    *p = (struct priv *)shm->storage;
	char           *shname = shm->name;
	assert(sizeof(shm->storage) > sizeof(*p));
	snprintf(shname, sizeof(shm->storage), "/fsatrace%ld", hash((unsigned char *)key));
	for (size_t i = 0, l = strlen(shname); i < l; i++)
		if (shname[i] == '/')
			shname[i] = '_';
	shm_unlink(shname); // Just in case, it might have crashed on a previous execution
	err += (-1 == (p->fd = shm_open(shname, O_CREAT | O_RDWR, 0666))) << 0;
	err += (-1 == ftruncate(p->fd, sz)) << 1;
	err += (MAP_FAILED == (shm->buf = mmap(0, sz, PROT_READ | PROT_WRITE, MAP_SHARED, p->fd, 0))) << 2;
	p->sz = sz;
	return err;
}

int
shmTerm(struct shm *shm)
{
	struct priv    *p = (struct priv *)shm->storage;
	int		err = 0;
	err += (-1 == munmap(shm->buf, p->sz)) << 0;
	err += (-1 == close(p->fd)) << 1;
	err += (-1 == shm_unlink(shm->name)) << 2;
	return err;
}
