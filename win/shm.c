#include <assert.h>
#include <windows.h>
#include <limits.h>

#include "../shm.h"

struct priv {
	HANDLE mf;
};

int
shmInit(struct shm *shm, const char *key, size_t sz)
{
	int i;
	int err = 0;
	char * shname = shm->name;
	struct priv * p = (struct priv *)shm->storage;
	assert(sizeof(shm->storage) > sizeof(p));
	for (i = 0; key[i]; i++)
        shname[i] = key[i] == '\\' || key[i] == ':'? '/' : key[i];
    shname[i] = 0;
    err += (0 == (p->mf = CreateFileMappingA(INVALID_HANDLE_VALUE, 0, PAGE_READWRITE,
											 0, sz, shname))) << 0;
	err += (0 == (shm->buf = MapViewOfFile(p->mf, FILE_MAP_ALL_ACCESS, 0, 0, sz))) << 1;
	return err;
}

int
shmTerm(struct shm *shm)
{
	int err = 0;
	struct priv * p = (struct priv *)shm->storage;
    err += !UnmapViewOfFile(shm->buf) << 0;
    err += !CloseHandle(p->mf) << 1;
	return err;
}
