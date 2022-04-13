#include <stdbool.h>

struct shm {
	char  *buf;
	char   name[PATH_MAX];
	char   storage[32];
	size_t buf_size;
	bool   buf_overflow;
};
int shmInit(struct shm *, const char *, size_t, int);
int shmTerm(struct shm *, int);
