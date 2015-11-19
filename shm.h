struct shm {
	char           *buf;
	char		name      [PATH_MAX];
	char		storage   [32];
};
int		shmInit    (struct shm *, const char *, size_t);
int		shmTerm    (struct shm *);
