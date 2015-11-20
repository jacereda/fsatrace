enum procerr {
	ERR_PROC_OK,
	ERR_PROC_FORK,
	ERR_PROC_EXEC,
	ERR_PROC_WAIT,
};

void		procPath  (char *);
enum procerr	procRun(unsigned, const char * const *, int *);
