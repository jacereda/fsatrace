enum procerr {
	ERR_PROC_FORK = ((int)'fork'),
	ERR_PROC_EXEC = ((int)'exec'),
	ERR_PROC_WAIT = ((int)'wait'),
};

void		procPath  (char *);
enum procerr	procRun(const char *, char **);
