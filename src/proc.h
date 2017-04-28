enum procerr {
	ERR_PROC_OK,
	ERR_PROC_FORK,
	ERR_PROC_EXEC,
	ERR_PROC_SIGNALED,
	ERR_PROC_STOPPED,
	ERR_PROC_WAIT,
	ERR_PROC_UNKNOWN, 
};

void		procPath  (char *);
void procDumpArgs(unsigned, char * const []);
enum procerr	procRun(unsigned, char * const [], int *);
