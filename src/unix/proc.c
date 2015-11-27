#include <assert.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <limits.h>
#include <unistd.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#if !defined __linux__
#include <libproc.h>
#endif
#include "../proc.h"

void
procPath(char *fullpath)
{
#if defined __linux__
	char		exepath   [64];
	ssize_t		ret;
	snprintf(exepath, sizeof(exepath), "/proc/%d/exe", getpid());
	ret = readlink(exepath, fullpath, PATH_MAX);
	assert(ret != -1);
#else
	proc_pidpath  (getpid(), fullpath, PATH_MAX);
#endif
}

void
procDumpArgs(unsigned nargs, char *const args[])
{
	unsigned i;
	for (i = 0; i < nargs; i++)
		fprintf(stderr, "argv[%d]=%s\n", i, args[i]);
}

enum procerr
procRun(unsigned nargs, char *const args[], int *rc)
{
	extern char   **environ;
	int		ret;
	int		child;
	char		so        [PATH_MAX];
	char		fullpath  [PATH_MAX];
	procPath(fullpath);
	snprintf(so, sizeof(so), "%s.so", fullpath);
#if defined __linux__
	setenv("LD_PRELOAD", so, 1);
#else
	setenv("DYLD_INSERT_LIBRARIES", so, 1);
	setenv("DYLD_FORCE_FLAT_NAMESPACE", "1", 1);
#endif
	if (posix_spawnp(&child, args[0], 0, 0, args, environ))
		ret = ERR_PROC_FORK;
	else if (-1 != waitpid(child, rc, 0)) {
		if (WIFEXITED(*rc)) {
			ret = ERR_PROC_OK;
			*rc = WEXITSTATUS(*rc);
		} else
			ret = ERR_PROC_EXEC;
	} else
		ret = ERR_PROC_WAIT;
	return ret;
}
