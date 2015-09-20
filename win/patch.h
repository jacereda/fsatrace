void patchInit();
void patchTerm();
void patchInstall(void *orig, void *hook, void **preal, const char *nm);
int patchInstalled();
