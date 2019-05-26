

/* Take a command line of options to perform
   rfile -- file to read
   wfile -- file to write
   e[command] -- command to execute
   sN -- sleep for N seconds
   f -- raise a failure
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#ifndef _WIN32_WINNT
#include <spawn.h>
#endif

void unescape(char* s)
{
    // w = write index, r = read index
    int w = 0;
    for (int r = 0; s[r]; r++) {
        if (s[r] != '#')
            s[w++] = s[r];
        else if (s[r+1] != '#')
            s[w++] = ' ';
        else
            s[w++] = s[++r];
    }
    s[w] = 0;
}

void exec(char* s)
{
    char* cmd;
    char* args[1000];
    cmd = strtok (s, " ");
    args[0] = cmd;
    for (int i = 1; i < 1000; i++) {
        args[i] = strtok(NULL, " ");
        if (args[i] == NULL)
            break;
    }
    spawnv(P_WAIT, cmd, args);
}

int main(int argc, const char* argv[])
{
    int trace = 0;
    int exitCode = 0;

    for (int i = 1; i < argc; i++) {
        const char* s = argv[i];
        FILE* fp;
        char* cmdline;
        int seconds;

        switch (s[0]) {
            case 'r':
                if (trace) printf("Reading from: %s\n", &s[1]);
                fp = fopen(&s[1], "r");
                while(fgetc(fp) != EOF)
                    ; // nothing
                fclose(fp);
                break;

            case 'w':
                if (trace) printf("Writing to: %s\n", &s[1]);
                fp = fopen(&s[1], "w");
                fputs("Written by fsatest harness\n", fp);
                fclose(fp);
                break;

            case 's':
                seconds = atoi(&s[i]);
                if (trace) printf("Sleeping for: %is\n", seconds);
                sleep(seconds);
                break;

            case 'f':
                if (trace) printf("Will return a failing exit code\n");
                exitCode = 1;
                break;

            case 'e':
                cmdline = strdup(&s[1]);
                unescape(cmdline);
                if (trace) printf("Running command: %s\n", cmdline);
                exec(cmdline);
                free(cmdline);
                break;

            default:
                printf("FAILED: Could not interpret command line: %s\n", s);
                return 1;
        }
    }
    return exitCode;
}
