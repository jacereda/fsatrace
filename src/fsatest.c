

/* Take a command line of options to perform
   rfile -- file to read
   wfile -- file to write
   e[command] -- command to execute
   f -- raise a failure
*/

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <stdlib.h>


void unescape(char* s)
{
    for (int i = 0; s[i]; i++) {
        if (s[i] != '#') continue;

        if (s[i+1] != '#') {
            s[i] = ' ';
        } else {
            memmove(&s[i], &s[i+1], strlen(&s[i+1]));
            i++;
        }
    }
}

int main(int argc, const char* argv[])
{
    int trace = 0;
    int exitCode = 0;
    for (int i = 1; i < argc; i++) {
        const char* s = argv[i];
        FILE* fp;
        char* cmdline;

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

            case 'f':
                if (trace) printf("Will return a failing exit code\n");
                exitCode = 1;
                break;

            case 'e':
                cmdline = strdup(&s[1]);
                unescape(cmdline);
                if (trace) printf("Running command: %s\n", cmdline);
                system(cmdline);
                free(cmdline);
                break;

            default:
                printf("FAILED: Could not interpret command line: %s\n", s);
                return 1;
        }
    }
    return exitCode;
}
