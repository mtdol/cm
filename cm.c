#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

char* replacechr (char* chs, char targ, char rep);

const int win32 = 1;
const int other = 2;
// figure out if windows or "other" (linux)
#ifdef _WIN32
const int os = 1;
#else
const int os = 2;
#endif


int main (int argc, char *argv[]) {
    int status = 0;
    char str[300];
    char query[300];

    if (argc == 1) {
        strcpy (query, "racket -e \"(require cm/core/repl)\"");
    } else if (argc == 2 && !(strcmp (argv[1], "-install"))) {
        if (os == win32) {
            strcpy (query, ".\\install.ps1");
        } else {
            strcpy (query, "./install.sh");
        }
    } else if (argc == 2 || (argc == 3 && !(strcmp (argv[1], "-f")))) {
        if (argc == 2) {
            strcpy (str, argv[1]);
        } else {
            strcpy (str, argv[2]);
        }

        // act like path is unix
        strcpy (str, (replacechr (str, '\\', '/')));
        sprintf (query,
                "racket -e \"(require cm/core/main) (silent (run-file \\\"%s\\\"))\"",
                str);
    } else if (argc == 3 && !(strcmp (argv[1], "-F"))) {
        strcpy (str, argv[2]);

        // act like path is unix
        strcpy (str, (replacechr (str, '\\', '/')));
        sprintf (query,
                "racket -e \"(require cm/core/main) (silent (run-file-abs \\\"%s\\\"))\"",
                str);
    } else {
        printf ("Invalid args.\n");
        return 1;
    }
    
    status = system (query);
    return status;

}

// replaces all instances of targ in chs with rep
char* replacechr (char* chs, char targ, char rep) {
    int i = 0;
    for (i = 0; chs[i]; i++) {
        if (chs[i] == targ) chs[i] = rep;        
    }
    return chs;
}
