#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

char* replacechr (char* chs, char targ, char rep);

const int WIN32 = 1;
const int OTHER = 2;
// figure out if windows or "other" (linux)
#ifdef _WIN32
const int os = 1;
#else
const int os = 2;
#endif


int main (int argc, char *argv[]) {
    int status = 0;
    char query[300];

    if (argc == 1) {
        strcpy (query, "racket -e \"(require cm/core/repl)\"");
    } else if (argc == 2 && !(strcmp (argv[1], "-install"))) {
        if (os == WIN32) {
            strcpy (query, ".\\install.ps1");
        } else {
            strcpy (query, "./install.sh");
        }
    } else if (argc == 2) {
        // act like path is unix
        strcpy (argv[1], (replacechr (argv[1], '\\', '/')));
        sprintf (query,
                "racket -e \"(require cm/core/cm) (cm-run-file \\\"%s\\\")\"",
                argv[1]);
    } else if (argc == 3 && !(strcmp (argv[1], "-s"))) {
        strcpy (argv[2], (replacechr (argv[2], '\\', '/')));
        sprintf (query,
                "racket -e \"(require cm/core/cm) (cm-run-file-silent \\\"%s\\\")\"",
                argv[2]);
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
