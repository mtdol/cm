#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

char* replacechr (char* chs, char targ, char rep);
int handle_invalid_args ();

const int win32 = 1;
const int other = 2;
// figure out if windows or "other" (linux)
#ifdef _WIN32
const int os = 1;
#else
const int os = 2;
#endif

const char pkg_help[] = "Use --install to install or reset the module system.\n";
const char help[] = 
                "No args: run the repl\n"
                "file: run a file\n"
                "-f file: run a file\n"
                "-e: run an expression\n"
                "-st: run a statement\n"
                "--pkg: package manager\n";

int main (int argc, char *argv[]) {
    int status = 0;
    char str[300];
    char query[300];
    strcpy (query, "");

    if (argc == 1) {
        strcpy (query, "racket -e \"(require cm/core/repl)\"");

    // package manager options
    } else if (argc >= 2 && !(strcmp (argv[1], "--pkg"))) {
        if (argc == 2 || (argc == 3 && 
                    (!(strcmp (argv[2], "--help")) || (strcmp (argv[2], "-h"))))) {
            printf (pkg_help);
            return 0;
        } else if (argc == 3 && !(strcmp (argv[2], "--install"))) {
            if (os == win32) {
                strcpy (query, ".\\install.ps1");
            } else {
                strcpy (query, "./install.sh");
            }
        } else {
            handle_invalid_args();
        }
    } else if (argc == 2 && 
                    (!(strcmp (argv[1], "--help")) ||
                     !(strcmp (argv[1], "-h")))) {
        printf (help);
        return 0;
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
    } else if (argc == 3 && !(strcmp (argv[1], "-st"))) {
        strcpy (str, argv[2]);
        sprintf (query,
                "racket -e \"(require cm/core/main) (display-output (run \\\"%s\\\"))\"",
                str);
        
    } else if (argc == 3 && !(strcmp (argv[1], "-e"))) {
        strcpy (str, argv[2]);
        sprintf (query,
                "racket -e \"(require cm/core/main) (display-expr-output (run-expr \\\"%s\\\"))\"",
                str);
        
    } else {
        handle_invalid_args();
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

// exits the program
int handle_invalid_args () {
    printf ("Invalid args.\n");
    return 1;
}
