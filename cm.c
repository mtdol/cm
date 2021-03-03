#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[]) {
    int status = 0;
    char arg[200];
    
    if (argc == 1) {
        status = system ("racket core/repl.rkt");
    } else if (argc == 2) {
        // treats argv[1] as a file and runs it
        sprintf (arg, "racket -e '(require cm/core/cm) (cm-run-file \"%s\")'", argv[1]);
        status = system (arg);
    } else {
        printf ("Invalid number of args.\n");
        return 1;
    }

    return status;

}
