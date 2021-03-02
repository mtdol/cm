#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[]) {
    int status = 0;

    /*if (argc == 2) {*/
        /*printf ("Arg supplied: %s\n", argv[1]);*/
    /*} else {*/
        /*printf ("cm requires a single argument.\n");*/
        /*return 1;*/
    /*}*/

    // TODO: add more than just repl
    status = system ("racket core/repl.rkt");

    return status;

}
