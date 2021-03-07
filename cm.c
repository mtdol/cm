#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int main (int argc, char *argv[]) {
    int status = 0;
    char arg[200];
    char *args[] = {"racket", "-e", NULL, NULL};
    
    if (argc == 1) {
        args[2] = "(require cm/core/repl)";
    } else if (argc == 2) {
        sprintf (arg, "(require cm/core/cm) (cm-run-file \"%s\")", argv[1]);
        args[2] = arg;
    } else {
        printf ("Invalid number of args.\n");
        return 1;
    }

    status = execvp (args[0], args);
    return status;

}
