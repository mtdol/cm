#!/bin/bash

# get the directory path of the script
dir_path=$(dirname `readlink -f $0`)

# set the cm path in a file
printf '%s' $dir_path > ./config/cm_loc.txt
# set up a bare modules file
printf '# this is a comment\n"std_lib":"%s"' $dir_path/std_lib/ > ./config/modules.txt
