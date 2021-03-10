$std_loc = join-path $PWD std_lib\
set-content ./config/modules.txt "# this is a comment`n""std_lib"":""$std_loc"""
