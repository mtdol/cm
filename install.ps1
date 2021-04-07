$cm_loc = $PSScriptRoot
$std_loc = join-path $cm_loc std_lib\

# set the location of the cm directory
set-content (join-path $cm_loc config\ cm_loc.txt) `
    $cm_loc
# create a basic modules file
set-content (join-path $cm_loc config\ modules.txt) `
    "# this is a comment`n""std_lib"":""$std_loc"""
