# cm
Implementation of the cm language.

Development in progress.

[Full Docs](https://github.com/mtdol/cm/wiki)

## Installation
[Racket is required to run cm.](https://racket-lang.org/)

Use the following to install cm as a package:
```
cd "path to cm dir"
raco pkg install
```

The modules system must also be installed:
```
cm.rkt --install
```

On windows make sure to set the default application to run `.rkt` files to `racket.exe`,
else you will have to run `cm.rkt` with the racket application every time:
```
racket cm.rkt args
```

## Repl
Use
```
cm.rkt
```
in language directory to load the REPL and test language expressions.  

## Running Files
`cm.rkt` can be used to run cm files:  
```
cm.rkt "path to file"
```
The -f switch is normally implied and indicates a file arg.
```
# -f is not really necessary
cm.rkt -f "path to file"
```

Some files will have a `main` function that takes in a list of strings as an argument.  
```
defun main (args) := ...
```

`cm.rkt` will automatically attempt to call any function called `main` with the arguments  
given to it on the command line.  
```
cm.rkt file args
```

To learn further about how to use cm.exe, use the `--help` or `-h` switch to display help text.
```
cm.rkt --help
```

There are example files in the `examples` directory to test and explore `cm.rkt`.  

## Modules
There is a file under config/ called `modules.txt`.
This file is used by cm to determine the absolute paths of modules that can be imported with the `load` keyword.

The file has this form:
```
# comment
"module_name":"absolute_module_path"
# another comment
"module2":"path_to_module2"
```

The `cm.rkt` script contains a package manager that can be run using the `--pkg` switch.  
This package manager will allow you to more easily add modules without having to manually edit the
modules file in your editor.

The module installation during setup replaces or adds the `modules.txt` file and fills it
with the `std_lib` module (required for the repl.)

### import

To import files for use in your program use the `import` macro with this form:
```
-- with no prefix
#:import "file1", "file2", "file3"

-- with a prefix of "f_"
#:import:f_ "file1", "file2", "file3"
```

The strings used on the right side of the `import` macro take one of these three forms:
```
"module::path_to_file_in_module"

"path_to_file"

-- same as without the `m:`, but explicit
"m:module::path_to_file_in_module"

-- same as without `f:`, but explicit
"f:path_to_file"
```
For example, `std_lib/std.cm` can be loaded with:
```
-- as a module
#:import "std_lib::std.cm"

-- as an absolute path
#:import "~/code/cm/std_lib/std.cm"

-- relative to working directory
#:import "cm/std_lib/std.cm"

-- with an "std_" prefix
#:import:std_ "std_lib::std"
```

### lazy_import

To avoid cycles in the import graph, there is an additional `lazy_import` macro. `lazy_import` takes items that have
already been imported into the global context and provides references to them in the current module space.

`lazy_import` is used with the following syntax:
```
-- no prefix
#:lazy_import "file" -> var var_name, type type_name, macro macro_name

-- prefix of "pref_"
#:lazy_import:pref_ "file" -> var var_name, type type_name, macro macro_name
```
The element to the left of the `->` must be a file name string and will be the file that the items will be provided from.   
The elements to the right will be a comma seperated list of `var`, `type`, or `macro` followed by the name
of what you wish to import.

`,` and `->` must be escaped by enclosing the affected name in quotes. Besides escaping, quotes are optional.
```
-- yes
#:lazy_import "file.cm" -> macro "a,b"

-- no
#:lazy_import "file.cm" -> macro a,b

-- fine, since macro is simply named
#:lazy_import "file.cm" -> macro ab
```

An example, using `std.cm`:
```
#:lazy_import:std_ "std_lib::std.cm" -> var add1, var pos?, macro "%"

> std_add1:2
3
> {std_% @1|@2}
1
2
```

### #:lang cm
The macro `#:lang cm` can be used (usually at the top of a file)
to load all of the standard modules that are recommended for the language.

```
#:lang cm

-- a function included in std.cm
@ add1:4.
```
The macro imports various files from the `std_lib` folder.

## Testing
```
cd "path to cm dir"
raco test test/*
```
