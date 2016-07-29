# Basic C compile

This is my first Emacs package so any and all constructive criticism
is appreciated.

This is a script for the basic actions involved in compiling and
running a C file.  It will be able to create a basic Makefile of the
form:

``` Makefile
CC = <compiler set by basic-c-compile-compiler>
INFILE= <file-name>.c
OUTFILE= <file-name>.o

build: %(INFILE)
    $(CC) -Wall $(INFILE) -o $(OUTFILE)

clean:
    rm -rf *.o

rebuild: clean build
```

*Note*: `INFILE` can also be a list of files depending on what
`basic-c-compile-all-files` is set to.

This package allows three commands to be called: `basic-c-compile-file`,
`basic-c-compile-makefile` and `basic-c-compile-run-c`.

## basic-c-compile-file

This command initially checks if you want to compile
with or without a Makefile (just press 'y' or 'n').  If you press 'y',
it will check for a Makefile in the file's directory. If there is one
present, it will check for of the form `<file-name>.o`. Upon finding
one, the package will run the command `make rebuild`. This assumes the
Makefile is structured as expected. If no file is found, then `make
build` will be run. If you choose to run without a Makefile,
`gcc -Wall -o  <file-name>.c <file-name>.o` will be run.

## basic-c-compile-makefile

This command creates a Makefile in the form shown above.

If the variable `basic-c-compile-all-files` is set to "all", then all
the files in the directory with the `.c` extension will be
compiled. If set to "selected" you will be prompted to write a list of
the files to be compiled. The files should be separated by a space. If
`basic-c-compile-all-files` is set to anything else, only the current
file will be included.

## basic-c-compile-run-c

This command runs the output file `<file-name>.o` with `./<file-name>.o`.

## Settings

All customisable setting can be found in the customisation menu
(`M-x customize-variable`). They are in the group `basic-c-compile` which is in the `tools` section. You can also change them in your init file with:
`(setq <variable-name> <variable-value>)`.

1. `basic-c-compile-compiler`: This chooses the compiler that is used
   to compile the file(s). By default it is "gcc"".
2. `basic-c-compile-all-files`: This is explained above but just
   quickly, it allows you to choose which files are compiled. By
   default it is "all".
3. `basic-c-compile-compiler-flags`: This is one string containing all
   flags files are compiled with. By default it is "-Wall".
4. `basic-c-compiler-auto-comp`: This is a boolean variable defining
   whether or not out of date binary files are compiled. If it is
   true, then when `basic-c-compile-run-c` is called, it will check if
   the file's binary is up to date. If it isn't, then
   `basic-c-compile-file` will be called.
