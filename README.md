# Basic C compile

This is my first Emacs package so any and all constructive criticism
is appreciated.

This is a script for the basic actions involved in compiling and
running a C file.  It will be able to create a basic Makefile of the
form:

``` Makefile 
CC=gcc 
INFILE=<file-name>.c 
OUTFILE=<file-name>.o

build: %(INFILE) 
    $(CC) -Wall -o $(OUTFILE) $(INFILE)

clean: 
    rm -rf *.o

rebuild: clean build 
```

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
`gcc -Wall -o <file-name>.o <file-name>.c` will be run. 

## basic-c-compile-makefile

This is a really simple command that just creates a Makefile in the form
shown above.

At this point in time the Makefile is only build for a single file
with a `.c` extension but it would be quite easy to edit this in. The
main reason it is not implemented is because I've never had to compile
multiple files together.

## basic-c-compile-run-c

This command runs the output file `<file-name>.o` with `./<file-name>.o`.

# TODO
- Allow a choice of compiler
	- At the moment this has to be done by editing Makefile
- Make compatible with windows
	- Recognise system and change things like `./<file-name>.o`
- Recognise if the file has multiple files and give the following option:
	1. Choose files to be included in compilation
	2. Include all files in compilation
	3. Only include current file in compilation





