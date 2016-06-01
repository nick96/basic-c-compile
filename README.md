# Basic C compile

This is a script for the basic actions involved in compiling and running a C file.
It will be able to create a basic Makefile of the form:


``` Makefile
CC=gcc
INFILE=<current-file>
OUTFILE=<current-file-without-extension>.o

build: %(INFILE)
    $(CC) -Wall -o $(OUTFILE) $(INFILE)

clean:
    rm -rf *.o

rebuild: clean build
```

File can be compiled with or without a Makefile. This is done by calling
`compile-file`. This gives the option (y or n) of compiling with or without
a makefile. If yes, the program checks for a Makefile. If there is one present
it calls `make rebuild` (assuming the style of the  Makefile shown above).
Otherwise, the program creates the Makefile and compiles the file. If you want
to compile the file without a Makefile, press "n" at the prompt and
`gcc -Wall -o <file-sans-extension>.o <file>` will be calls instead.

The file can also be run using  `run-c-file`. This will print the
output to a temporary buffer which can be quit by pressing 'q'.

This package assumes that a single C file is being compiled, however, it is easy
to edit the Makefile to allow multiple files to be compiled together.
