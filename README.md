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

Compilation can be done with or without the Makefile `compile-with-makefile` 
will compile the current file using the makefile in the same directory. 
`compile-sans-makefile` will compile the file without the Makefile (using gcc).

The file can also be run using the `run-current-buffer`. This will print the
output to a temporary buffer which can be quit by pressing 'q'.
