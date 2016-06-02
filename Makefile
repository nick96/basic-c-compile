CC=gcc
INFILE=/Users/nickspain/.emacs.d/user_packages/basic_c_compile/basic-c-compile.el
OUTFILE=/Users/nickspain/.emacs.d/user_packages/basic_c_compile/basic-c-compile.o

build: $(INFILE)
	$(CC) -Wall -o $(OUTFILE) $(INFILE)

clean:
	 rm -f *.o 

rebuild: clean build