CC = gcc
INFILE = 
OUTFILE = basic-c-compile.o

build: $(INFILE)
	$(CC) -Wall $(INFILE)  -o $(OUTFILE)

clean:
	 rm -f *.o 

rebuild: clean build