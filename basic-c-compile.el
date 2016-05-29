;; Small Emacs script for basic C file commands
;; It has the following functionality:
                                        ; Create a simple Makefile for a C file
                                        ;   in the same directory as the file
                                        ; Compile the file to an output file of the same name
                                        ; Run the output file and print the output to a temporary buffer


;; Compile file to output file of same name

;; Compile without Makefile
(defun compile-sans-makefile ()
  "Compiles current file without the need for a Makefile"
  (shell-command-to-string (format "gcc -Wall -o %s %s"
                                   (file-name-sans-extension (buffer-file-name))
                                   (buffer-file-name))))

;; Compile with Makefile
(defun compile-with-makefile ()
  "Compile current file with the Makefile in the same directory"
  (shell-command-to-string "make"))



;; Create a Makefile
(defun create-makefile ()
  "Create a basic Makefile for the current file, in the same directory"
  (let (makefile-contents
        (concat "CC=gcc\n INFILE=%s\n OUTFILE=%s\n\n build: %(INFILE)\n\t $(CC) -o $(OUTFILE) %(INFILE)\n\n"
                "clean:\n\t rm -f *.o \n\n"
                "rebuild: clean build")
        (buffer-file-name) (file-name-sans-extension (buffer-file-name)))
    (shell-command-to-string (format "cd %s && touch Makefile && echo %s >> Makefile"
                                     (file-name-directory (buffer-file-name)) makefile-contents))))


