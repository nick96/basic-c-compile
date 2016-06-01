;;; basic-c-compile.el --- Quickly create a basic Makefile, compile and run a C program.

;; Copyright (C) 2016 Nick Spain

;; Author: Nick Spain <nicholas.spain96@gmail.com>
;; Version: 1.0
;; Keywords: C, Makefile, compilation
;; URL: https://github.com/nick96/basic-c-compile


;;; Commentary:

;; basic-c-compile.el is a basic script for C programming.  It can create a basic
;; Makefile, compile the C program (with or without the Makefile) and run the
;; file.

;; This package assumes that a single C file is being compiled, however, the
;; Makefile is easily edited.

;;;###autoload
(defun basic-c-compile-file ()
  (interactive)
  (compile-sans-makefile)
  (compile-with-makefile)
  (create-makefile)
  (run-c-file))

;;; Code:


;; Compile file to output file of same name

;; Compile without Makefile
(defun compile-sans-makefile ()
  "Compiles current file without the need for a Makefile."
  (shell-command-to-string (format "gcc -Wall -o %s %s"
                                   (file-name-sans-extension (buffer-name))
                                   (buffer-name))))


;; Compile with Makefile
(defun compile-with-makefile ()
  "Compile current file with the Makefile in the same directory."
  (shell-command-to-string "make"))


;; Create a Makefile

;; Contents of Makefile
(defvar makefile-contents
  (format (concat "CC=gcc\n"
                  "INFILE=%s\n"
                  "OUTFILE=%s.o\n\n"
                  "build: $(INFILE)\n\t"
                  "$(CC) -o $(OUTFILE) $(INFILE)\n\n"
                  "clean:\n\t rm -f *.o \n\n"
                  "rebuild: clean build")
          (buffer-name) (file-name-sans-extension (buffer-name))))

(defun create-makefile ()
  "Create a basic Makefile for the current file, in the same directory."
  (write-region
   makefile-contents
   nil
   "Makefile"))


;; Run file
(defun run-c-file ()
  "Run the file with the output printing in a temporary buffer."
  (shell-command-to-string
   (format "./%s"
           (file-name-sans-extension (buffer-name)))))

(provide 'basic-c-compile)
;;; basic-c-compile.el ends here
