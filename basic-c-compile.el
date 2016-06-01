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
(defun compile-file ()
  "Compile file with or without a Makefile."
  (interactive)
  (let ((path (shell-quote-argument (file-name-directory (buffer-file-name))))
        (file (shell-quote-argument (buffer-file-name))))
    (if (y-or-n-p "Compile with Makefile?")
        (dolist (file-name (directory-files path))))
          (if (file-exists-p "Makefile")
              (if (file-exists-p (file-name-sans-extension file))
                  (compile-with-makefile "rebuild")
                (compile-with-makefile "build"))
            (create-makefile file))
      (compile-sans-makefile file)))

(defun run-c ()
  "Run the program."
  (interactive)
  (run-c-file (buffer-file-name)))


;;; Code:


;; Compile file to output file of same name

;; Compile without Makefile
(defun compile-sans-makefile (file)
  "Compiles FILE without the need for a Makefile."
  (compile (format "gcc -Wall -o %s %s"
                   (file-name-sans-extension file)
                   file)))


;; Compile with Makefile
(defun compile-with-makefile (arg)
  "Compile FILE with the Makefile in the same directory."
  (compile (format "make %s" arg)))


;; Create a Makefile

;; Contents of Makefile

(defun create-makefile (file)
  "Create a basic Makefile for FILE, in the same directory."
  (let ((makefile-contents
         (format (concat "CC=gcc\n"
                         "INFILE=%s\n"
                         "OUTFILE=%s.o\n\n"
                         "build: $(INFILE)\n\t"
                         "$(CC) -Wall -o $(OUTFILE) $(INFILE)\n\n"
                         "clean:\n\t rm -f *.o \n\n"
                         "rebuild: clean build")
                 file (file-name-sans-extension file))))
  (write-region
   makefile-contents
   nil
   (concat (file-name-directory file) "Makefile"))))


;; Run file
(defun run-c-file (file)
  "Run FILE with the output printing in a temporary buffer."
  (shell-command-to-string
   (format "./%s"
           (file-name-sans-extension file))))



(provide 'basic-c-compile)
;;; basic-c-compile.el ends here
