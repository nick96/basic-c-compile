;;; basic-c-compile.el --- Quickly create a basic Makefile, compile and run a C program.

;; The MIT License (MIT)

;; Copyright (c) 2016 nick96

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Nick Spain <nicholas.spain96@gmail.com>
;; Version: 1.1
;; Keywords: C, Makefile, compilation
;; URL: https://github.com/nick96/basic-c-compile


;;; Commentary:

;; basic-c-compile.el is a basic script for C programming.  It can create a basic
;; Makefile, compile the C program (with or without the Makefile) and run the
;; file.

;; This package assumes that a single C file is being compiled, however, the
;; Makefile is easily edited.

;;;###autoload
(defun makefile ()
  "Create a Makefile of the form shown in README."
  (interactive)
  (create-makefile (buffer-name)))

(defun compile-file ()
  "Compile file with or without a Makefile."
  (interactive)
  (let ((path (file-name-directory (buffer-file-name)))
        (file (buffer-name)))
    
    (if (y-or-n-p "Compile with Makefile? ")
        ;; Check for presence of Makefile to stop creating duplicates
        (dolist (file-name (directory-files path))))
          (if (file-exists-p "Makefile")
              (if (file-exists-p (file-name-sans-extension file))
                  (compile-with-makefile "rebuild")
                (compile-with-makefile "build"))
            (create-makefile file)
            (compile-with-makefile "build"))
      (compile-sans-makefile file)))

(defun run-c ()
  "Run the program."
  (interactive)
  (run-c-file (buffer-name))
  (other-window 1))

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
  "Compile file using the Makefile with specified ARG (build, clean, rebuild)."
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
  (compile
   (format "./%s"
           (file-name-sans-extension file))))

(provide 'basic-c-compile)
;;; basic-c-compile.el ends here
