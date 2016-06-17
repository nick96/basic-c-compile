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
;; Version: 1.1.1
;; Keywords: C, Makefile, compilation
;; URL: https://github.com/nick96/basic-c-compile
;; Package-Requires: ((cl-lib))

;;; Commentary:

;; basic-c-compile.el is a basic script for C programming.  It can create a basic
;; Makefile, compile the C program (with or without the Makefile) and run the
;; file.

;; This package assumes that a single C file is being compiled, however, the
;; Makefile is easily edited.
(require 'cl-lib)
;;;###autoload
(defun basic-c-compile-makefile ()
  "Create a Makefile of the form shown in README."
  (interactive)
  (basic-c-compile--create-makefile (buffer-file-name)))

;;;###autoload
(defun basic-c-compile-file ()
  "Compile file with or without a Makefile."
  (interactive)
  ;; Define local scope variables
  (let ((path (file-name-directory (buffer-file-name)))
        (file (buffer-file-name)))
    ;; Makefile control flow
    (if (y-or-n-p "Compile with Makefile? ")
        ;; Check for presence of Makefile to stop creating duplicates
        (dolist (file-name (directory-files path))))
          (if (file-exists-p "Makefile")
              (if (file-exists-p (file-name-sans-extension file))
                  (basic-c-compile--with-makefile "rebuild")
                (basic-c-compile--with-makefile "build"))
            (basic-c-compile--create-makefile file)
            (basic-c-compile--with-makefile "build"))
          (basic-c-compile--sans-makefile file)))

;;;###autoload
(defun basic-c-compile-run-c ()
  "Run the program."
  (interactive)
  (basic-c-compile--run-c-file (file-name-nondirectory (buffer-file-name))))

;;; Code:

;; Global variables
;; These can be changed by the user in their init file
(defvar basic-c-compile-compiler "gcc") ; Change to whatever compiler you prefer
;; basic-c-compile-all-files can be "all" (or t), "selection" or nil
(defvar basic-c-compile-all-files t)


;; Function called when user wants to specify a subset of the files to compile
(defun basic-c-compile--choose-files ()
  "Return string of files entered in the minibuffer."
  (read-string "Enter list of files: "))

;; Returned file list is based on the option set in basic-c-compile-all-files
(defun basic-c-compile--files-to-compile (file)
  "Return a list of the files to compile which are in the same directory as FILE."
  (cond (basic-c-compile-all-files
         (mapconcat 'identity
                    (map #'shell-quote-argument
                         (cl-remove-if-not #'(lambda (x) (string-match "*.c" x))
                                           (directory-files (file-name-directory file))))
                    (shell-quote-argument file)))
        ((equal basic-c-compile-all-files "selection")
         (basic-c-compile--choose-files))
        (t file)))

;; Compile without Makefile
(defun basic-c-compile--sans-makefile (file)
  "Compiles FILE without the need for a Makefile."
        (compile (format "%S -Wall %S -o %S.o"
                         basic-c-compile-compiler
                         (basic-c-compile--files-to-compile file)
                         (shell-quote-argument (file-name-sans-extension file)))))


;; Compile with Makefile
(defun basic-c-compile--with-makefile (arg)
  "Compile file using the Makefile with specified ARG (build, clean, rebuild)."
  (compile (format "make %S"
                   arg)))


;; Create a Makefile

(defun basic-c-compile--create-makefile (file)
  "Create a basic Makefile for FILE, in the same directory."
  (let ((makefile-contents
         (format (concat "CC=%S\n"
                         "INFILE=%S\n"
                         "OUTFILE=%S.o\n\n"
                         "build: $(INFILE)\n\t"
                         "$(CC) -Wall $(INFILE)  -o $(OUTFILE)\n\n"
                         "clean:\n\t rm -f *.o \n\n"
                         "rebuild: clean build")
                 basic-c-compile-compiler
                 (basic-c-compile--files-to-compile file)
                 (shell-quote-argument (file-name-nondirectory (file-name-sans-extension file))))))
  (write-region makefile-contents
                nil
                "Makefile")))


;; Run file
(defun basic-c-compile--run-c-file (file)
  "Run FILE with the output printing in a temporary buffer."
  (compile (format "./%S.o"
                   (shell-quote-argument (file-name-sans-extension file)))))

(provide 'basic-c-compile)
;;; basic-c-compile.el ends here
