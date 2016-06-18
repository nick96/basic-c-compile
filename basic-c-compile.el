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
;; Package-Requires: ((cl-lib "0.5"))

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
  (basic-c-compile--create-makefile buffer-file-name))

(basic-c-compile-makefile)
;;;###autoload
(defun basic-c-compile-file ()
  "Compile file with or without a Makefile."
  (interactive)
  ;; Define local scope variables
  (let* ((path (file-name-directory (buffer-file-name)))
         (infile (buffer-file-name))
         (outfile (concat (file-name-sans-extension infile) ".o")))
    ;; Makefile control flow
    (if (y-or-n-p "Compile with Makefile? ")
        ;; Check for presence of Makefile to stop creating duplicates
        (if (member "Makefile" (directory-files path))
              (if (member outfile (directory-files path))
                  (basic-c-compile--with-makefile "rebuild")
                (basic-c-compile--with-makefile "build"))
            (basic-c-compile--create-makefile infile)
            (basic-c-compile--with-makefile "build"))
          (basic-c-compile--sans-makefile infile))))

;;;###autoload
(defun basic-c-compile-run-c ()
  "Run the program."
  (interactive)
  (basic-c-compile--run-c-file (file-name-nondirectory (buffer-file-name))))

;;; Code:

;; Global variables
;; These can be changed by the user in their init file
(defvar basic-c-compile-compiler "gcc") ; Change to whatever compiler you prefer
;; basic-c-compile-all-files can be "all", "selection"
;; Any other value will mean that only the current file is compiled
(defvar basic-c-compile-all-files "all")


;; Function called when user wants to specify a subset of the files to compile
(defun basic-c-compile--choose-files ()
  "Return string of files entered in the mini-buffer."
  (let ((file-list (read-string "Enter file names: ")))
    file-list))


;; Returned file list is based on the option set in basic-c-compile-all-files
;; FIX ME Failing with (wrong-type-argument stringp nil)
(defun basic-c-compile--files-to-compile (file)
  "Return a list of the files to compile which are in the same directory as FILE."
  (cond (;; Make list of all .c files in directory
         (equal basic-c-compile-all-files "all")
         (mapconcat 'identity
                    (mapcar #'shell-quote-argument
                         (cl-remove-if-not #'(lambda (x) (equal (cdr (split-string x "\\."))
                                                                '("c")))
                                           (directory-files (file-name-directory
                                                             file))))
                    " "))
         (;; Call function that allows input of files to be compiled
          (equal basic-c-compile-all-files "selection")
          (basic-c-compile--choose-files))
         (;; Default to only compiling the current file
          t file)))

;; Compile without Makefile
(defun basic-c-compile--sans-makefile (file)
  "Compiles FILE without the need for a Makefile."
        (compile (format "%s -Wall %s -o %s.o"
                         basic-c-compile-compiler
                         (basic-c-compile--files-to-compile file)
                         (shell-quote-argument (file-name-sans-extension file)))))


;; Compile with Makefile
(defun basic-c-compile--with-makefile (arg)
  "Compile file using the Makefile with specified ARG (build, clean, rebuild)."
  (compile (format "make %s"
                   arg)))


;; Create a Makefile

(defun basic-c-compile--create-makefile (file)
  "Create a basic Makefile for FILE, in the same directory."
  (let ((makefile-contents
         (format (concat "CC = %s\n"
                         "INFILE = %s\n"
                         "OUTFILE = %s.o\n\n"
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
  (compile (format "./%s.o"
                   (shell-quote-argument (file-name-sans-extension file)))))

(provide 'basic-c-compile)
;;; basic-c-compile.el ends here
