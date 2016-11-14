;;; basic-c-compile-test.el --- Testing of basic-c-compile.

;;; Commentary:
;; Tests for package basic-c-compile.
;;; Code:

(require 'basic-c-compile)
(require 'ert)
(require 'test-helper)
(require 'f)
(require 's)

(defvar c-hello-world (s-join "\n" '("#include <stdio.h>"
				     "int main (int argc, char *argv[]) {"
				     "    printf(\"Hello, World\\n\");"
				     "}\\n")))

(defvar makefile (s-join "\n" '("CC = gcc"
				"INFILE = test.c"
				"OUTFILE = test.o"
				"FLAGS = -Wall"
				"build: $(INFILE)"
				"\t$(CC) $(FLAGS) $(INFILE) -o $(OUTFILE)\n"
				"clean:"
				"\trm -rf *.o\n"
				"rebuild: clean build")))

(ert-deftest test-basic-c-compile/c-file-extension-p ()
  "Test ``basic-c-compile--c-file-extension-p''."
  (should (basic-c-compile--c-file-extension-p "test.c"))
  (should (basic-c-compile--c-file-extension-p "test.c.c"))
  (should (basic-c-compile--c-file-extension-p "test.x.c..n.y.xyz.c"))
  (should (basic-c-compile--c-file-extension-p "test with space.c"))
  (should-not (basic-c-compile--c-file-extension-p "test.o"))
  (should-not (basic-c-compile--c-file-extension-p "test.h"))
  (should-not (basic-c-compile--c-file-extension-p "test.x.y.dsf..dfs...d"))
  (should-not (basic-c-compile--c-file-extension-p "test with spaces.x")))


;; Fails in sandbox -- Do I need to create my own?
;; Test with spaced file names
(ert-deftest test-basic-c-compile/sans-makefile ()
  "Test ``basic-c-compile--sans-makefile''."
  (within-sandbox (f-write c-hello-world 'utf-8 "test.c")
		  (basic-c-compile--sans-makefile "gcc" "" nil "test.c" "o")))

(ert-deftest test-basic-c-compile/with-makefile ()
  "Test ``basic-c-compile--with-makefile''.")


(ert-deftest test-basic-c-compile/create-makefile ()
  "Test ``basic-c-compile--create-makefile''."
  (within-sandbox (basic-c-compile--create-makefile "gcc"
						    nil
						    "test.c"
						    "o"
						    ""
						    "rm *.o"
						    "Makefile")
		  (should (equal (f-read "Makefile") makefile)))

(ert-deftest test-basic-c-compile/run-c-file ()
  "Test ``basic-c-compile--run-c-file''.")

(provide 'basic-c-compile-test)

;;; basic-c-compile-test.el ends here
