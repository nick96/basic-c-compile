;;; basic-c-compile-test.el --- Testing of basic-c-compile.

;;; Commentary:
;; Tests for package basic-c-compile.
;;; Code:

(require 'basic-c-compile)
(require 'ert)
(require 'test-helper)
(require 'f)
(require 's)

(setq c-hello-world (s-join "\n" '("#include <stdio.h>"
				     "int main (int argc, char *argv[]) {"
				     "    printf(\"Hello, World\");"
				     "}")))

(setq makefile-str-single
      (s-join "\n" '("CC = gcc"
		     "INFILE = test.c"
		     "OUTFILE = test.o"
		     "FLAGS = -Wall"
		     ""
		     "build: $(INFILE)"
		     "	$(CC) $(FLAGS) $(INFILE) -o $(OUTFILE)"
		     ""
		     "clean:"
		     "	rm *.o"
		     ""
		     "rebuild: clean build")))

(setq makefile-str-all (s-join "\n" '("CC = gcc"
				      "INFILE = %s"
				      "OUTFILE = foo.o"
				      "FLAGS = -Wall"
				      ""
				      "build: $(INFILE)"
				      "	$(CC) $(FLAGS) $(INFILE) -o $(OUTFILE)"
				      ""
				      "clean:"
				      "	rm *.o"
				      ""
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


(ert-deftest test-basic-c-compile/sans-makefile ()
  "Test ``basic-c-compile--sans-makefile''."
  (within-sandbox
   (f-write c-hello-world 'utf-8 "test.c")
   (f-write c-hello-world 'utf-8 "test with spaces.c")
   (should (basic-c-compile--sans-makefile "gcc" "" "test.c" "test.c" "o"))
   (kill-compilation)
   (should (basic-c-compile--sans-makefile "gcc" "" "test.c"
					   "test with spaces.c" "o")))
  (kill-compilation))

(ert-deftest test-basic-c-compile/with-makefile ()
  "Test ``basic-c-compile--with-makefile''."
  (within-sandbox
   (f-write c-hello-world 'utf-8 "test.c")
   (f-write c-hello-world 'utf-8 "test with spaces.c")
   (basic-c-compile--create-makefile "gcc"
				     "test.c"
				     "test.c"
				     "o"
				     "-Wall"
				     "rm *.o"
				     "Makefile")
   (should (basic-c-compile--with-makefile ""))
   (kill-compilation)
   (basic-c-compile--create-makefile "gcc"
				     "test with spaces.c"
				     "test with spaces.c"
				     "o"
				     "-Wall"
				     "rm *.o"
				     "Makefile")
   (should (basic-c-compile--with-makefile ""))
   (kill-compilation)))


(ert-deftest test-basic-c-compile/create-makefile ()
  "Test ``basic-c-compile--create-makefile''."
  (let ((files "foo.c foo2.c foo3.c foo4.c"))
  (within-sandbox
   ;; Test that Makefile creation works for explicit file declartion.
   (basic-c-compile--create-makefile "gcc"
				     "test.c"
				     "test.c"
				     "o"
				     "-Wall"
				     "rm *.o"
				     "Makefile")
   (should (equal (f-read "Makefile") makefile-str-single))
   (mapc (lambda (file) (f-write "foo" 'utf-8 file)) (s-split " " files))
   ;; Test that Makefile creation works for a given string of files
   (basic-c-compile--create-makefile "gcc"
				     (s-split " " files)
				     "foo.c"
				     "o"
				     "-Wall"
				     "rm *.o"
				     "Makefile")
   (should (equal (f-read "Makefile") (format makefile-str-all files))))))

;; (ert-run-tests-interactively t)
(provide 'basic-c-compile-test)

;;; basic-c-compile-test.el ends here
