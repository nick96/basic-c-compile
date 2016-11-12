(require 'f)

(defvar basic-c-compile-support-path
  (f-dirname load-file-name))

(defvar basic-c-compile-features-path
  (f-parent basic-c-compile-support-path))

(defvar basic-c-compile-root-path
  (f-parent basic-c-compile-features-path))

(add-to-list 'load-path basic-c-compile-root-path)

(require 'basic-c-compile)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
