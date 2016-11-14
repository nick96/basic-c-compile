;;; test-helper --- Test helper for basic-c-compile

;;; Commentary:
;; test helper inspired from
;; https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar basic-c-compile-test-path
  (f-dirname (f-this-file)))

(defvar basic-c-compile-root-path
  (f-parent basic-c-compile-test-path))

(defvar basic-c-compile-sandbox-path
  (f-expand "sandbox" basic-c-compile-test-path))

(when (f-exists? basic-c-compile-sandbox-path)
  (error "Something is already in %s. Check and destroy it
  yourself" basic-c-compile-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory basic-c-compile-sandbox-path))
     (when (f-exists? basic-c-compile-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir basic-c-compile-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl))
(require 'basic-c-compile)

(provide 'test-helper)
;;; test-helper.el ends here
