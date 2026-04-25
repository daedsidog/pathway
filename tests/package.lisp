(uiop:define-package #:pathway/tests
  (:use #:clean)
  (:export #:run-tests))

(in-package #:pathway/tests)

(defun run-tests ()
  (pathway/tests/pathname-utilities-test:run-tests)
  (pathway/tests/filesystem-utilities-test:run-tests)
  (pathway/tests/asdf/workspace-test:run-tests))
