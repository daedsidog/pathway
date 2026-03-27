;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(uiop:define-package #:pathway/tests
  (:use #:clean)
  (:export #:run-tests))

(in-package #:pathway/tests)

(defun run-tests ()
  (pathway/tests/pathname-utilities-test:run-tests)
  (pathway/tests/filesystem-utilities-test:run-tests)
  (pathway/tests/asdf/virtual-static-file-test:run-tests)
  (pathway/tests/asdf/virtual-static-directory-test:run-tests))
