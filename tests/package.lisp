;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/tests
  (:use #:clean #:fiveam #:pathway)
  (:export #:run-tests))

(in-package #:pathway/tests)

(def-suite* pathway-test)

(defun run-tests ()
  (run! 'pathway-test))
