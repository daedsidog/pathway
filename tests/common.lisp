;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/tests/common
  (:use #:clean))

(in-package #:pathway/tests/common)

(defparameter +sleep-interval+ 1.5
  "Delay to ensure FILE-WRITE-DATE granularity in staleness tests")
