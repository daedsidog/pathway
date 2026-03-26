;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/asdf
  (:use #:clean)
  (:local-nicknames (#:pw #:pathway))
  (:export #:extract-op
           #:mapped-static-file
           #:target-pathname))

(in-package #:pathway/asdf)
