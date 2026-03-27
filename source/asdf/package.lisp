;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/asdf
  (:use #:clean)
  (:local-nicknames (#:pw #:pathway))
  (:export #:extract-op
           #:virtual-static-file
           #:virtual-pathname))

(in-package #:pathway/asdf)
