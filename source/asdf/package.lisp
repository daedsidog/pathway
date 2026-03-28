;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(uiop:define-package #:pathway/asdf
  (:use #:clean)
  (:use-reexport #:pathway/asdf/virtual-static-file
                 #:pathway/asdf/virtual-static-directory))

(in-package #:pathway/asdf)

(import 'virtual-static-file :asdf)
(import 'virtual-static-directory :asdf)
