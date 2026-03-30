;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(uiop:define-package #:pathway/asdf
  (:use #:clean)
  (:use-reexport #:pathway/asdf/workspace-extract))

(in-package #:pathway/asdf)

(import 'workspace-extract :asdf)
