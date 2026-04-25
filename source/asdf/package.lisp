(uiop:define-package #:pathway/asdf
  (:use #:clean)
  (:use-reexport #:pathway/asdf/workspace))

(in-package #:pathway/asdf)

(import 'workspace-extract :asdf)
(import 'workspace-amalgam :asdf)
