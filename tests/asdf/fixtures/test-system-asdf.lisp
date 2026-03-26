;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defsystem #:pathway-test-mapped
  :defsystem-depends-on ("pathway/asdf")
  :components
  ((:mapped-static-file "test-archive.zip/test.txt"
    :target-pathname "test.txt")))
