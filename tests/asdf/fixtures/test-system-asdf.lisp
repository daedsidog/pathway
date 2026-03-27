;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defsystem #:pathway-test-mapped
  :defsystem-depends-on ("pathway/asdf")
  :components
  ((:virtual-static-file "test-archive.zip/archived-test-file.txt"
    :virtual-pathname "archived-test-file.txt")
   (:virtual-static-file "unarchived-test-file.txt"
    :virtual-pathname "unarchived-test-file.txt")))
