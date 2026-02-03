;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defsystem #:ck-fs
  :depends-on (#:ck-clle)
  :components ((:module "source"
                :components ((:file "fs")))))

(defsystem #:ck-fs/tests
  :depends-on (#:ck-fs #:fiveam)
  :components ((:module "tests"
                :components ((:file "tests")))))
