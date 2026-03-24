;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defsystem #:ck-fs
  :depends-on (#:clean)
  :components ((:module "source"
                :components ((:file "package")))))

(defsystem #:ck-fs/tests
  :depends-on (#:ck-fs #:fiveam)
  :components ((:module "tests"
                :components ((:file "package")))))
