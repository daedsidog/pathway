;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defsystem #:pathway
  :depends-on (#:clean)
  :components ((:module "source"
                :components ((:file "package")))))

(defsystem #:pathway/tests
  :depends-on (#:pathway #:fiveam)
  :components ((:module "tests"
                :components ((:file "package")))))
