;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defsystem #:pathway
  :depends-on (#:clean)
  :components ((:module "source"
                :components ((:file "package")))))

(defsystem #:pathway/asdf
  :depends-on (#:pathway)
  :components ((:module "source"
                :components ((:module "asdf"
                              :components ((:file "package")
                                           (:file "mapped-static-file")))))))

(defsystem #:pathway/tests
  :depends-on (#:pathway #:pathway/asdf #:fiveam #:closer-mop)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "asdf")))))
