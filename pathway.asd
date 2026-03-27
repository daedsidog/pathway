;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defsystem #:pathway
  :depends-on (#:clean)
  :components ((:module "source"
                :components ((:file "package")
                             (:file "pathname-utilities")
                             (:file "filesystem-utilities")))))

(defsystem #:pathway/asdf
  :depends-on (#:pathway)
  :components ((:module "source"
                :components ((:module "asdf"
                              :components ((:file "package")
                                           (:file "virtual-static-file")))))))

(defsystem #:pathway/tests
  :depends-on (#:pathway #:pathway/asdf #:fiveam #:closer-mop)
  :components ((:module "tests"
                :serial t
                :components ((:file "common")
                             (:file "package")
                             (:file "pathname-utilities-test")
                             (:file "filesystem-utilities-test")
                             (:module "asdf"
                               :components ((:file "virtual-static-file-test")))))))
