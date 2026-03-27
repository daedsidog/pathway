;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defsystem #:pathway
  :depends-on (#:clean)
  :components ((:module "source"
                :components ((:file "pathname-utilities")
                             (:file "filesystem-utilities"
                              :depends-on ("pathname-utilities"))
                             (:file "package"
                              :depends-on ("pathname-utilities"
                                           "filesystem-utilities"))))))

(defsystem #:pathway/asdf
  :depends-on (#:pathway)
  :components ((:module "source"
                :components ((:module "asdf"
                              :components ((:file "virtual-static-file")
                                           (:file "virtual-static-directory"
                                            :depends-on ("virtual-static-file"))
                                           (:file "package"
                                            :depends-on ("virtual-static-file"
                                                         "virtual-static-directory"))))))))

(defsystem #:pathway/tests
  :depends-on (#:pathway #:pathway/asdf #:fiveam #:closer-mop)
  :components ((:module "tests"
                :serial t
                :components ((:file "common")
                             (:file "pathname-utilities-test")
                             (:file "filesystem-utilities-test")
                             (:module "asdf"
                               :components ((:file "virtual-static-file-test")))
                             (:file "package")))))
