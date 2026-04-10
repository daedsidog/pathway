(defsystem "pathway"
  :depends-on ("clean")
  :components ((:module "source"
                :components ((:file "pathname-utilities")
                             (:file "filesystem-utilities"
                              :depends-on ("pathname-utilities"))
                             (:file "package"
                              :depends-on ("pathname-utilities"
                                           "filesystem-utilities"))))))

(defsystem "pathway/asdf"
  :depends-on ("pathway")
  :components ((:module "source"
                :components ((:module "asdf"
                              :components ((:file "workspace-extract")
                                           (:file "package"
                                            :depends-on ("workspace-extract"))))))))

(defsystem "pathway/tests"
  :depends-on ("pathway" "pathway/asdf" "fiveam" "closer-mop")
  :components ((:module "tests"
                :serial t
                :components ((:file "common")
                             (:file "pathname-utilities-test")
                             (:file "filesystem-utilities-test")
                             (:module "asdf"
                               :components ((:file "workspace-extract-test")))
                             (:file "package")))))
