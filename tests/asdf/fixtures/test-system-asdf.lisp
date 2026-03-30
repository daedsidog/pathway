(defsystem #:pathway-test-mapped
  :defsystem-depends-on ("pathway/asdf")
  :components
  ((:workspace-extract "test-archive.zip/archived-test-file.txt")
   (:workspace-extract "unarchived-test-file.txt")))

(defsystem #:pathway-test-directory
  :defsystem-depends-on ("pathway/asdf")
  :components
  ((:workspace-extract "test-directory-archive.zip/"
    :workspace-pathname "all/")
   (:workspace-extract "test-directory-archive.zip/test-directory/"
    :workspace-pathname "subtree/")
   (:workspace-extract "test-directory-archive.zip/**/*.h"
    :workspace-pathname "filtered/")))
