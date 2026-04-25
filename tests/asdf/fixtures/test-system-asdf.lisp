(defsystem #:pathway-test-mapped
  :defsystem-depends-on ("pathway/asdf")
  :components
  ((:workspace-extract "test-archive.zip/archived-test-file.txt")
   (:workspace-extract "unarchived-test-file.txt")
   (:workspace-extract "subdir/nested-test-file.txt")))

(defsystem #:pathway-test-directory
  :defsystem-depends-on ("pathway/asdf")
  :components
  ((:workspace-extract "test-directory-archive.zip/"
    :workspace-pathname "all/")
   (:workspace-extract "test-directory-archive.zip/test-directory/"
    :workspace-pathname "subtree/")
   (:workspace-extract "test-directory-archive.zip/**/*.h"
    :workspace-pathname "filtered/")))

(defsystem #:pathway-test-amalgam
  :defsystem-depends-on ("pathway/asdf")
  :components
  ((:workspace-amalgam "amalgam-test"
    :workspace-pathname "amalgam-test.txt"
    :parts ("amalgam-test.part1" "amalgam-test.part2"))))

(defsystem #:pathway-test-amalgam-glob
  :defsystem-depends-on ("pathway/asdf")
  :components
  ((:workspace-amalgam "amalgam-glob-test"
    :workspace-pathname "amalgam-glob-test.txt"
    :parts "amalgam-test.part*")))
