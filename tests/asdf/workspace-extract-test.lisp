(defpackage #:pathway/tests/asdf/workspace-extract-test
  (:use #:clean #:fiveam #:pathway #:pathway/asdf)
  (:import-from #:pathway/tests/common
                #:+sleep-interval+
                #:+fixture-asd+
                #:delete-if-exists
                #:backup-pathname
                #:with-test-workspace)
  (:export #:run-tests))

(in-package #:pathway/tests/asdf/workspace-extract-test)

(def-suite* workspace-extract-test)

(defparameter +test-system-name+ "pathway-test-mapped")
(defparameter +test-component-name+ "test-archive.zip/archived-test-file.txt")
(defparameter +expected-content+ "archived-test-file.txt")
(defparameter +archive-input-search+ "test-archive.zip")
(defparameter +plain-component-name+ "unarchived-test-file.txt")
(defparameter +plain-expected-content+ +plain-component-name+)
(defparameter +nested-component-name+ "subdir/nested-test-file.txt")
(defparameter +nested-expected-content+ "nested-test-file.txt")

(defun extract-op ()
  (asdf:make-operation 'pathway/asdf:extract-op))

(defun load-fixture-system ()
  (asdf::load-asd +fixture-asd+)
  (asdf:find-system +test-system-name+))

(defun fixture-component (&optional (name +test-component-name+))
  (asdf:find-component (load-fixture-system) name))

(defmacro define-output-files-test (test-name component-name output-search)
  `(test ,test-name
     ,(format nil "Verify OUTPUT-FILES for ~A returns the expected workspace path." test-name)
     (with-test-workspace (:system-name +test-system-name+)
       (let* ((component (fixture-component ,component-name))
              (outputs (asdf:output-files (extract-op) component)))
         (is (= 1 (length outputs)))
         (is (search ,output-search (namestring (first outputs))))))))

(defmacro define-input-files-test (test-name component-name input-search)
  `(test ,test-name
     ,(format nil "Verify INPUT-FILES for ~A returns the expected source path." test-name)
     (with-test-workspace (:system-name +test-system-name+)
       (let* ((component (fixture-component ,component-name))
              (inputs (asdf:input-files (extract-op) component)))
         (is (= 1 (length inputs)))
         (is (search ,input-search (namestring (first inputs))))
         (is (probe-file (first inputs)))))))

(defmacro define-perform-test (test-name component-name expected-content)
  `(test ,test-name
     ,(format nil "Verify EXTRACT-OP produces correct content for ~A." test-name)
     (with-test-workspace (:system-name +test-system-name+)
       (let* ((component (fixture-component ,component-name))
              (output (first (asdf:output-files (extract-op) component))))
         (delete-if-exists output)
         (asdf:perform (extract-op) component)
         (is (probe-file output))
         (is (string= ,expected-content (uiop:read-file-string output)))))))

(defmacro define-staleness-test (test-name component-name)
  `(test ,test-name
     ,(format nil "Verify ASDF skips extraction for ~A when output is newer." test-name)
     (with-test-workspace (:system-name +test-system-name+)
       (let* ((component (fixture-component ,component-name))
              (output (first (asdf:output-files (extract-op) component))))
         (delete-if-exists output)
         (asdf:operate 'pathway/asdf:extract-op component)
         (is (probe-file output))
         (sleep +sleep-interval+)
         (with-open-file (s output :direction :output :if-exists :append)
           s)
         (let ((touched-write-date (file-write-date output)))
           (asdf:operate 'pathway/asdf:extract-op component)
           (is (= touched-write-date (file-write-date output))))))))

(defmacro define-extract-op-tests (name component-name expected-content
                                    input-search)
  (flet ((prefixed (suffix) (intern (format nil "~A-~A" name suffix))))
    `(progn
       (define-output-files-test ,(prefixed "OUTPUT-FILES") ,component-name ,expected-content)
       (define-input-files-test ,(prefixed "INPUT-FILES") ,component-name ,input-search)
       (define-perform-test ,(prefixed "PERFORM") ,component-name ,expected-content)
       (define-staleness-test ,(prefixed "STALENESS") ,component-name))))

(test workspace-extract-class-exists
  "Verify that the WORKSPACE-EXTRACT component class is registered."
  (is (find-class 'pathway/asdf:workspace-extract nil))
  (is (subtypep 'pathway/asdf:workspace-extract 'asdf:file-component)))

(test extract-op-class-exists
  "Verify that EXTRACT-OP is a non-propagating ASDF operation."
  (is (find-class 'pathway/asdf:extract-op nil))
  (is (subtypep 'pathway/asdf:extract-op 'asdf:non-propagating-operation)))

(test workspace-extract-asdf-import
  "Verify that WORKSPACE-EXTRACT is importable from the ASDF package."
  (is (eqlp (find-class 'asdf::workspace-extract)
            (find-class 'pathway/asdf:workspace-extract))))

(define-extract-op-tests archive
  +test-component-name+ +expected-content+ +archive-input-search+)

(define-extract-op-tests plain
  +plain-component-name+ +plain-expected-content+ +plain-component-name+)

(define-extract-op-tests nested
  +nested-component-name+ +nested-expected-content+ +nested-component-name+)

(test missing-archive-signals-error
  "Verify that EXTRACT-OP signals an error when the archive does not exist."
  (with-test-workspace (:system-name +test-system-name+)
    (let* ((component (fixture-component))
           (op (extract-op))
           (archive (first (asdf:input-files op component)))
           (backup (backup-pathname archive)))
      (rename-file archive backup)
      (unwind-protect
        (signals error (asdf:perform op component))
        (rename-file backup archive)))))

(test workspace-pathname-defaults
  "Verify that WORKSPACE-PATHNAME defaults correctly for different component types."
  (with-test-workspace (:system-name +test-system-name+)
    (let ((archived (fixture-component +test-component-name+))
          (plain (fixture-component +plain-component-name+))
          (nested (fixture-component +nested-component-name+)))
      (is (string= "archived-test-file.txt"
                    (pathway/asdf:workspace-pathname archived)))
      (is (string= "unarchived-test-file.txt"
                    (pathway/asdf:workspace-pathname plain)))
      (is (string= "nested-test-file.txt"
                    (pathway/asdf:workspace-pathname nested))))))

(defparameter +dir-system-name+ "pathway-test-directory")
(defparameter +subdirectory-name+ "test-directory")
(defparameter +library-file+ "library.lib")
(defparameter +header-file+ "include/header.h")
(defparameter +readme-file+ "readme.txt")
(defparameter +all-workspace-pathname+ "all/")
(defparameter +subtree-workspace-pathname+ "subtree/")
(defparameter +filtered-workspace-pathname+ "filtered/")

(defun workspace-output-directory (workspace-pathname)
  "Return the test output directory for WORKSPACE-PATHNAME."
  (merge-pathnames
    (pathname workspace-pathname)
    (default-workspace-pathname)))

(defun subdir-file (filename)
  (format nil "~A/~A" +subdirectory-name+ filename))

(test glob-class-exists
  "Verify that WORKSPACE-EXTRACT handles glob components."
  (is (find-class 'workspace-extract nil))
  (is (subtypep 'workspace-extract 'asdf:file-component)))

(test extract-all-from-archive
  "Verify extraction of all files from an archive."
  (with-test-workspace (:system-name +dir-system-name+)
    (asdf:load-system +dir-system-name+)
    (let ((out (workspace-output-directory +all-workspace-pathname+)))
      (is (probe-file (merge-pathnames (subdir-file +library-file+) out)))
      (is (probe-file (merge-pathnames (subdir-file +header-file+) out)))
      (is (probe-file (merge-pathnames (subdir-file +readme-file+) out))))))

(test extract-subtree-from-archive
  "Verify extraction of a subtree with prefix stripping."
  (with-test-workspace (:system-name +dir-system-name+)
    (asdf:load-system +dir-system-name+)
    (let ((out (workspace-output-directory +subtree-workspace-pathname+)))
      (is (probe-file (merge-pathnames +library-file+ out)))
      (is (probe-file (merge-pathnames +header-file+ out))))))

(test extract-filtered-from-archive
  "Verify extraction with a wildcard filter."
  (with-test-workspace (:system-name +dir-system-name+)
    (asdf:load-system +dir-system-name+)
    (let ((out (workspace-output-directory +filtered-workspace-pathname+)))
      (is (probe-file
            (merge-pathnames (subdir-file +header-file+) out)))
      (is-false (probe-file
                  (merge-pathnames (subdir-file +library-file+) out)))
      (is-false (probe-file
                  (merge-pathnames (subdir-file +readme-file+) out))))))

(test extraction-is-idempotent
  "Verify that a second load does not re-extract when the archive is unchanged."
  (with-test-workspace (:system-name +dir-system-name+)
    (asdf:load-system +dir-system-name+)
    (let* ((out (workspace-output-directory +all-workspace-pathname+))
           (file (merge-pathnames (subdir-file +library-file+) out))
           (mtime (file-write-date file)))
      (sleep +sleep-interval+)
      (asdf:load-system +dir-system-name+)
      (is (= mtime (file-write-date file))))))

(defun run-tests ()
  (run! 'workspace-extract-test))
