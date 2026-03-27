;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/tests/asdf/virtual-static-directory-test
  (:use #:clean #:fiveam #:pathway #:pathway/asdf)
  (:import-from #:pathway/tests/common
                #:+sleep-interval+
                #:with-test-virtual-root)
  (:export #:run-tests))

(in-package #:pathway/tests/asdf/virtual-static-directory-test)

(def-suite* virtual-static-directory-test)

(defparameter +test-system-name+ "pathway-test-directory")
(defparameter +subdirectory-name+ "test-directory")
(defparameter +library-file+ "library.lib")
(defparameter +header-file+ "include/header.h")
(defparameter +readme-file+ "readme.txt")
(defparameter +all-virtual-pathname+ "all/")
(defparameter +subtree-virtual-pathname+ "subtree/")
(defparameter +filtered-virtual-pathname+ "filtered/")

(defun output-directory (virtual-pathname)
  "Return the test output directory for VIRTUAL-PATHNAME."
  (merge-pathnames
    (pathname virtual-pathname)
    (merge-pathnames
      (make-pathname :directory `(:relative ,+test-system-name+))
      (virtual-root-pathname))))

(defun subdir-file (filename)
  "Return FILENAME prefixed with the test subdirectory."
  (format nil "~A/~A" +subdirectory-name+ filename))

(test class-exists
  "Verify that VIRTUAL-STATIC-DIRECTORY is registered."
  (is (find-class 'virtual-static-directory nil))
  (is (subtypep 'virtual-static-directory 'asdf:static-file)))

(test extract-all-from-archive
  "Verify extraction of all files from an archive."
  (with-test-virtual-root (:system-name +test-system-name+)
    (asdf:load-system +test-system-name+)
    (let ((out (output-directory +all-virtual-pathname+)))
      (is (probe-file (merge-pathnames (subdir-file +library-file+) out)))
      (is (probe-file (merge-pathnames (subdir-file +header-file+) out)))
      (is (probe-file (merge-pathnames (subdir-file +readme-file+) out))))))

(test extract-subtree-from-archive
  "Verify extraction of a subtree with prefix stripping."
  (with-test-virtual-root (:system-name +test-system-name+)
    (asdf:load-system +test-system-name+)
    (let ((out (output-directory +subtree-virtual-pathname+)))
      (is (probe-file (merge-pathnames +library-file+ out)))
      (is (probe-file (merge-pathnames +header-file+ out))))))

(test extract-filtered-from-archive
  "Verify extraction with a wildcard filter."
  (with-test-virtual-root (:system-name +test-system-name+)
    (asdf:load-system +test-system-name+)
    (let ((out (output-directory "filtered/")))
      (is (probe-file
            (merge-pathnames (subdir-file +header-file+) out)))
      (is-false (probe-file
                  (merge-pathnames (subdir-file +library-file+) out)))
      (is-false (probe-file
                  (merge-pathnames (subdir-file +readme-file+) out))))))

(test virtual-pathname-map-populated
  "Verify that VIRTUAL-PATHNAME-MAP has a directory entry after loading."
  (with-test-virtual-root (:system-name +test-system-name+)
    (asdf:load-system +test-system-name+)
    (let ((found nil))
      (virtual-pathname-map
        (lambda (key value)
          (declare (ignore key))
          (when (uiop:directory-pathname-p value)
            (setf found t))))
      (is-true found))))

(test extraction-is-idempotent
  "Verify that a second load does not re-extract when the archive is unchanged."
  (with-test-virtual-root (:system-name +test-system-name+)
    (asdf:load-system +test-system-name+)
    (let* ((out (output-directory +all-virtual-pathname+))
           (file (merge-pathnames (subdir-file +library-file+) out))
           (mtime (file-write-date file)))
      (sleep +sleep-interval+)
      (asdf:load-system +test-system-name+)
      (is (= mtime (file-write-date file))))))

(defun run-tests ()
  (run! 'virtual-static-directory-test))
