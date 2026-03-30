;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/tests/common
  (:use #:clean)
  (:local-nicknames (#:pw #:pathway))
  (:export #:+sleep-interval+
           #:+fixture-asd+
           #:delete-if-exists
           #:backup-pathname
           #:with-test-virtual-root))

(in-package #:pathway/tests/common)

(defparameter +sleep-interval+ 1.5
  "Delay to ensure FILE-WRITE-DATE granularity in staleness tests")

(defparameter +fixture-asd+
  (asdf:system-relative-pathname :pathway
                                 "tests/asdf/fixtures/test-system-asdf.lisp")
  "Pathname to the shared test fixture system definition")

(defun delete-if-exists (pathname)
  (when (probe-file pathname)
    (delete-file pathname)))

(defun backup-pathname (pathname)
  "Return the backup pathname for PATHNAME."
  (merge-pathnames
    (concatenate 'string
                 #+win32 "" #-win32 "~"
                 (file-namestring pathname)
                 #+win32 ".bak" #-win32 "")
    (uiop:pathname-directory-pathname pathname)))

(defmacro with-test-virtual-root ((&key system-name) &body body)
  "Execute BODY with a temporary virtual root, cleaning up afterward."
  (with-gensyms (temp-dir old-resolver)
    `(let ((,temp-dir (uiop:ensure-directory-pathname
                        (merge-pathnames
                          (make-pathname
                            :directory `(:relative ,(symbol-name (gensym))))
                          (pw:default-tempdir-pathname))))
           (,old-resolver pw:*virtual-root-pathname-resolver*))
       (ensure-directories-exist ,temp-dir)
       (setf pw:*virtual-root-pathname-resolver*
             (lambda () ,temp-dir))
       (asdf::load-asd +fixture-asd+)
       (unwind-protect
         (progn ,@body)
         (setf pw:*virtual-root-pathname-resolver* ,old-resolver)
         ,@(when system-name
             `((asdf:clear-system ,system-name)))
         (ignore-errors
           (uiop:delete-directory-tree ,temp-dir :validate t))))))
