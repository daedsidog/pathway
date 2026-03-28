;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/tests/asdf/virtual-static-file-test
  (:use #:clean #:fiveam #:pathway #:pathway/asdf)
  (:import-from #:pathway/tests/common
                #:+sleep-interval+
                #:+fixture-asd+
                #:delete-if-exists
                #:backup-pathname
                #:with-test-virtual-root)
  (:export #:run-tests))

(in-package #:pathway/tests/asdf/virtual-static-file-test)

(def-suite* virtual-static-file-test)

(defparameter +test-system-name+ "pathway-test-mapped")
(defparameter +test-component-name+ "test-archive.zip/archived-test-file.txt")
(defparameter +expected-content+ "archived-test-file.txt")
(defparameter +archive-input-search+ "test-archive.zip")

(defparameter +plain-component-name+ "unarchived-test-file.txt")
(defparameter +plain-expected-content+ +plain-component-name+)

(defun extract-op ()
  (asdf:make-operation 'pathway/asdf:extract-op))

(defun load-fixture-system ()
  (asdf::load-asd +fixture-asd+)
  (asdf:find-system +test-system-name+))

(defun fixture-component (&optional (name +test-component-name+))
  (asdf:find-component (load-fixture-system) name))

(defun fixture-output-path (&optional (name +test-component-name+))
  (let ((component (fixture-component name)))
    (first (asdf:output-files (extract-op) component))))

(defmacro define-output-files-test (test-name component-name output-search)
  "Generate a test verifying OUTPUT-FILES returns a path containing OUTPUT-SEARCH."
  `(test ,test-name
     ,(format nil "Verify OUTPUT-FILES for ~A returns the expected cache path." test-name)
     (with-test-virtual-root (:system-name +test-system-name+)
       (let* ((component (fixture-component ,component-name))
              (outputs (asdf:output-files (extract-op) component)))
         (is (= 1 (length outputs)))
         (is (search ,output-search (namestring (first outputs))))))))

(defmacro define-input-files-test (test-name component-name input-search)
  "Generate a test verifying INPUT-FILES returns a path containing INPUT-SEARCH."
  `(test ,test-name
     ,(format nil "Verify INPUT-FILES for ~A returns the expected source path." test-name)
     (with-test-virtual-root (:system-name +test-system-name+)
       (let* ((component (fixture-component ,component-name))
              (inputs (asdf:input-files (extract-op) component)))
         (is (= 1 (length inputs)))
         (is (search ,input-search (namestring (first inputs))))
         (is (probe-file (first inputs)))))))

(defmacro define-perform-test (test-name component-name expected-content)
  "Generate a test verifying EXTRACT-OP produces the expected content."
  `(test ,test-name
     ,(format nil "Verify EXTRACT-OP produces correct content for ~A." test-name)
     (with-test-virtual-root (:system-name +test-system-name+)
       (let* ((component (fixture-component ,component-name))
              (output (first (asdf:output-files (extract-op) component))))
         (delete-if-exists output)
         (asdf:perform (extract-op) component)
         (is (probe-file output))
         (is (string= ,expected-content (uiop:read-file-string output)))))))

(defmacro define-staleness-test (test-name component-name)
  "Generate a test verifying ASDF skips extraction when output is newer."
  `(test ,test-name
     ,(format nil "Verify ASDF skips extraction for ~A when output is newer." test-name)
     (with-test-virtual-root (:system-name +test-system-name+)
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
  "Generate the standard EXTRACT-OP test suite for a component."
  (flet ((prefixed (suffix) (intern (format nil "~A-~A" name suffix))))
    `(progn
       (define-output-files-test ,(prefixed "OUTPUT-FILES") ,component-name ,expected-content)
       (define-input-files-test ,(prefixed "INPUT-FILES") ,component-name ,input-search)
       (define-perform-test ,(prefixed "PERFORM") ,component-name ,expected-content)
       (define-staleness-test ,(prefixed "STALENESS") ,component-name))))

(test virtual-static-file-class-exists
  "Verify that the VIRTUAL-STATIC-FILE component class is registered."
  (is (find-class 'pathway/asdf:virtual-static-file nil))
  (is (subtypep 'pathway/asdf:virtual-static-file 'asdf:static-file)))

(test extract-op-class-exists
  "Verify that EXTRACT-OP is a non-propagating ASDF operation."
  (is (find-class 'pathway/asdf:extract-op nil))
  (is (subtypep 'pathway/asdf:extract-op 'asdf:non-propagating-operation)))

(test virtual-static-file-asdf-import
  "Verify that VIRTUAL-STATIC-FILE is importable from the ASDF package."
  (is (eqlp (find-class 'asdf::virtual-static-file)
            (find-class 'pathway/asdf:virtual-static-file))))

(define-extract-op-tests archive
  +test-component-name+ +expected-content+ +archive-input-search+)

(define-extract-op-tests plain
  +plain-component-name+ +plain-expected-content+ +plain-component-name+)

(test missing-archive-signals-error
  "Verify that EXTRACT-OP signals an error when the archive does not exist."
  (with-test-virtual-root (:system-name +test-system-name+)
    (let* ((component (fixture-component))
           (op (extract-op))
           (archive (first (asdf:input-files op component)))
           (backup (backup-pathname archive)))
      (rename-file archive backup)
      (unwind-protect
        (signals error (asdf:perform op component))
        (rename-file backup archive)))))

(defun run-tests ()
  (run! 'virtual-static-file-test))
