;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(in-package #:pathway/tests)

(def-suite* asdf-test)

(defparameter +fixture-asd+
  (merge-pathnames "tests/asdf/fixtures/test-system-asdf.lisp"
                   (asdf:system-source-directory (asdf:find-system "pathway")))
  "Pathname to the test fixture system definition")

(defparameter +test-system-name+ "pathway-test-mapped")
(defparameter +test-component-name+ "test-archive.zip/test.txt")
(defparameter +expected-content+ "Hello from test archive")

(defun load-fixture-system ()
  (asdf::load-asd +fixture-asd+)
  (asdf:find-system +test-system-name+))

(defun fixture-component ()
  (asdf:find-component (load-fixture-system) +test-component-name+))

(defun fixture-output-path ()
  (let ((component (fixture-component)))
    (first (asdf:output-files (asdf:make-operation 'pathway/asdf:extract-op) component))))

(defun cleanup-fixture ()
  "Clear the test system and remove any extracted files."
  (let ((output (fixture-output-path)))
    (when (probe-file output)
      (delete-file output)))
  (asdf:clear-system +test-system-name+))

;;; Smoke tests

(test mapped-static-file-class-exists
  "Verify that the MAPPED-STATIC-FILE component class is registered."
  (is (find-class 'pathway/asdf:mapped-static-file nil))
  (is (subtypep 'pathway/asdf:mapped-static-file 'asdf:static-file)))

(test extract-op-class-exists
  "Verify that EXTRACT-OP is a non-propagating ASDF operation."
  (is (find-class 'pathway/asdf:extract-op nil))
  (is (subtypep 'pathway/asdf:extract-op 'asdf:non-propagating-operation)))

(test mapped-static-file-asdf-import
  "Verify that MAPPED-STATIC-FILE is importable from the ASDF package."
  (is (eq (find-class 'asdf::mapped-static-file)
          (find-class 'pathway/asdf:mapped-static-file))))

;;; Integration tests

(test output-files-returns-cache-path
  "Verify that OUTPUT-FILES returns a path under the cache directory containing the target file."
  (unwind-protect
       (let* ((component (fixture-component))
              (op (asdf:make-operation 'pathway/asdf:extract-op))
              (outputs (asdf:output-files op component)))
         (is (= 1 (length outputs)))
         (is (search "test.txt" (namestring (first outputs)))))
    (cleanup-fixture)))

(test input-files-returns-archive-path
  "Verify that INPUT-FILES returns the archive path on disk."
  (unwind-protect
       (let* ((component (fixture-component))
              (op (asdf:make-operation 'pathway/asdf:extract-op))
              (inputs (asdf:input-files op component)))
         (is (= 1 (length inputs)))
         (is (search "test-archive.zip" (namestring (first inputs))))
         (is (probe-file (first inputs))))
    (cleanup-fixture)))

(test extract-op-perform
  "Verify that EXTRACT-OP extracts the file with correct content."
  (unwind-protect
       (let* ((component (fixture-component))
              (op (asdf:make-operation 'pathway/asdf:extract-op))
              (output (first (asdf:output-files op component))))
         (when (probe-file output)
           (delete-file output))
         (asdf:perform op component)
         (is (probe-file output))
         (is (string= +expected-content+ (uiop:read-file-string output))))
    (cleanup-fixture)))

(test extract-op-staleness
  "Verify that ASDF skips extraction when the output is already newer than the archive."
  (unwind-protect
       (let* ((component (fixture-component))
              (op (asdf:make-operation 'pathway/asdf:extract-op))
              (output (first (asdf:output-files op component))))
         (when (probe-file output)
           (delete-file output))
         ;; First extraction via ASDF operate (goes through plan)
         (asdf:operate 'pathway/asdf:extract-op component)
         (is (probe-file output))
         (sleep +sleep-interval+)
         ;; Touch the output to make it newer than the archive
         (with-open-file (s output :direction :output
                                   :if-exists :append)
           s)
         (let ((touched-write-date (file-write-date output)))
           ;; Second operate should skip extraction since output is newer
           (asdf:operate 'pathway/asdf:extract-op component)
           (is (= touched-write-date (file-write-date output)))))
    (cleanup-fixture)))

(test missing-archive-signals-error
  "Verify that EXTRACT-OP signals an error when the archive does not exist."
  (unwind-protect
       (let* ((component (fixture-component))
              (op (asdf:make-operation 'pathway/asdf:extract-op))
              ;; Temporarily rename the archive to simulate missing file
              (archive (first (asdf:input-files op component)))
              (backup (merge-pathnames "test-archive.zip.bak"
                                       (uiop:pathname-directory-pathname archive))))
         (rename-file archive backup)
         (unwind-protect
              (signals error (asdf:perform op component))
           (rename-file backup archive)))
    (cleanup-fixture)))
