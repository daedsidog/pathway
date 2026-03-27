;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(in-package #:pathway/tests)

(def-suite* asdf-test :in pathway-test)

(defparameter +fixture-asd+
  (merge-pathnames "tests/asdf/fixtures/test-system-asdf.lisp"
                   (asdf:system-source-directory (asdf:find-system "pathway")))
  "Pathname to the test fixture system definition")

(defparameter +test-system-name+ "pathway-test-mapped")
(defparameter +test-component-name+ "test-archive.zip/archived-test-file.txt")
(defparameter +expected-content+
  (subseq +test-component-name+
          (+ (search ".zip/" +test-component-name+) #.(length ".zip/"))))

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

(defun delete-if-exists (pathname)
  (when (probe-file pathname)
    (delete-file pathname)))

(defun cleanup-fixture (&optional (name +test-component-name+))
  "Clear the test system and remove any extracted/copied files."
  (delete-if-exists (fixture-output-path name))
  (asdf:clear-system +test-system-name+))

;;; Smoke tests

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
  (is (eq (find-class 'asdf::virtual-static-file)
          (find-class 'pathway/asdf:virtual-static-file))))

;;; Integration test generator

(defmacro def-extract-op-tests (prefix component-name expected-content
                                 output-search input-search)
  "Generate the four standard EXTRACT-OP integration tests for a component."
  (flet ((test-name (suffix)
           (intern (format nil "~A-~A" (string prefix) (string suffix)))))
    `(progn
       (test ,(test-name 'output-files)
         ,(format nil "Verify OUTPUT-FILES for ~A returns a cache path." prefix)
         (unwind-protect
              (let* ((component (fixture-component ,component-name))
                     (op (extract-op))
                     (outputs (asdf:output-files op component)))
                (is (= 1 (length outputs)))
                (is (search ,output-search (namestring (first outputs)))))
           (cleanup-fixture ,component-name)))

       (test ,(test-name 'input-files)
         ,(format nil "Verify INPUT-FILES for ~A returns the source path." prefix)
         (unwind-protect
              (let* ((component (fixture-component ,component-name))
                     (op (extract-op))
                     (inputs (asdf:input-files op component)))
                (is (= 1 (length inputs)))
                (is (search ,input-search (namestring (first inputs))))
                (is (probe-file (first inputs))))
           (cleanup-fixture ,component-name)))

       (test ,(test-name 'perform)
         ,(format nil "Verify EXTRACT-OP produces correct content for ~A." prefix)
         (unwind-protect
              (let* ((component (fixture-component ,component-name))
                     (op (extract-op))
                     (output (first (asdf:output-files op component))))
                (delete-if-exists output)
                (asdf:perform op component)
                (is (probe-file output))
                (is (string= ,expected-content (uiop:read-file-string output))))
           (cleanup-fixture ,component-name)))

       (test ,(test-name 'staleness)
         ,(format nil "Verify ASDF skips extraction for ~A when output is newer." prefix)
         (unwind-protect
              (let* ((component (fixture-component ,component-name))
                     (op (extract-op))
                     (output (first (asdf:output-files op component))))
                (delete-if-exists output)
                ;; First extraction via ASDF:OPERATE (goes through plan)
                (asdf:operate 'pathway/asdf:extract-op component)
                (is (probe-file output))
                (sleep +sleep-interval+)
                ;; Touch the output to make it newer than the source
                (with-open-file (s output :direction :output
                                          :if-exists :append)
                  s)
                (let ((touched-write-date (file-write-date output)))
                  ;; Second ASDF:OPERATE should skip since output is newer
                  (asdf:operate 'pathway/asdf:extract-op component)
                  (is (= touched-write-date (file-write-date output)))))
           (cleanup-fixture ,component-name))))))

;;; Integration tests

(def-extract-op-tests archive
  +test-component-name+ +expected-content+
  +expected-content+ (subseq +test-component-name+ 0
                             (+ (search ".zip/" +test-component-name+)
                                #.(length ".zip"))))

(def-extract-op-tests plain
  +plain-component-name+ +plain-expected-content+
  +plain-component-name+ +plain-component-name+)

;;; Archive-specific tests

(test missing-archive-signals-error
  "Verify that EXTRACT-OP signals an error when the archive does not exist."
  (unwind-protect
       (let* ((component (fixture-component))
              (op (extract-op))
              ;; Temporarily rename the archive to simulate missing file
              (archive (first (asdf:input-files op component)))
              (backup (merge-pathnames
                       (concatenate 'string (file-namestring archive) ".bak")
                       (uiop:pathname-directory-pathname archive))))
         (rename-file archive backup)
         (unwind-protect
              (signals error (asdf:perform op component))
           (rename-file backup archive)))
    (cleanup-fixture)))
