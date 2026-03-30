(defpackage #:pathway/tests/pathname-utilities-test
  (:use #:clean #:fiveam #:pathway)
  (:export #:run-tests))

(in-package #:pathway/tests/pathname-utilities-test)

(def-suite* pathname-utilities-test)

(test pathname-resolution
  "Test pathname resolution functions."
  (let ((test-path "test/path"))
    (is (uiop:absolute-pathname-p (absolute-pathname test-path)))
    (is (equalp "/absolute/path" (absolute-pathname "/absolute/path"))))
  (let* ((cwd      (uiop:getcwd))
         (abs-path (merge-pathnames "test.txt" cwd)))
    (is (uiop:relative-pathname-p (relative-pathname abs-path)))
    (is (equalp "relative/path" (relative-pathname "relative/path"))))
  (is (equalp "file"     (pathname-stem "/path/to/file.txt")))
  (is (equalp "file"     (pathname-stem "file.txt")))
  (is (equalp ".profile" (pathname-stem "/path/to/.profile")))
  (is (equalp ".profile" (pathname-stem ".profile")))
  (is (equalp "txt"      (file-extension "test.txt")))
  (is (equalp ""         (file-extension "file")))
  (is (equalp "lisp"     (file-extension "/path/to/test.lisp")))
  (is (equalp "test"     (file-base "test.txt")))
  (is (equalp ""         (file-base ".hidden")))
  (is (equalp "file"     (file-base "file")))
  (is (equalp ""         (namestring (parent-directory "dir/file"))))
  (is (uiop:relative-pathname-p (parent-directory "file")))
  (is (uiop:relative-pathname-p (parent-directory ""))))

(defun run-tests ()
  (run! 'pathname-utilities-test))
