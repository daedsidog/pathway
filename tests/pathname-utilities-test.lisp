(defpackage #:pathway/tests/pathname-utilities-test
  (:use #:clean #:fiveam)
  (:local-nicknames (#:pw #:pathway))
  (:export #:run-tests))

(in-package #:pathway/tests/pathname-utilities-test)

(def-suite* pathname-utilities-test)

(test pathname-resolution
  "Test pathname resolution functions."
  (let ((test-path "test/path"))
    (is (uiop:absolute-pathname-p (pw:absolute-pathname test-path)))
    (is (equalp "/absolute/path" (pw:absolute-pathname "/absolute/path"))))
  (let* ((cwd      (uiop:getcwd))
         (abs-path (merge-pathnames "test.txt" cwd)))
    (is (uiop:relative-pathname-p (pw:relative-pathname abs-path)))
    (is (equalp "relative/path" (pw:relative-pathname "relative/path"))))
  (is (equalp "file"     (pw:pathname-stem "/path/to/file.txt")))
  (is (equalp "file"     (pw:pathname-stem "file.txt")))
  (is (equalp ".profile" (pw:pathname-stem "/path/to/.profile")))
  (is (equalp ".profile" (pw:pathname-stem ".profile")))
  (is (equalp "txt"      (pw:file-extension "test.txt")))
  (is (equalp ""         (pw:file-extension "file")))
  (is (equalp "lisp"     (pw:file-extension "/path/to/test.lisp")))
  (is (equalp "test"     (pw:file-base "test.txt")))
  (is (equalp ""         (pw:file-base ".hidden")))
  (is (equalp "file"     (pw:file-base "file")))
  (is (equalp ""         (namestring (pw:parent-directory "dir/file"))))
  (is (uiop:relative-pathname-p (pw:parent-directory "file")))
  (is (uiop:relative-pathname-p (pw:parent-directory ""))))

(defun run-tests ()
  (run! 'pathname-utilities-test))
