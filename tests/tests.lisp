;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defpackage #:ck-fs/tests
  (:use #:cl #:ck-clle #:fiveam #:ck-fs)
  (:export #:run-tests))

(in-package #:ck-fs/tests)

(defparameter +sleep-interval+ 1.5
  "Sleep interval for tests that need timing differences.
Must be >1 second because FILE-WRITE-DATE returns universal time which has 1-second resolution.")

(defun run-tests ()
  (run! 'fs-test))

(def-suite* fs-test)

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

(test with-transient-file-creation
  "Test that WITH-TRANSIENT-FILE creates a stream."
  (let (captured-pathname)
    (with-transient-file temp-stream
      (setf captured-pathname (pathname temp-stream))
      (write-line "test content" temp-stream)
      (file-position temp-stream 0)
      (is (equalp "test content" (read-line temp-stream))))
    (is (nullp (probe-file captured-pathname)))))

(test with-transient-file-cleanup-on-error
  "Test that WITH-TRANSIENT-FILE cleans up even when an error occurs."
  (let (captured-pathname)
    (handler-case
        (with-transient-file temp-stream
          (setf captured-pathname (pathname temp-stream))
          (write-line "test" temp-stream)
          (error "Test error"))
      (error (e)
        (declare (ignore e))))
    (is (nullp (probe-file captured-pathname)))))

(test with-transient-file-unique-names
  "Test that WITH-TRANSIENT-FILE generates unique files."
  (let (pathname1 pathname2)
    (with-transient-file stream1
      (setf pathname1 (pathname stream1))
      (with-transient-file stream2
        (setf pathname2 (pathname stream2))
        (is (not (equalp (namestring pathname1) (namestring pathname2))))
        (write-line "stream1 content" stream1)
        (write-line "stream2 content" stream2)
        (file-position stream1 0)
        (file-position stream2 0)
        (is (equalp "stream1 content" (read-line stream1)))
        (is (equalp "stream2 content" (read-line stream2)))))
    (is (nullp (probe-file pathname1)))
    (is (nullp (probe-file pathname2)))))

(test with-transient-file-nested-usage
  "Test nested usage of WITH-TRANSIENT-FILE."
  (let (outer-pathname inner-pathname)
    (with-transient-file outer-stream
      (setf outer-pathname (pathname outer-stream))
      (write-line "outer content" outer-stream)
      (with-transient-file inner-stream
        (setf inner-pathname (pathname inner-stream))
        (write-line "inner content" inner-stream)
        (file-position outer-stream 0)
        (file-position inner-stream 0)
        (is (equalp "outer content" (read-line outer-stream)))
        (is (equalp "inner content" (read-line inner-stream))))
      (file-position outer-stream 0)
      (is (equalp "outer content" (read-line outer-stream)))
      (is (nullp (probe-file inner-pathname))))
    (is (nullp (probe-file outer-pathname)))
    (is (nullp (probe-file inner-pathname)))))

(test file-age-function
  "Test FILE-AGE function with existing and non-existing files."
  (signals error (file-age "/path/that/does/not/exist"))
  (signals error (file-age (user-temporary-directory)))
  (with-transient-file temp-stream
    (let ((temp-path (pathname temp-stream)))
      (write-line "test content" temp-stream)
      (force-output temp-stream)
      (let ((age (file-age temp-path)))
        (is (not (nullp age)))
        (is (integerp age))
        (is (< (- (get-universal-time) age) 60)))))
  (with-transient-file temp-stream
    (let ((temp-path-string (namestring (pathname temp-stream))))
      (write-line "string path test" temp-stream)
      (force-output temp-stream)
      (let ((age (file-age temp-path-string)))
        (is (integerp age))
        (is (> age 0)))))
  (with-transient-file temp-stream
    (let ((temp-path (pathname temp-stream)))
      (write-line "initial content" temp-stream)
      (force-output temp-stream)
      (let ((first-age (file-age temp-path)))
        (sleep +sleep-interval+)
        (write-line "modified content" temp-stream)
        (force-output temp-stream)
        (let ((second-age (file-age temp-path)))
          (is (>= second-age first-age)))))))

(test user-cache-directory-function
  "Test USER-CACHE-DIRECTORY function with and without subdirectories."
  (let ((cache-dir (user-cache-directory)))
    (is (uiop:directory-pathname-p cache-dir))
    (is (probe-file cache-dir)))
  (let ((cache-subdir (user-cache-directory "test/subdir")))
    (is (uiop:directory-pathname-p cache-subdir))
    (is (probe-file cache-subdir))
    (let ((cache-str (namestring cache-subdir)))
      (is (search "test" cache-str))
      (is (search "subdir" cache-str))))
  (let ((nested-cache (user-cache-directory "deep/nested/subdirectory/path")))
    (is (uiop:directory-pathname-p nested-cache))
    (is (probe-file nested-cache))
    (let ((cache-str (namestring nested-cache)))
      (is (search "deep" cache-str))
      (is (search "nested" cache-str))
      (is (search "subdirectory" cache-str))
      (is (search "path" cache-str))))
  (let ((special-cache (user-cache-directory "test-dir_with.special-chars")))
    (is (uiop:directory-pathname-p special-cache))
    (is (probe-file special-cache))
    (let ((cache-str (namestring special-cache)))
      (is (search "test-dir_with.special-chars" cache-str))))
  (let ((cache1 (user-cache-directory "consistency-test"))
        (cache2 (user-cache-directory "consistency-test")))
    (is (string= (namestring cache1) (namestring cache2)))))

(test copy-if-newer-function
  "Test COPY-IF-NEWER function with various scenarios."
  (with-transient-file source-stream
    (let ((source-path (pathname source-stream))
          (dest-path   (merge-pathnames "dest-file.txt" (user-temporary-directory))))
      (write-line "source content" source-stream)
      (force-output source-stream)
      (when (probe-file dest-path)
        (delete-file dest-path))
      (is (copy-if-newer source-path dest-path))
      (is (probe-file dest-path))
      (when (probe-file dest-path)
        (delete-file dest-path))))
  ;; Test: source newer than dest triggers copy
  (let ((source-path (merge-pathnames "source-file2.txt" (user-temporary-directory)))
        (dest-path   (merge-pathnames "dest-file2.txt" (user-temporary-directory))))
    (unwind-protect
         (progn
           ;; Create dest file first
           (with-open-file (dest-stream dest-path
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
             (write-line "old content" dest-stream))
           (sleep +sleep-interval+)
           ;; Create source file after (so it's newer)
           (with-open-file (source-stream source-path
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
             (write-line "new content" source-stream))
           (is (copy-if-newer source-path dest-path)))
      (when (probe-file source-path) (delete-file source-path))
      (when (probe-file dest-path) (delete-file dest-path))))
  (with-transient-file source-stream
    (let* ((source-path    (pathname source-stream))
           (temp-dir       (merge-pathnames "test-dir/" (user-temporary-directory)))
           (expected-dest  (merge-pathnames (file-namestring source-path) temp-dir)))
      (write-line "file to dir content" source-stream)
      (force-output source-stream)
      (ensure-directories-exist temp-dir)
      (when (probe-file expected-dest)
        (delete-file expected-dest))
      (is (copy-if-newer source-path temp-dir))
      (is (probe-file expected-dest))
      (with-open-file (dest-stream expected-dest :direction :input)
        (is (string= "file to dir content" (read-line dest-stream))))
      (when (probe-file expected-dest)
        (delete-file expected-dest))))
  (with-transient-file source-stream
    (let ((source-path (pathname source-stream))
          (dest-path   (merge-pathnames "newer-dest.txt" (user-temporary-directory))))
      (write-line "source content" source-stream)
      (force-output source-stream)
      (sleep +sleep-interval+)
      (with-open-file (dest-stream dest-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
        (write-line "newer destination content" dest-stream))
      (is (null (copy-if-newer source-path dest-path)))
      (with-open-file (dest-stream dest-path :direction :input)
        (is (string= "newer destination content" (read-line dest-stream))))
      (when (probe-file dest-path)
        (delete-file dest-path))))
  (with-transient-file source-stream
    (let ((source-path-string (namestring (pathname source-stream)))
          (dest-path-string   (namestring (merge-pathnames "string-test.txt"
                                                           (user-temporary-directory)))))
      (write-line "string path content" source-stream)
      (force-output source-stream)
      (when (probe-file dest-path-string)
        (delete-file dest-path-string))
      (is (copy-if-newer source-path-string dest-path-string))
      (is (probe-file dest-path-string))
      (when (probe-file dest-path-string)
        (delete-file dest-path-string))))
  (with-transient-file source-stream
    (let ((source-path (pathname source-stream)))
      (write-line "same file test" source-stream)
      (force-output source-stream)
      (is (null (copy-if-newer source-path source-path))))))
