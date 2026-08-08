(defpackage #:pathway/tests/filesystem-utilities-test
  (:use #:clean #:fiveam)
  (:local-nicknames (#:pw #:pathway))
  (:import-from #:pathway/tests/common #:+sleep-interval+)
  (:export #:run-tests))

(in-package #:pathway/tests/filesystem-utilities-test)

(def-suite* filesystem-utilities-test)

(define-condition test-error (error) ()
  (:documentation "Error signaled deliberately in tests."))

(defparameter +test-content+ "Test content")
(defparameter +source-content+ "Source content")

(test with-transient-file-creation
  "Test that WITH-TRANSIENT-FILE creates a stream."
  (let (captured-pathname)
    (pw:with-transient-file temp-stream
      (setf captured-pathname (pathname temp-stream))
      (write-line +test-content+ temp-stream)
      (file-position temp-stream 0)
      (is (equalp +test-content+ (read-line temp-stream))))
    (is (nullp (probe-file captured-pathname)))))

(test with-transient-file-cleanup-on-error
  "Test that WITH-TRANSIENT-FILE cleans up even when an error occurs."
  (let (captured-pathname)
    (handler-case
        (pw:with-transient-file temp-stream
          (setf captured-pathname (pathname temp-stream))
          (write-line +test-content+ temp-stream)
          (error 'test-error))
      (error (e)
        (declare (ignore e))))
    (is (nullp (probe-file captured-pathname)))))

(test with-transient-file-unique-names
  "Test that WITH-TRANSIENT-FILE generates unique files."
  (let (pathname1 pathname2
        (content-1 "Stream 1")
        (content-2 "Stream 2"))
    (pw:with-transient-file stream1
      (setf pathname1 (pathname stream1))
      (pw:with-transient-file stream2
        (setf pathname2 (pathname stream2))
        (is (not (equalp (namestring pathname1) (namestring pathname2))))
        (write-line content-1 stream1)
        (write-line content-2 stream2)
        (file-position stream1 0)
        (file-position stream2 0)
        (is (equalp content-1 (read-line stream1)))
        (is (equalp content-2 (read-line stream2)))))
    (is (nullp (probe-file pathname1)))
    (is (nullp (probe-file pathname2)))))

(test with-transient-file-nested-usage
  "Test nested usage of WITH-TRANSIENT-FILE."
  (let (outer-pathname inner-pathname
        (outer-content "Outer content")
        (inner-content "Inner content"))
    (pw:with-transient-file outer-stream
      (setf outer-pathname (pathname outer-stream))
      (write-line outer-content outer-stream)
      (pw:with-transient-file inner-stream
        (setf inner-pathname (pathname inner-stream))
        (write-line inner-content inner-stream)
        (file-position outer-stream 0)
        (file-position inner-stream 0)
        (is (equalp outer-content (read-line outer-stream)))
        (is (equalp inner-content (read-line inner-stream))))
      (file-position outer-stream 0)
      (is (equalp outer-content (read-line outer-stream)))
      (is (nullp (probe-file inner-pathname))))
    (is (nullp (probe-file outer-pathname)))
    (is (nullp (probe-file inner-pathname)))))

(test file-age-function
  "Test FILE-AGE function with existing and non-existing files."
  (signals error (pw:file-age "/path/that/does/not/exist"))
  (signals error (pw:file-age (pw:default-tempdir-pathname)))
  (pw:with-transient-file temp-stream
    (let ((temp-path (pathname temp-stream)))
      (write-line +test-content+ temp-stream)
      (force-output temp-stream)
      (let ((age (pw:file-age temp-path)))
        (is (not (nullp age)))
        (is (integerp age))
        (is (< (- (universal-time) age) 60)))))
  (pw:with-transient-file temp-stream
    (let ((temp-path-string (namestring (pathname temp-stream))))
      (write-line +test-content+ temp-stream)
      (force-output temp-stream)
      (let ((age (pw:file-age temp-path-string)))
        (is (integerp age))
        (is (> age 0)))))
  (pw:with-transient-file temp-stream
    (let ((temp-path (pathname temp-stream)))
      (write-line +test-content+ temp-stream)
      (force-output temp-stream)
      (let ((first-age (pw:file-age temp-path)))
        (sleep +sleep-interval+)
        (write-line "Modified content" temp-stream)
        (force-output temp-stream)
        (let ((second-age (pw:file-age temp-path)))
          (is (>= second-age first-age)))))))

(test default-cachedir-pathname-function
  "Test DEFAULT-CACHEDIR-PATHNAME function with and without subdirectories."
  (let ((cache-dir (pw:default-cachedir-pathname)))
    (is (uiop:directory-pathname-p cache-dir))
    (is (probe-file cache-dir)))
  (let ((cache-subdir (pw:default-cachedir-pathname "test/subdir")))
    (is (uiop:directory-pathname-p cache-subdir))
    (is (probe-file cache-subdir))
    (let ((cache-str (namestring cache-subdir)))
      (is (search "test" cache-str))
      (is (search "subdir" cache-str))))
  (let ((nested-cache (pw:default-cachedir-pathname "deep/nested/subdirectory/path")))
    (is (uiop:directory-pathname-p nested-cache))
    (is (probe-file nested-cache))
    (let ((cache-str (namestring nested-cache)))
      (is (search "deep" cache-str))
      (is (search "nested" cache-str))
      (is (search "subdirectory" cache-str))
      (is (search "path" cache-str))))
  (let ((special-cache (pw:default-cachedir-pathname "test-dir_with.special-chars")))
    (is (uiop:directory-pathname-p special-cache))
    (is (probe-file special-cache))
    (let ((cache-str (namestring special-cache)))
      (is (search "test-dir_with.special-chars" cache-str))))
  (let ((cache1 (pw:default-cachedir-pathname "consistency-test"))
        (cache2 (pw:default-cachedir-pathname "consistency-test")))
    (is (string= (namestring cache1) (namestring cache2)))))

(test copy-if-newer-function
  "Test COPY-IF-NEWER function with various scenarios."
  (pw:with-transient-file source-stream
    (let ((source-path (pathname source-stream))
          (dest-path   (merge-pathnames "dest-file.txt" (pw:default-tempdir-pathname))))
      (write-line +source-content+ source-stream)
      (force-output source-stream)
      (when (probe-file dest-path)
        (delete-file dest-path))
      (is (pw:copy-if-newer source-path dest-path))
      (is (probe-file dest-path))
      (when (probe-file dest-path)
        (delete-file dest-path))))
  ;; Test: source newer than dest triggers copy
  (let ((source-path (merge-pathnames "source-file2.txt" (pw:default-tempdir-pathname)))
        (dest-path   (merge-pathnames "dest-file2.txt" (pw:default-tempdir-pathname))))
    (unwind-protect
         (progn
           ;; Create dest file first
           (with-open-file (dest-stream dest-path
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
             (write-line "Old content" dest-stream))
           (sleep +sleep-interval+)
           ;; Create source file after (so it's newer)
           (with-open-file (source-stream source-path
                                          :direction :output
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
             (write-line "New content" source-stream))
           (is (pw:copy-if-newer source-path dest-path)))
      (when (probe-file source-path) (delete-file source-path))
      (when (probe-file dest-path) (delete-file dest-path))))
  (pw:with-transient-file source-stream
    (let* ((source-path    (pathname source-stream))
           (temp-dir       (merge-pathnames "test-dir/" (pw:default-tempdir-pathname)))
           (expected-dest  (merge-pathnames (file-namestring source-path) temp-dir))
           (content        "File to directory content"))
      (write-line content source-stream)
      (force-output source-stream)
      (ensure-directories-exist temp-dir)
      (when (probe-file expected-dest)
        (delete-file expected-dest))
      (is (pw:copy-if-newer source-path temp-dir))
      (is (probe-file expected-dest))
      (with-open-file (dest-stream expected-dest :direction :input)
        (is (string= content (read-line dest-stream))))
      (when (probe-file expected-dest)
        (delete-file expected-dest))))
  (pw:with-transient-file source-stream
    (let ((source-path (pathname source-stream))
          (dest-path   (merge-pathnames "newer-dest.txt" (pw:default-tempdir-pathname)))
          (dest-content "Newer destination content"))
      (write-line +source-content+ source-stream)
      (force-output source-stream)
      (sleep +sleep-interval+)
      (with-open-file (dest-stream dest-path
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
        (write-line dest-content dest-stream))
      (is (nullp (pw:copy-if-newer source-path dest-path)))
      (with-open-file (dest-stream dest-path :direction :input)
        (is (string= dest-content (read-line dest-stream))))
      (when (probe-file dest-path)
        (delete-file dest-path))))
  (pw:with-transient-file source-stream
    (let ((source-path-string (namestring (pathname source-stream)))
          (dest-path-string   (namestring (merge-pathnames "string-test.txt"
                                                           (pw:default-tempdir-pathname)))))
      (write-line +source-content+ source-stream)
      (force-output source-stream)
      (when (probe-file dest-path-string)
        (delete-file dest-path-string))
      (is (pw:copy-if-newer source-path-string dest-path-string))
      (is (probe-file dest-path-string))
      (when (probe-file dest-path-string)
        (delete-file dest-path-string))))
  (pw:with-transient-file source-stream
    (let ((source-path (pathname source-stream)))
      (write-line +source-content+ source-stream)
      (force-output source-stream)
      (is (nullp (pw:copy-if-newer source-path source-path))))))

(defun run-tests ()
  (run! 'filesystem-utilities-test))
