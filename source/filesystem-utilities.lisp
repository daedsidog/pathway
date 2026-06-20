(defpackage #:pathway/filesystem-utilities
  (:use #:clean)
  (:import-from #:pathway/pathname-utilities
                #:default-tempdir-pathname)
  (:export #:file-age
           #:copy-if-newer
           #:with-cwd
           #:with-transient-file
           #:with-transient-directory
           #:extract-archive
           #:extract-from-archive))

(in-package #:pathway/filesystem-utilities)

(define-condition filesystem-error (simple-error) ()
  (:documentation "Error signaled for invalid filesystem operations."))

(defun file-age (pathname)
  "Return the modification time (universal time) of the given PATHNAME."
  (check-type pathname (or string pathname))
  (when (uiop:directory-pathname-p pathname)
    (error 'filesystem-error
           :format-control "'~A' is a directory, not a file."
           :format-arguments (list pathname)))
  (file-write-date pathname))

(defun copy-if-newer (source destination)
  "Copy SOURCE to DESTINATION if SOURCE is newer or DESTINATION absent,
returning pathname or nil."
  (check-type source (or string pathname))
  (check-type destination (or string pathname))
  (let ((final-destination (if (and (uiop:directory-pathname-p destination)
                                    (not (uiop:directory-pathname-p source)))
                               (merge-pathnames (file-namestring source) destination)
                               destination))
        (copy nil))
    (when (uiop:pathname-equal source final-destination)
      (return-from copy-if-newer nil))
    (handler-case
        (let ((dest-age (file-age final-destination)))
          (when (> (file-age source) dest-age)
            (setf copy t)))
      (error ()
        (setf copy t)))
    (when copy
      (ensure-directories-exist (uiop:pathname-parent-directory-pathname final-destination))
      (uiop:copy-file source final-destination)
      final-destination)))

(defmacro with-cwd (directory &body body)
  "Return the result of executing BODY with DIRECTORY as the current working
directory."
  `(let ((*default-pathname-defaults* ,directory))
     ,@body))

(defmacro with-transient-file (file-handle &body body)
  "Create a transient file in the system transient directory, open it as a
stream bound to FILE-HANDLE, & execute BODY, returning its result."
  (let ((temp-pathname (gensym "TEMP-PATHNAME")))
    `(let ((,temp-pathname (merge-pathnames (symbol-name (gensym "TEMP"))
                                            (default-tempdir-pathname))))
       (ensure-directories-exist ,temp-pathname)
       (unwind-protect
            (with-open-file (,file-handle ,temp-pathname
                                          :direction :io
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              ,@body)
         (when (probe-file ,temp-pathname)
           (delete-file ,temp-pathname))))))

(defmacro with-transient-directory ((directory-var) &body body)
  "Return the result of executing BODY with a transient directory bound to
DIRECTORY-VAR."
  `(let ((,directory-var
           (merge-pathnames (make-pathname :directory
                                           `(:relative ,(symbol-name (gensym "TEMP-DIR"))))
                            (default-tempdir-pathname))))
     (ensure-directories-exist ,directory-var)
     (unwind-protect
          (progn ,@body)
       (when (probe-file ,directory-var)
         (uiop:delete-directory-tree ,directory-var :validate t)))))

(defun zip-file-p (pathname)
  "Return T if PATHNAME is a valid ZIP archive."
  (check-type pathname (or string pathname))
  (when (probe-file pathname)
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program #+win32 (list "tar" "-tzf" (namestring pathname))
                              #-win32 (list "unzip" "-t" (namestring pathname))
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (zerop exit-code))
      (error () nil))))

(defun extract-archive (archive-path destination)
  "Extract the full contents of ARCHIVE-PATH to DESTINATION directory."
  (check-type archive-path (or string pathname))
  (check-type destination (or string pathname))
  (unless (probe-file archive-path)
    (error "Archive not found: ~A" archive-path))
  (unless (zip-file-p archive-path)
    (error "Not a ZIP archive: ~A" archive-path))
  (ensure-directories-exist destination)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program
        #+win32 (list "tar" "-xf" (namestring archive-path)
                      "-C" (namestring destination))
        #-win32 (list "unzip" "-q" "-o" (namestring archive-path)
                      "-d" (namestring destination))
        :ignore-error-status t)
    (declare (ignore output error-output))
    (unless (zerop exit-code)
      (error "Failed to extract archive: ~A" archive-path)))
  destination)

(defun extract-from-archive (archive-path file-specs)
  "Extract specific files from an archive, returning extracted pathnames.

FILE-SPECS is a list of conses where each CAR is the internal archive pathname
and each CDR is the destination pathname."
  (check-type file-specs list)
  (with-transient-directory (temp-dir)
    (extract-archive archive-path temp-dir)
    (loop :for (internal-path . destination) :in file-specs
          :for extracted-file := (merge-pathnames internal-path temp-dir)
          :do (unless (probe-file extracted-file)
                (error 'filesystem-error
                       :format-control "File '~A' not found in '~A'."
                       :format-arguments (list internal-path archive-path)))
              (ensure-directories-exist
                (uiop:pathname-parent-directory-pathname destination))
              (uiop:copy-file extracted-file destination)
          :collect destination)))
