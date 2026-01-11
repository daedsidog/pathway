;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defpackage #:ck-fs
  (:use #:cl #:ck-clle #:ck-pm)
  (:export #:absolute-pathname
           #:relative-pathname
           #:pathname-stem
           #:parent-directory
           #:cwd
           #:file-extension
           #:file-base
           #:user-home-directory
           #:user-temporary-directory
           #:file-age
           #:user-cache-directory
           #:with-working-directory
           #:with-transient-file
           #:with-temporary-directory
           #:extract-files-from-archive
           #:copy-if-newer))

(in-package #:ck-fs)

(defun absolute-pathname (pathname)
  "Return the absolute pathname of the given PATHNAME."
  (check-type pathname (or string pathname))
  (if (uiop:absolute-pathname-p pathname)
      pathname
      (merge-pathnames pathname (uiop:getcwd))))

(defun relative-pathname (pathname)
  "Return the relative pathname from the current directory to PATHNAME."
  (check-type pathname (or string pathname))
  (if (uiop:relative-pathname-p pathname)
      pathname
      (let* ((cwd      (uiop:getcwd))
             (absolute (uiop:absolute-pathname-p pathname)))
        (if absolute
            (uiop:parse-native-namestring
             (subseq (uiop:native-namestring pathname)
                     (length (uiop:native-namestring cwd))))
            (error "PATHNAME must be absolute or relative.")))))

(defun pathname-stem (pathname)
  "Extract and return the stem of the provided PATHNAME.
The stem is the path including the filename but excluding the extension."
  (check-type pathname (or string pathname))
  (let* ((name (file-namestring pathname))
         (pos  (position #\. name :from-end t)))
    (cond
      ((and pos (= pos 0)) name)
      (pos (subseq name 0 pos))
      (t name))))

(defun parent-directory (pathname)
  "Get the parent pathname of the given PATHNAME."
  (uiop:pathname-parent-directory-pathname pathname))

(setf (fdefinition 'cwd) #'uiop:getcwd)

(defun file-extension (pathname)
  "Get the file extension of the given PATHNAME.
Returns empty string if there is no extension."
  (check-type pathname (or string pathname))
  (let ((name (namestring pathname)))
    (let ((pos (position #\. name :from-end t)))
      (if pos
          (subseq name (1+ pos))
          ""))))

(defun file-base (pathname)
  "Get the base name of the file without extension."
  (check-type pathname (or string pathname))
  (let* ((name (file-namestring pathname))
         (pos  (position #\. name :from-end t)))
    (cond
      ((and pos (= pos 0)) "")
      (pos (subseq name 0 pos))
      (t name))))

(defun file-age (pathname)
  "Return the modification time (universal time) of the given PATHNAME.
Throws an error if the pathname is a directory."
  (check-type pathname (or string pathname))
  (when (uiop:directory-pathname-p pathname)
    (error "FILE-AGE does not work on directories: ~A." pathname))
  (file-write-date pathname))

(defun copy-if-newer (source destination)
  "Copy SOURCE to DESTINATION if SOURCE is newer than DESTINATION or if DESTINATION does not exist.
Returns the destination pathname if a copy was made, NIL otherwise."
  (check-type source (or string pathname))
  (check-type destination (or string pathname))
  (let* ((final-destination (if (and (uiop:directory-pathname-p destination)
                                     (not (uiop:directory-pathname-p source)))
                                (merge-pathnames (file-namestring source) destination)
                                destination))
         (should-copy nil))
    (when (uiop:pathname-equal source final-destination)
      (return-from copy-if-newer nil))
    (handler-case
        (let ((dest-age (file-age final-destination)))
          (when (> (file-age source) dest-age)
            (setf should-copy t)))
      (error ()
        (setf should-copy t)))
    (when should-copy
      (ensure-directories-exist (uiop:pathname-parent-directory-pathname final-destination))
      (uiop:copy-file source final-destination)
      final-destination)))

(defun user-home-directory ()
  "Return the user's home directory as a pathname."
  (user-homedir-pathname))

(defun user-temporary-directory ()
  "Return the user's temporary directory as a pathname."
  (uiop:default-temporary-directory))

(let ((cache-directory nil))
  (defun user-cache-directory (&optional subdirectory)
    "Return the user's cache directory as a pathname, optionally with a suffixed SUBDIRECTORY."
    (unless cache-directory
      (setf cache-directory
            #+win32
            (or (ignore-errors (uiop:get-folder-path :appdata))
                (merge-pathnames "AppData/Roaming/" (user-home-directory)))
            #-win32
            (merge-pathnames ".cache/" (user-home-directory))))
    (if subdirectory
        (uiop:ensure-directory-pathname (merge-pathnames subdirectory cache-directory))
        cache-directory)))

(defmacro with-working-directory (directory &body body)
  "Execute BODY with DIRECTORY as the current working directory."
  `(let ((*default-pathname-defaults* ,directory))
     ,@body))

(defmacro with-transient-file (file-handle &body body)
  "Create a temporary file in the system temporary directory, open it as a stream, bind the stream
to FILE-HANDLE, execute BODY, and ensure the file is closed and deleted after BODY completes (even
if an error occurs)."
  (let ((temp-pathname (gensym "TEMP-PATHNAME")))
    `(let ((,temp-pathname (merge-pathnames (symbol-name (gensym "TEMP"))
                                            (user-temporary-directory))))
       (ensure-directories-exist ,temp-pathname)
       (unwind-protect
            (with-open-file (,file-handle ,temp-pathname
                                          :direction :io
                                          :if-exists :supersede
                                          :if-does-not-exist :create)
              ,@body)
         (when (probe-file ,temp-pathname)
           (delete-file ,temp-pathname))))))

(defmacro with-temporary-directory ((directory-var) &body body)
  "Create a temporary directory, bind it to DIRECTORY-VAR, execute BODY, and clean up."
  `(let ((,directory-var
           (merge-pathnames (make-pathname :directory
                                           `(:relative ,(symbol-name (gensym "TEMP-DIR"))))
                            (user-temporary-directory))))
     (ensure-directories-exist ,directory-var)
     (unwind-protect
          (progn ,@body)
       (when (probe-file ,directory-var)
         (uiop:delete-directory-tree ,directory-var :validate t)))))

(defun zip-file-p (pathname)
  "Check if PATHNAME is a valid ZIP archive by testing it."
  (check-type pathname (or string pathname))
  (when (probe-file pathname)
    (handler-case
        (progn
          #+win32
          (start-process "tar" "-tzf" (namestring pathname) :ignore-error-status nil)
          #-win32
          (start-process "unzip" "-t" (namestring pathname) :ignore-error-status nil)
          t)
      (error () nil))))

(defun extract-files-from-archive (archive-path file-specs)
  "Extract multiple files from an archive in a single extraction operation.

Returns a list of destination pathnames for successfully extracted files.
FILE-SPECS is a list of (<internal pathname> . <destination pathname>) pairs."
  (check-type archive-path (or string pathname))
  (check-type file-specs list)
  (unless (probe-file archive-path)
    (error "Archive not found: ~A." archive-path))
  (unless (zip-file-p archive-path)
    (error "File is not a ZIP archive: ~A." archive-path))
  (with-temporary-directory (temp-dir)
    #+win32
    (start-process "tar" "-xf" (namestring archive-path) "-C" (namestring temp-dir))
    #-win32
    (start-process "unzip" "-q" "-o" (namestring archive-path) "-d" (namestring temp-dir))
    (loop :for (internal-path . destination) :in file-specs
          :for extracted-file := (merge-pathnames internal-path temp-dir)
          :do (unless (probe-file extracted-file)
                (error "File ~A not found in ~A." internal-path archive-path))
              (ensure-directories-exist (uiop:pathname-parent-directory-pathname destination))
              (uiop:copy-file extracted-file destination)
          :collect destination)))
