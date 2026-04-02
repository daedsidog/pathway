(defpackage #:pathway/pathname-utilities
  (:use #:clean)
  (:export #:absolute-pathname
           #:relative-pathname
           #:pathname-stem
           #:parent-directory
           #:cwd
           #:file-extension
           #:file-base
           #:user-homedir-pathname
           #:default-tempdir-pathname
           #:default-cachedir-pathname
           #:default-workspace-pathname))

(in-package #:pathway/pathname-utilities)

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
            (error "~A is not an absolute or relative pathname." pathname)))))

(defun pathname-stem (pathname)
  "Return the stem extracted of the provided PATHNAME."
  (check-type pathname (or string pathname))
  (let* ((name (file-namestring pathname))
         (pos  (position #\. name :from-end t)))
    (cond
      ((and pos (= pos 0)) name)
      (pos (subseq name 0 pos))
      (t name))))

(defun parent-directory (pathname)
  "Return the parent pathname of the given PATHNAME."
  (uiop:pathname-parent-directory-pathname pathname))

(defun file-extension (pathname)
  "Return the file extension of the given PATHNAME or empty string if none."
  (check-type pathname (or string pathname))
  (let ((name (namestring pathname)))
    (let ((pos (position #\. name :from-end t)))
      (if pos
          (subseq name (1+ pos))
          ""))))

(defun file-base (pathname)
  "Return the base name of the file without extension."
  (check-type pathname (or string pathname))
  (let* ((name (file-namestring pathname))
         (pos  (position #\. name :from-end t)))
    (cond
      ((and pos (= pos 0)) "")
      (pos (subseq name 0 pos))
      (t name))))

(setf (fdefinition 'cwd) #'uiop:getcwd)

(defun default-tempdir-pathname ()
  "Return the system's temporary directory as a pathname."
  (uiop:default-temporary-directory))

(let ((cache-directory nil))
  (defun default-cachedir-pathname (&optional subdirectory)
    "Return the system cache directory as a pathname, optionally with a suffixed SUBDIRECTORY."
    (unless cache-directory
      (setf cache-directory
            #+win32
            (or (ignore-errors (uiop:get-folder-path :appdata))
                (merge-pathnames "AppData/Roaming/" (user-homedir-pathname)))
            #-win32
            (merge-pathnames ".cache/" (user-homedir-pathname))))
    (if subdirectory
        (uiop:ensure-directory-pathname (merge-pathnames subdirectory cache-directory))
        cache-directory)))

(defvar *default-workspace-pathname-resolver*
  (lambda ()
    (case uiop:*image-dumped-p*
      (:executable
       (uiop:pathname-directory-pathname
         (let ((parsed (uiop:parse-native-namestring (uiop:argv0))))
           (if (uiop:absolute-pathname-p parsed)
               parsed
               (uiop:truename* parsed)))))
      (otherwise
       (default-cachedir-pathname
         (make-pathname :directory
           (list :relative
                 #+win32 "Pathway"
                 #-win32 "pathway"))))))
  "Designator for a function of zero arguments returning the workspace directory pathname")

(defun default-workspace-pathname ()
  "Return the default workspace directory pathname."
  (funcall *default-workspace-pathname-resolver*))
