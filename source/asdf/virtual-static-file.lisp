;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/asdf/virtual-static-file
  (:use #:clean)
  (:local-nicknames (#:pw #:pathway))
  (:export #:extract-op
           #:virtual-static-file
           #:virtual-pathname
           #:virtual-pathname-map))

(in-package #:pathway/asdf/virtual-static-file)

(defvar *virtual-map-table* (make-hash-table :test #'equal)
  "Map from ASDF component names to virtual pathnames")

;;; Operation

(defclass extract-op (asdf:non-propagating-operation) ()
  (:documentation "File extraction/copying operation for VIRTUAL-STATIC-FILE components"))

;;; Component

(defclass virtual-static-file (asdf:static-file)
  ((virtual-pathname :initarg :virtual-pathname
                     :reader virtual-pathname
                     :documentation
                     "Relative pathname of the file within the virtual root directory"))
  (:documentation
   "A static file in the source tree, mapped to a cache location.
Supports both archived files (name contains .zip/) and plain files."))

(defun archive-component-p (component)
  "Return T if COMPONENT names an archived file (contains .zip/ in the name)."
  (and (search ".zip/" (asdf:component-name component)) t))

(defun parse-archive-path (name)
  "Parse component NAME into archive-relative and internal paths.

<paths>         ::= (values <archive-path> <internal-path>)
<archive-path>  ::= string
<internal-path> ::= string"
  (let ((zip-pos (search ".zip/" name)))
    (unless zip-pos
      (error "Invalid archive component name: ~A" name))
    (let ((split-pos (+ zip-pos #.(length ".zip"))))
      (values (subseq name 0 split-pos)
              (subseq name (1+ split-pos))))))

(defun cache-directory (component)
  (merge-pathnames
    (make-pathname :directory `(:relative ,(asdf:component-name
                                             (asdf:component-system component))))
    (pw:virtual-root-pathname)))

(defun source-pathname (component)
  "Return the source file pathname for COMPONENT.
For archive components, returns the archive path on disk.
For plain components, returns the file path relative to the system source directory."
  (let ((system (asdf:component-system component)))
    (if (archive-component-p component)
        (multiple-value-bind (archive-relative internal)
            (parse-archive-path (asdf:component-name component))
          (declare (ignore internal))
          (merge-pathnames archive-relative
                           (asdf:system-source-directory system)))
        (merge-pathnames (asdf:component-name component)
                         (asdf:system-source-directory system)))))

(defun output-pathname (component)
  (merge-pathnames (virtual-pathname component) (cache-directory component)))

;;; ASDF protocol methods

(defmethod asdf:input-files ((op extract-op) (c virtual-static-file))
  (list (source-pathname c)))

(defmethod asdf:output-files ((op extract-op) (c virtual-static-file))
  (values (list (output-pathname c)) t))

(defmethod asdf:perform ((op extract-op) (c virtual-static-file))
  (let ((source (first (asdf:input-files op c)))
        (output (first (asdf:output-files op c))))
    (ensure-directories-exist output)
    (if (archive-component-p c)
        (multiple-value-bind (archive-relative internal-path)
            (parse-archive-path (asdf:component-name c))
          (declare (ignore archive-relative))
          (pw:extract-from-archive source (list (cons internal-path output))))
        (uiop:copy-file source output))
    (setf (gethash (pathname (asdf:component-name c)) *virtual-map-table*)
          (pathname (virtual-pathname c)))))

(defmethod asdf:component-depends-on ((op asdf:load-op) (c virtual-static-file))
  `((extract-op ,c) ,@(call-next-method)))

(defmethod asdf:perform ((op asdf:compile-op) (c virtual-static-file))
  nil)

(defmethod asdf:perform ((op asdf:load-op) (c virtual-static-file))
  nil)

;;; Map iteration

(defun virtual-pathname-map (function)
  "Apply FUNCTION to each mapping in the virtual pathname table:

<function>           ::= (function (<component-pathname> <virtual-pathname>))
<component-pathname> ::= pathname
<virtual-pathname>   ::= pathname"
  (maphash function *virtual-map-table*))
