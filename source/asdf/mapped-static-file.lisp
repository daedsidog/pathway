;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(in-package #:pathway/asdf)

;;; Operation

(defclass extract-op (asdf:non-propagating-operation) ()
  (:documentation "Archive file extraction operation"))

;;; Component

(defclass mapped-static-file (asdf:static-file)
  ((target-pathname :initarg :target-pathname
                    :reader target-pathname
                    :documentation "Relative pathname of the extracted file within the cache directory"))
  (:documentation "A static file archived in the source tree, extracted to a cache location"))

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
  (pw:user-cache-directory
   (make-pathname :directory `(:relative ,(asdf:component-name
                                           (asdf:component-system component))))))

(defun archive-pathname (component)
  (multiple-value-bind (archive-relative internal)
      (parse-archive-path (asdf:component-name component))
    (declare (ignore internal))
    (merge-pathnames archive-relative
                     (asdf:system-source-directory (asdf:component-system component)))))

(defun output-pathname (component)
  (merge-pathnames (target-pathname component) (cache-directory component)))

;;; ASDF protocol methods

(defmethod asdf:input-files ((op extract-op) (c mapped-static-file))
  (list (archive-pathname c)))

(defmethod asdf:output-files ((op extract-op) (c mapped-static-file))
  (values (list (output-pathname c)) t))

(defmethod asdf:perform ((op extract-op) (c mapped-static-file))
  (let ((archive (first (asdf:input-files op c)))
        (output  (first (asdf:output-files op c))))
    (multiple-value-bind (archive-relative internal-path)
        (parse-archive-path (asdf:component-name c))
      (declare (ignore archive-relative))
      (pw:extract-files-from-archive archive (list (cons internal-path output))))))

(defmethod asdf:component-depends-on ((op asdf:load-op) (c mapped-static-file))
  `((extract-op ,c) ,@(call-next-method)))

(defmethod asdf:perform ((op asdf:compile-op) (c mapped-static-file))
  nil)

(defmethod asdf:perform ((op asdf:load-op) (c mapped-static-file))
  nil)

(import 'mapped-static-file :asdf)
