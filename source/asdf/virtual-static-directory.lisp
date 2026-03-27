;;;; Copyright (C) 2026 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway/asdf/virtual-static-directory
  (:use #:clean #:pathway/asdf/virtual-static-file)
  (:local-nicknames (#:pw #:pathway))
  (:export #:virtual-static-directory))

(in-package #:pathway/asdf/virtual-static-directory)

(defclass virtual-static-directory (asdf:static-file)
  ((virtual-pathname :initarg :virtual-pathname
                     :reader virtual-pathname
                     :documentation
                     "Relative directory pathname within the virtual root"))
  (:documentation "A directory tree mapped to the virtual root"))

(defun output-directory (component)
  "Return the output directory for COMPONENT under the virtual root."
  (merge-pathnames (virtual-pathname component)
                   (cache-directory component)))

(defun split-path-segments (path-string separator)
  "Split PATH-STRING on SEPARATOR into a list of non-empty segments."
  (remove "" (uiop:split-string path-string :separator separator)
         :test #'string=))

(defun segment-to-directory-component (segment)
  "Convert a path SEGMENT to a CL directory component."
  (cond
    ((string= segment "**") :wild-inferiors)
    ((string= segment "*") :wild)
    (t segment)))

(defun parse-wildcard (pattern-string)
  "Convert PATTERN-STRING into a CL wildcard pathname, where an empty
string or trailing path separator matches everything.

<result> ::= pathname"
  (cond
    ((or (string= pattern-string "")
         (string= pattern-string "/"))
     (make-pathname :directory '(:relative :wild-inferiors)
                    :name :wild :type :wild))
    ((char= (char pattern-string (1- (length pattern-string)))
            #\/)
     (let ((dirs (mapcar #'segment-to-directory-component
                         (split-path-segments pattern-string "/"))))
       (make-pathname :directory `(:relative ,@dirs :wild-inferiors)
                      :name :wild :type :wild)))
    (t
     (let* ((segments (split-path-segments pattern-string "/"))
            (file-part (car (last segments)))
            (dir-parts (butlast segments))
            (dir-components (mapcar #'segment-to-directory-component
                                    dir-parts))
            (dot-pos (position #\. file-part :from-end t))
            (name (if dot-pos (subseq file-part 0 dot-pos) file-part))
            (type (when dot-pos (subseq file-part (1+ dot-pos)))))
       (make-pathname
         :directory `(:relative ,@dir-components)
         :name (if (string= name "*") :wild name)
         :type (cond
                 ((null type) :wild)
                 ((string= type "*") :wild)
                 (t type)))))))

(defun collect-matching-files (directory wildcard)
  "Collect files under DIRECTORY matching WILDCARD.

<result>   ::= ({<match>}*)
<match>    ::= (<relative> . <absolute>)
<relative> ::= string
<absolute> ::= pathname"
  (let ((results nil)
        (directory (truename directory)))
    (labels ((walk (dir)
               (dolist (entry (uiop:directory-files dir))
                 (let ((relative (enough-namestring entry directory)))
                   (when (pathname-match-p (pathname relative) wildcard)
                     (push (cons relative entry) results))))
               (dolist (subdir (uiop:subdirectories dir))
                 (walk subdir))))
      (walk directory))
    (nreverse results)))

(defun static-prefix (pattern-string)
  "Return the non-wildcard directory prefix of PATTERN-STRING, or empty string."
  (let ((segments (split-path-segments pattern-string "/")))
    (if (and segments
             (char= (char pattern-string (1- (length pattern-string))) #\/)
             (notany (lambda (s) (or (string= s "*") (string= s "**")))
                     segments))
        pattern-string
        "")))

(defun strip-prefix (path prefix)
  "Remove PREFIX from the beginning of PATH string."
  (if (and (plusp (length prefix))
           (>= (length path) (length prefix))
           (string= path prefix :end1 (length prefix)))
      (subseq path (length prefix))
      path))

(defun copy-matching-files (source-dir output-dir wildcard
                            &optional (prefix ""))
  "Copy files matching WILDCARD from SOURCE-DIR to OUTPUT-DIR, stripping PREFIX."
  (dolist (pair (collect-matching-files source-dir wildcard))
    (let* ((relative (strip-prefix (car pair) prefix))
           (dest (merge-pathnames relative output-dir)))
      (ensure-directories-exist dest)
      (uiop:copy-file (cdr pair) dest))))

(defmethod asdf:input-files ((op extract-op) (c virtual-static-directory))
  (list (source-pathname c)))

(defmethod asdf:output-files ((op extract-op) (c virtual-static-directory))
  (values nil t))

(defmethod asdf:operation-done-p ((op extract-op) (c virtual-static-directory))
  "Check if the output directory exists and is newer than the archive."
  (let ((archive (first (asdf:input-files op c)))
        (output-dir (output-directory c)))
    (and (uiop:directory-exists-p output-dir)
         (>= (file-write-date output-dir)
             (file-write-date archive)))))

(defmethod asdf:perform ((op extract-op) (c virtual-static-directory))
  (let ((output-dir (output-directory c)))
    (if (archive-component-p c)
        (multiple-value-bind (archive-relative pattern-string)
            (parse-archive-path (asdf:component-name c))
          (declare (ignore archive-relative))
          (let ((wildcard (parse-wildcard pattern-string))
                (prefix (static-prefix pattern-string)))
            (pw:with-transient-directory (temp-dir)
              (pw:extract-archive (first (asdf:input-files op c)) temp-dir)
              (copy-matching-files temp-dir output-dir wildcard prefix))))
        (let ((source (first (asdf:input-files op c))))
          (copy-matching-files source output-dir
                               (make-pathname :directory '(:relative :wild-inferiors)
                                              :name :wild :type :wild))))))

(defmethod asdf:component-depends-on ((op asdf:load-op) (c virtual-static-directory))
  `((extract-op ,c) ,@(call-next-method)))

(defmethod asdf:perform ((op asdf:compile-op) (c virtual-static-directory))
  nil)

(defmethod asdf:perform ((op asdf:load-op) (c virtual-static-directory))
  (setf (gethash (pathname (asdf:component-name c)) *virtual-map-table*)
        (uiop:ensure-directory-pathname (pathname (virtual-pathname c)))))
