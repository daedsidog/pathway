(defpackage #:pathway/asdf/workspace-extract
  (:use #:clean)
  (:local-nicknames (#:pw #:pathway))
  (:import-from #:pathway/pathname-utilities #:*default-workspace-pathname-resolver*)
  (:export #:extract-op
           #:workspace-extract
           #:workspace-pathname))

(in-package #:pathway/asdf/workspace-extract)

(defclass extract-op (asdf:non-propagating-operation) ()
  (:documentation "Extraction operation for WORKSPACE-EXTRACT components."))

(defclass workspace-extract (asdf:file-component)
  ((type :initform nil)
   (workspace-pathname :initarg       :workspace-pathname
                       :reader        workspace-pathname
                       :documentation "Relative pathname within the workspace directory."))
  (:documentation
   "Component extracting files from archives or directories into the system workspace."))

(defmethod shared-initialize :after ((c workspace-extract) slot-names &key)
  (declare (ignore slot-names))
  (unless (slot-boundp c 'workspace-pathname)
    (let ((name (asdf:component-name c)))
      (setf (slot-value c 'workspace-pathname)
            (if (archive-path-p name)
                (multiple-value-bind (archive internal)
                    (parse-archive-path name)
                  (declare (ignore archive))
                  (if (wildcard-path-p internal)
                      ""
                      internal))
                (if (wildcard-path-p name)
                    ""
                    (file-namestring (pathname name))))))))

(defun archive-path-p (name)
  "Return T if NAME references a file inside an archive."
  (and (search ".zip/" name) t))

(defun wildcard-path-p (path)
  "Return T if PATH contains wildcard characters."
  (or (and (search "*" path) t)
      (string= path "")
      (char= (char path (1- (length path))) #\/)))

(defun parse-archive-path (name)
  "Parse NAME into archive and internal paths, returning both as strings."
  (let ((zip-pos (search ".zip/" name)))
    (unless zip-pos
      (error "Invalid archive component name: ~A" name))
    (let ((split-pos (+ zip-pos #.(length ".zip"))))
      (values (subseq name 0 split-pos)
              (subseq name (1+ split-pos))))))

(defun source-pathname (component)
  "Return the source file pathname for COMPONENT."
  (let ((system (asdf:component-system component)))
    (if (archive-path-p (asdf:component-name component))
        (multiple-value-bind (archive-relative internal)
            (parse-archive-path (asdf:component-name component))
          (declare (ignore internal))
          (merge-pathnames archive-relative
                          (asdf:system-source-directory system)))
        (merge-pathnames (asdf:component-name component)
                         (asdf:system-source-directory system)))))

(defun output-pathname (component)
  "Return the output pathname for a single-file COMPONENT in the workspace."
  (merge-pathnames (workspace-pathname component)
                   (pw:default-workspace-pathname)))

(defun output-directory (component)
  "Return the output directory for a glob COMPONENT in the workspace."
  (merge-pathnames (uiop:ensure-directory-pathname (workspace-pathname component))
                   (pw:default-workspace-pathname)))

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
  "Convert PATTERN-STRING into a CL wildcard pathname."
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
            (dir-components (mapcar #'segment-to-directory-component dir-parts))
            (dot-pos (position #\. file-part :from-end t))
            (name (if dot-pos (subseq file-part 0 dot-pos) file-part))
            (type (when dot-pos (subseq file-part (1+ dot-pos)))))
       (make-pathname
         :directory `(:relative ,@dir-components)
         :name (if (string= name "*") :wild name)
         :type (cond
                 ((nullp type) :wild)
                 ((string= type "*") :wild)
                 (t type)))))))

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

(defun ensure-relative-pathname (pathname)
  "Normalize PATHNAME to have an explicit relative directory."
  (if (pathname-directory pathname)
      pathname
      (make-pathname :directory '(:relative) :defaults pathname)))

(defun collect-matching-files (directory wildcard)
  "Collect files under DIRECTORY matching WILDCARD."
  (let ((results nil)
        (directory (truename directory)))
    (labels ((walk (dir)
               (dolist (entry (uiop:directory-files dir))
                 (let ((relative (enough-namestring entry directory)))
                   (when (pathname-match-p
                           (ensure-relative-pathname (pathname relative))
                           wildcard)
                     (push (cons relative entry) results))))
               (dolist (subdir (uiop:subdirectories dir))
                 (walk subdir))))
      (walk directory))
    (nreverse results)))

(defun file-up-to-date-p (source dest)
  "Return T when DEST exists and is at least as new as SOURCE."
  (and (probe-file dest)
       (>= (file-write-date dest) (file-write-date source))))

(defun copy-matching-files (source-dir output-dir wildcard
                            &optional (prefix ""))
  "Copy files matching WILDCARD from SOURCE-DIR to OUTPUT-DIR, stripping PREFIX."
  (dolist (pair (collect-matching-files source-dir wildcard))
    (let* ((relative (strip-prefix (car pair) prefix))
           (dest (merge-pathnames relative output-dir)))
      (unless (file-up-to-date-p (cdr pair) dest)
        (ensure-directories-exist dest)
        (uiop:copy-file (cdr pair) dest)))))

(defun glob-component-p (component)
  "Return T if COMPONENT uses a glob pattern."
  (let ((name (asdf:component-name component)))
    (if (archive-path-p name)
        (multiple-value-bind (archive internal)
            (parse-archive-path name)
          (declare (ignore archive))
          (wildcard-path-p internal))
        (wildcard-path-p name))))

(defmethod asdf:input-files ((op extract-op) (c workspace-extract))
  (list (source-pathname c)))

(defmethod asdf:output-files ((op extract-op) (c workspace-extract))
  (if (glob-component-p c)
      (values nil t)
      (values (list (output-pathname c)) t)))

(defmethod asdf:operation-done-p ((op extract-op) (c workspace-extract))
  (if (glob-component-p c)
      (let ((archive (first (asdf:input-files op c)))
            (output-dir (output-directory c)))
        (and (uiop:directory-exists-p output-dir)
             (>= (file-write-date output-dir)
                 (file-write-date archive))))
      (call-next-method)))

(defmethod asdf:perform ((op extract-op) (c workspace-extract))
  (if (glob-component-p c)
      (perform-glob-extract c)
      (perform-single-extract c)))

(defun perform-single-extract (component)
  "Extract or copy a single file into the workspace."
  (let ((source (first (asdf:input-files (asdf:make-operation 'extract-op) component)))
        (output (output-pathname component)))
    (unless (file-up-to-date-p source output)
      (ensure-directories-exist output)
      (if (archive-path-p (asdf:component-name component))
          (multiple-value-bind (archive-relative internal-path)
              (parse-archive-path (asdf:component-name component))
            (declare (ignore archive-relative))
            (pw:extract-from-archive source (list (cons internal-path output))))
          (uiop:copy-file source output)))))

(defun perform-glob-extract (component)
  "Extract or copy matching files into the workspace."
  (let ((output-dir (output-directory component)))
    (if (archive-path-p (asdf:component-name component))
        (multiple-value-bind (archive-relative pattern-string)
            (parse-archive-path (asdf:component-name component))
          (declare (ignore archive-relative))
          (let ((wildcard (parse-wildcard pattern-string))
                (prefix (static-prefix pattern-string)))
            (pw:with-transient-directory (temp-dir)
              (pw:extract-archive
                (first (asdf:input-files (asdf:make-operation 'extract-op) component))
                temp-dir)
              (copy-matching-files temp-dir output-dir wildcard prefix))))
        (let ((source-dir (uiop:pathname-directory-pathname
                            (source-pathname component)))
              (pattern-string (asdf:component-name component)))
          (copy-matching-files source-dir output-dir
                              (parse-wildcard pattern-string))))))

(defmethod asdf:component-depends-on ((op asdf:load-op) (c workspace-extract))
  `((extract-op ,c) ,@(call-next-method)))

(defmethod asdf:output-files ((op asdf:operation) (c workspace-extract))
  (values nil t))

(defmethod asdf:perform ((op asdf:compile-op) (c workspace-extract))
  nil)

(defmethod asdf:perform ((op asdf:load-op) (c workspace-extract))
  nil)

(defun component-wildcard (component)
  "Return the CL wildcard pathname for a glob COMPONENT."
  (let ((name (asdf:component-name component)))
    (if (archive-path-p name)
        (multiple-value-bind (archive internal)
            (parse-archive-path name)
          (declare (ignore archive))
          (parse-wildcard internal))
        (parse-wildcard name))))

(defun component-workspace-files (component)
  "Return workspace-relative namestrings for COMPONENT."
  (if (glob-component-p component)
      (let ((dir (output-directory component))
            (ws (pw:default-workspace-pathname))
            (wildcard (component-wildcard component)))
        (when (uiop:directory-exists-p dir)
          (let ((results nil))
            (labels ((walk (dir)
                       (dolist (file (uiop:directory-files dir))
                         (let ((relative (enough-namestring file ws)))
                           (when (pathname-match-p
                                   (ensure-relative-pathname (pathname relative))
                                   wildcard)
                             (push relative results))))
                       (dolist (sub (uiop:subdirectories dir))
                         (walk sub))))
              (walk dir))
            (nreverse results))))
      (let ((path (output-pathname component)))
        (when (probe-file path)
          (list (enough-namestring path (pw:default-workspace-pathname)))))))

(defun collect-workspace-extracts (system)
  "Return all WORKSPACE-EXTRACT components in SYSTEM."
  (let ((results nil))
    (labels ((walk (c)
               (typecase c
                 (workspace-extract (push c results))
                 (asdf:parent-component
                  (dolist (child (asdf:module-components c))
                    (walk child))))))
      (walk (asdf:find-system system)))
    (nreverse results)))

(defun system-workspace-files (&rest systems)
  "Return workspace-relative namestrings for WORKSPACE-EXTRACT components in SYSTEMS.

For a single system, return a flat list of namestrings.  For multiple systems, return an alist keyed
by system keyword."
  (flet ((system-files (system)
           (loop :for c :in (collect-workspace-extracts system)
                 :nconc (component-workspace-files c))))
    (if (= 1 (length systems))
        (system-files (first systems))
        (loop :for sys :in systems
              :for files := (system-files sys)
              :when files
              :collect (cons (intern (string-upcase (asdf:component-name
                                                      (asdf:find-system sys)))
                                     :keyword)
                             files)))))

(defun root-level-file-p (namestring)
  "Return T when NAMESTRING has no directory components."
  (let ((dir (pathname-directory (pathname namestring))))
    (or (null dir) (equal dir '(:relative)))))

(defmethod asdf:perform :before ((op asdf:image-op) (system asdf:system))
  (let* ((exe (first (asdf:output-files op system)))
         (target-dir (uiop:pathname-directory-pathname exe))
         (ws (pw:default-workspace-pathname))
         (components (remove-if-not
                       (lambda (c) (typep c 'workspace-extract))
                       (asdf:required-components system :other-systems t))))
    (ensure-directories-exist exe)
    (dolist (c components)
      (dolist (file (component-workspace-files c))
        (when (root-level-file-p file)
          (let ((source (merge-pathnames file ws))
                (dest (merge-pathnames file
                                       (uiop:ensure-directory-pathname target-dir))))
            (uiop:copy-file source dest)))))
    (setf *default-workspace-pathname-resolver*
          (lambda ()
            (uiop:pathname-directory-pathname
              (or #+sbcl sb-ext:*runtime-pathname*
                  (let ((parsed (uiop:parse-native-namestring
                                  (first (uiop:raw-command-line-arguments)))))
                    (if (uiop:absolute-pathname-p parsed)
                        parsed
                        (merge-pathnames parsed (uiop:getcwd))))))))))
