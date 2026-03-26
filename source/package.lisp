;;;; Copyright (C) 2024 DAEDSIDOG.  All rights reserved.

(defpackage #:pathway
  (:use #:clean)
  (:export #:absolute-pathname
           #:relative-pathname
           #:pathname-stem
           #:parent-directory
           #:cwd
           #:file-extension
           #:file-base
           #:user-home-directory
           #:default-temporary-directory
           #:file-age
           #:default-cache-directory
           #:with-cwd
           #:with-transient-file
           #:with-transient-directory
           #:extract-from-archive
           #:copy-if-newer))
