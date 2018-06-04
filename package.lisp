;;;; package.lisp

(defpackage #:dip
  (:use #:cl)
  (:export #:imread
           #:imwrite
           #:imshow
           #:height
           #:width
           #:rows
           #:cols
           #:channels
           #:array2d->3d
           #:array3d->2d
           #:convert
           #:copy
           #:flip
           #:*default-backend*))
