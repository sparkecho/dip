(in-package #:dip)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Input
(declaim (inline get-imread-fn))
(defun get-imread-fn (filename)
  (alexandria:switch ((pathname-type filename) :test #'string=)
    ("jpeg" #'imread-jpeg)
    ("jpg"  #'imread-jpeg)))

(defun imread (filename)
  (funcall (get-imread-fn filename) filename))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Output
(declaim (inline get-imwrite-fn))
(defun get-imwrite-fn (filename)
  (alexandria:switch ((pathname-type filename) :test #'string=)
    ("jpeg" #'imwrite-jpeg)
    ("jpg"  #'imwrite-jpeg)))


(defun imwrite (filename image &key (force nil))
  (when force
    (ensure-directories-exist filename))
  (funcall (get-imwrite-fn filename) filename image))
