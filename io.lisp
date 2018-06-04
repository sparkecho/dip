(in-package #:dip)


(defun imread (filename)
  (multiple-value-bind (buffer height width ncomp)
      (cl-jpeg:decode-image filename)
    (make-image height width ncomp buffer)))

;; opticl/jpeg.lisp
;; (defparameter *rgb-sampling* '((1 1)(1 1)(1 1)))
;; (defparameter *rgb-q-tabs* (vector jpeg::+q-luminance-hi+
;;                                    jpeg::+q-chrominance-hi+))
;; (defparameter *gray-q-tabs* (vector jpeg::+q-luminance+))
(defun imwrite (filename image)
  (let ((ncomp (channels image)))
    (jpeg:encode-image filename (array-storage-vector image)
                       ncomp (height image) (width image))))
