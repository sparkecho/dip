(in-package #:dip)

(defun imread-jpeg (filename)
  (multiple-value-bind (buffer height width ncomp)
      (cl-jpeg:decode-image filename)
    (if (= ncomp 1)
        (make-image height width ncomp buffer) ;maybe reimpl to avoid judge ncomp again
        (make-rgb-from-bgr-vec height width buffer))))

(declaim (inline make-rgb-from-bgr-vec))
(defun make-rgb-from-bgr-vec (rows cols vector)
  (let* ((image (make-array (list rows cols 3)
                            :element-type (array-element-type vector)))
         (data (array-storage-vector image)))
    (loop for i below (length vector) by 3
       do (setf (aref data i)       (aref vector (+ i 2))
                (aref data (+ i 1)) (aref vector (+ i 1))
                (aref data (+ i 2)) (aref vector i)))
    image))

;; opticl/jpeg.lisp
;; (defparameter *rgb-sampling* '((1 1)(1 1)(1 1)))
;; (defparameter *rgb-q-tabs* (vector jpeg::+q-luminance-hi+
;;                                    jpeg::+q-chrominance-hi+))
;; (defparameter *gray-q-tabs* (vector jpeg::+q-luminance+))
(defun imwrite-jpeg (filename image)
  (let ((channels (channels image))
        (height (height image))
        (width (width image))
        (data (array-storage-vector image)))
    (cond ((= channels 1) (jpeg:encode-image filename data 1 height width))
          ((> channels 2) (let ((dst (make-array (* height width 3)
                                                 :element-type (array-element-type image))))
                            (loop for i below (length data) by channels
                               for j by 3
                               do (setf (aref dst j)       (aref data (+ i 2))
                                        (aref dst (+ j 1)) (aref data (+ i 1))
                                        (aref dst (+ j 2)) (aref data i)))
                            (jpeg:encode-image filename dst 3 height width)))
          (t (error "Unsupported channel number: ~A" channels)))))
