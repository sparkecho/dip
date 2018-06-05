(in-package #:dip)

(defun imread-jpeg (filename)
  (multiple-value-bind (buffer height width ncomp)
      (cl-jpeg:decode-image filename)
    (if (= ncomp 1)
        ;; grayscale image
        (let* ((image (make-array (list height width)
                                  :element-type (array-element-type buffer)))
               (dst (array-storage-vector image)))
          (loop for i below (length buffer)
             do (setf (aref dst i) (aref buffer i)))
          image)
        ;; RGB truecolor image
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
(defparameter *gray-q-tabs* (vector jpeg::+q-luminance+))
(defun imwrite-jpeg (filename image)
  (let ((channels (channels image))
        (height (height image))
        (width (width image))
        (data (array-storage-vector image)))
    (cond ((= channels 1) (jpeg:encode-image filename data 1 height width :q-tabs *gray-q-tabs*))
          ((> channels 2) (let ((dst (make-array (* height width 3)
                                                 :element-type (array-element-type image))))
                            (loop for i below (length data) by channels
                               for j by 3
                               do (setf (aref dst j)       (aref data (+ i 2))
                                        (aref dst (+ j 1)) (aref data (+ i 1))
                                        (aref dst (+ j 2)) (aref data i)))
                            (jpeg:encode-image filename dst 3 height width)))
          (t (error "Unsupported channel number: ~A" channels)))))
