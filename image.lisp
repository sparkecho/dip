(in-package #:dip)


;;;; Create image object (2d or 3d array)
(defun make-image (rows cols channels data)
  (let* ((dimensions (if (= channels 1)
                         (list rows cols)
                         (list rows cols channels)))
         (image (make-array dimensions :element-type (array-element-type data)))
         (vector (array-storage-vector image)))
    (loop for i below (array-total-size image)
       do (setf (aref vector i) (aref data i)))
    image))

;;;; Reture a vector (1-dimension array) which contains the given array's data
(declaim (inline array-storage-vector))
(defun array-storage-vector (array)
  #+sbcl (sb-ext:array-storage-vector array))

;;;; Query image's basic information
(declaim (inline height width rows cols channels))

(defun height (image)
  (array-dimension image 0))

(defun width (image)
  (array-dimension image 1))

(defun rows (image)
  (array-dimension image 0))

(defun cols (image)
  (array-dimension image 1))

(defun channels (image)
  (if (= (array-rank image) 2)
      1
      (array-dimension image 2)))
