(in-package #:dip)

;;;; Image creation
(defgeneric make-image (rows cols channels data &optional element-type)
  (:documentation "Create image object (2d or 3d array)."))

(defmethod make-image (rows cols channels (data number)
                       &optional (element-type '(unsigned-byte 8)))
  (let ((dimensions (if (= channels 1)
                        (list rows cols)
                        (list rows cols channels))))
    (make-array dimensions :element-type element-type :initial-element data)))

(defmethod make-image (rows cols channels (data simple-vector)
                       &optional (element-type '(unsigned-byte 8)))
  (assert (= channels (length data)) nil
          "Initial value's length does not match with image's channel number")
  (let* ((dimensions (if (= channels 1)
                         (list rows cols)
                         (list rows cols channels)))
         (image (make-array dimensions :element-type element-type))
         (vector (array-storage-vector image)))
    (loop for i below (length vector) by channels
       do (loop for j below channels
             do (setf (aref vector (+ i j)) (svref data j))))
    image))

(defmethod make-image (rows cols channels (data array) &optional element-type)
  (let* ((dimensions (if (= channels 1)
                         (list rows cols)
                         (list rows cols channels)))
         (image (make-array dimensions
                            :element-type (or element-type (array-element-type data))))
         (vector (array-storage-vector image)))
    (loop for i below (length data)
       do (setf (aref vector i) (aref data i)))
    image))

;;;; Reture a vector (1-dimension array) which contains the given array's data
(declaim (inline array-storage-vector))
(defun array-storage-vector (array)
  #+sbcl (sb-ext:array-storage-vector array)
  #-sbcl (make-array (array-total-size array)
                     :element-type (array-element-type array)
                     :displaced-to array))

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
