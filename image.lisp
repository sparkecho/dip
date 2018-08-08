(in-package #:dip)

;;;; Reture a vector (1-dimension array) which contains the given array's data
(declaim (inline array-storage-vector))
(defun array-storage-vector (array)
  #+sbcl (sb-ext:array-storage-vector array)
  #-sbcl (make-array (array-total-size array)
                     :element-type (array-element-type array)
                     :displaced-to array))


;;;; Image creation
(defgeneric make-image (rows cols channels data &optional element-type)
  (:documentation "Create image object (2d or 3d array)."))

;; (dip::make-image 2 2 2 2) => #3A(((2 2) (2 2)) ((2 2) (2 2)))
(defmethod make-image (rows cols channels (data number)
                       &optional (element-type '(unsigned-byte 8)))
  (let ((dimensions (if (= channels 1)
                        (list rows cols)
                        (list rows cols channels))))
    (make-array dimensions :element-type element-type :initial-element data)))

;; (dip::make-image 2 2 2 #(1 2)) => #3A(((1 2) (1 2)) ((1 2) (1 2)))
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

;; (dip::make-image 2 2 2 #2A((1 2 3 4) (5 6 7 8)) '(unsigned-byte 16))
;; => #3A(((1 2) (3 4)) ((5 6) (7 8)))
(defmethod make-image (rows cols channels (data array) &optional element-type)
  (let* ((dimensions (if (= channels 1)
                         (list rows cols)
                         (list rows cols channels)))
         (image (make-array dimensions
                            :element-type (or element-type (array-element-type data))))
         (src (if (vectorp data)
                  data
                  (array-storage-vector data)))
         (dst (array-storage-vector image)))
    (loop for i below (length src)
       do (setf (aref dst i) (aref src i)))
    image))


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


;; make an image that has the same property as the input
(declaim (inline make-similar))
(defun make-similar (image)
  (make-array (array-dimensions image)
              :element-type (array-element-type image)))


;; copy-method:
;; :inner, :outer
;; ADD copy-direction: tl, br, center. specify the meaning of x, y
(defun make-image-from (image rows cols &key (x 0) (y 0) (copy-method :outer))
  (let* ((channels (channels image))
         (dimensions (if (= channels 1)
                         (list rows cols)
                         (list rows cols channels)))
         (dst-image (make-array dimensions
                                :element-type (array-element-type image)))
         (src-rows (rows image))
         (src-cols (cols image)))
    (ecase copy-method
      (:outer (let ((row-interval (min src-rows (- rows y)))
                    (col-interval (min src-cols (- cols x))))
                (if (= channels 1)
                    (loop for i below row-interval
                          for r = (+ i y)
                          do (loop for j below col-interval
                                   for c = (+ j x)
                                   do (setf (aref dst-image r c)
                                            (aref image i j))))
                    (loop for k below channels
                          do (loop for i below row-interval
                                   for r = (+ i y)
                                   do (loop for j below col-interval
                                            for c = (+ j x)
                                            do (setf (aref dst-image r c k)
                                                     (aref image i j k))))))))
      (:inner (let ((row-interval (min rows (- src-rows y)))
                    (col-interval (min cols (- src-cols x))))
                (if (= channels 1)
                    (loop for i below row-interval
                          for r = (+ i y)
                          do (loop for j below col-interval
                                   for c = (+ j x)
                                   do (setf (aref dst-image i j)
                                            (aref image r c))))
                    (loop for k below channels
                          do (loop for i below row-interval
                                   for r = (+ i y)
                                   do (loop for j below col-interval
                                            for c = (+ j x)
                                            do (setf (aref dst-image i j k)
                                                     (aref image r c k)))))))))
    dst-image))


;; print image in a better format
;; TODO: support print specified region of image
(defun print-image (image)
  (let ((width 0)
        (rows (rows image))
        (cols (cols image))
        (channels (channels image))
        (data (array-storage-vector image)))
    (loop for i below (array-total-size data)
          do (let ((w (length (format nil "~D" (aref data i)))))
               (when (> w width)
                 (setf width w))))
    ;; in: width, out: control-string, 2 => "~3@A"
    (let ((control-string (format nil "~~~D@A " (1+ width))))
      (if (= channels 1)
          (loop for i below rows
                do (loop for j below cols
                         do (format t control-string (aref image i j)))
                do (terpri))
          (loop for k below channels
                do (loop for i below rows
                         do (loop for j below cols
                                  do (format t control-string (aref image i j k)))
                         do (terpri))
                do (terpri))))
    (values)))


;; fn: cast function, accept 1 arg
(defun copy-value (src dst &optional (fn #'identity))
  (let ((rows (rows src))
        (cols (cols src))
        (channels (channels src)))
    (if (= channels 1)
        (loop for i below rows
              do (loop for j below cols
                       do (setf (aref dst i j)
                                (funcall fn (aref src i j)))))
        (loop for k below channels
              do (loop for i below rows
                       do (loop for j below cols
                                do (setf (aref dst i j k)
                                         (funcall fn (aref src i j k)))))))
    dst))

(defun convert-type (image type)
  (let* ((element-type (array-element-type image))
         (dst-image (make-array (array-dimensions image)
                                :element-type type))
         (fn (cond ((subtypep element-type 'float)
                    (cond ((equal type '(unsigned-byte 8))
                           #'(lambda (x) (alexandria:clamp (round x) 0 255)))
                          ((subtypep type 'integer) #'round)
                          (t #'identity)))
                   ((subtypep type 'float) #'float)
                   (t #'identity))))
    (copy-value image dst-image fn)))
