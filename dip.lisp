(in-package #:dip)


(defun array3d->2d (image)
  (let* ((newimage (make-array (list (rows image) (* (cols image) (channels image)))
                               :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector newimage))
         (size (array-total-size image)))
    (loop for i below size
       do (setf (aref dst i) (aref src i)))
    newimage))

(defun array2d->3d (image channels)
  (assert (zerop (mod (cols image) channels)))
  (let* ((newimage (make-array (list (rows image) (/ (cols image) channels) channels)
                               :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector newimage))
         (size (array-total-size image)))
    (loop for i below size
       do (setf (aref dst i) (aref src i)))
    newimage))

(defun copy (image)
  (let ((newimage (make-array (array-dimensions image)
                              :element-type (array-element-type image))))
    (let ((src (array-storage-vector image))
          (dst (array-storage-vector newimage)))
      (loop for i below (array-total-size image)
         do (setf (aref dst i) (aref src i))))
    newimage))


(defun convert (image method)
  (ecase method
    (:rgb->bgr (bgr<->rgb image))
    (:bgr->rgb (bgr<->rgb image))
    (:rgb->gray (rgb->gray image))
    (:gray->rgb (gray->rgb image))))

(defun bgr<->rgb (image)
  (let ((vector (array-storage-vector image)))
    (loop for i below (array-dimension vector 0) by 3
       do (rotatef (aref vector i) (aref vector (+ i 2))))
    image))

(declaim (inline mean3))
(defun mean3 (a b c)
  (declare (type unsigned-byte a b c))
  (the unsigned-byte (truncate (the unsigned-byte (+ a b c)) 3)))

(defun rgb->gray (image)
  (let* ((gray (make-array (list (rows image) (cols image))
                           :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector gray)))
    (loop for i below (length src) by 3
       for j by 1
       do (setf (aref dst j) (mean3 (aref src i)
                                    (aref src (+ i 1))
                                    (aref src (+ i 2)))))
    gray))

(defun gray->rgb (image)
  (let* ((rgb (make-array (list (rows image) (cols image) 3)
                          :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector rgb)))
    (loop for i below (length src)
       for j by 3       
       for val = (aref src i) then (aref src i)
       do (setf (aref dst j) val
                (aref dst (+ j 1)) val
                (aref dst (+ j 2)) val))
    rgb))

(defun rgba->rgb (image)
  (let* ((rgb (make-array (list (rows image) (cols image) 3)
                          :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector rgb)))
    (loop for i below (length src) by 4
       for j by 3
       do (setf (aref dst j)       (aref src i)
                (aref dst (+ j 1)) (aref src (+ i 1))
                (aref dst (+ j 2)) (aref src (+ i 2))))
    rgb))

(defun rgb->rgba (image)
  (let* ((rgba (make-array (list (rows image) (cols image) 4)
                           :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector rgba)))
    (loop for i below (length src) by 3
       for j by 4
       do (setf (aref dst j)       (aref src i)
                (aref dst (+ j 1)) (aref src (+ i 1))
                (aref dst (+ j 2)) (aref src (+ i 2))
                (aref dst (+ j 3)) 255))
    rgba))

(defun flip (image code)
  (ecase code
    ((:h :x 0) (flip-horizontal image))
    ((:v :y 1) (flip-vertical image))
    ((:hv :xy -1) (flip-both image))))

(declaim (inline flip-vertical flip-horizontal flip-both))
(defun flip-vertical (image)
  (let ((rows (rows image))
        (cols (cols image))
        (channels (channels image)))
    (if (= channels 1)
        (loop for i below (truncate rows 2)
           with sum = (- rows 1)
           do (loop for j below cols
                 do (rotatef (aref image i j) (aref image (- sum i) j))))
        (loop for i below (truncate rows 2)
           with sum = (- rows 1)
           do (loop for j below cols
                 do (loop for k below channels
                       do (rotatef (aref image i j k) (aref image (- sum i) j k)))))))
  image)

(defun flip-horizontal (image)
  (let ((rows (rows image))
        (cols (cols image))
        (channels (channels image)))
    (if (= channels 1)
        (loop for i below rows
           with sum = (- cols 1)
           do (loop for j below (truncate cols 2)
                 do (rotatef (aref image i j) (aref image i (- sum j)))))
        (loop for i below rows
           with sum = (- cols 1)
           do (loop for j below (truncate cols 2)
                 do (loop for k below channels
                       do (rotatef (aref image i j k) (aref image i (- sum j) k)))))))
  image)

(defun flip-both (image)
  (let ((height (height image))
        (width (width image))
        (channels (channels image)))
    (if (= channels 1)
        (progn
          ;; tl <-> br
          (loop for y to (truncate height 2)
             with sumy = (- height 1)
             with sumx = (- width 1)
             do (loop for x below (truncate width 2)
                   do (rotatef (aref image y x) (aref image (- sumy y) (- sumx x)))))
          ;; tr <-> bl
          (loop for y below (truncate height 2)
             with sumy = (- height 1)
             with sumx = (- width 1)
             do (loop for x from (truncate width 2) below width
                   do (rotatef (aref image y x) (aref image (- sumy y) (- sumx x))))))
        (progn
          ;; tl <-> br
          (loop for y to (truncate height 2)
             with sumy = (- height 1)
             with sumx = (- width 1)
             do (loop for x below (truncate width 2)
                   do (loop for c below channels
                         do (rotatef (aref image y x c) (aref image (- sumy y) (- sumx x) c)))))
          ;; tr <-> bl
          (loop for y below (truncate height 2)
             with sumy = (- height 1)
             with sumx = (- width 1)
             do (loop for x from (truncate width 2) below width
                   do (loop for c below channels
                         do (rotatef (aref image y x c) (aref image (- sumy y) (- sumx x) c))))))))
  image)

(defun rotate (image angle)
  (declare (ignore image angle)))
