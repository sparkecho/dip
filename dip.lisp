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
  (let* ((newimage (make-similar image))
         (src (array-storage-vector image))
         (dst (array-storage-vector newimage)))
    (loop for i below (array-total-size image)
          do (setf (aref dst i) (aref src i)))
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

;; TODO: add support while element type of image is float
(defun rgb->gray (image)
  (let* ((element-type (array-element-type image))
         (gray (make-array (list (rows image) (cols image))
                           :element-type element-type))
         (src (array-storage-vector image))
         (dst (array-storage-vector gray)))
    (loop for i below (length src) by 3
       for j by 1
       do (setf (aref dst j)
                (round (+ (* 0.2989 (aref src i))
                          (* 0.5870 (aref src (+ i 1)))
                          (* 0.1140 (aref src (+ i 2)))))))
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

(defun rgba<->abgr (image)
  (let* ((abgr (make-array (array-dimensions image)
                           :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector abgr)))
    (loop for i below (length src) by 4
       do (setf (aref dst i)       (aref src (+ i 3))
                (aref dst (+ i 1)) (aref src (+ i 2))
                (aref dst (+ i 2)) (aref src (+ i 1))
                (aref dst (+ i 3)) (aref src i)))
    abgr))

(defun rgba->bgra (image)
  (let* ((bgra (make-array (array-dimensions image)
                           :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector bgra)))
    (loop for i below (length src) by 4
       do (setf (aref dst i)       (aref src (+ i 2))
                (aref dst (+ i 1)) (aref src (+ i 1))
                (aref dst (+ i 2)) (aref src i)
                (aref dst (+ i 3)) (aref src (+ i 3))))
    bgra))

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


(defun pyr-down (image)
  (let* ((drows (truncate (rows image) 2))
         (dcols (truncate (cols image) 2))
         (dchannels (channels image))
         (dimage (make-array (list drows dcols dchannels)
                           :element-type (array-element-type image))))
    (loop for i below drows
          do (loop for j below dcols
                   do (loop for k below dchannels
                            do (setf (aref dimage i j k)
                                     (aref image (* i 2) (* j 2) k)))))
    dimage))


;; split an image to a list of images
;; each element of it is a channel of the image
(defun split-image (image)
  (let ((rows (rows image))
        (cols (cols image))
        (channels (channels image))
        (element-type (array-element-type image)))
    (if (= channels 1)
        (list image)
        (loop for k below channels
              collect (let ((dst (make-array (list rows cols)
                                             :element-type element-type)))
                        (loop for i below rows
                              do (loop for j below cols
                                       do (setf (aref dst i j)
                                                (aref image i j k))))
                        dst)))))

;; merge a list of 2d images to a multi-channel 3d image
(defun merge-image (image-list)
  (let* ((img0 (car image-list))
         (rows (rows img0))
         (cols (cols img0))
         (element-type (array-element-type img0))
         (channels (length image-list))
         (image (make-array (list rows cols channels)
                            :element-type element-type)))
    (loop for img in image-list
            for k = 0 then (+ k 1)
            do (loop for i below rows
                     do (loop for j below cols
                              do (setf (aref image i j k) (aref img i j)))))
      image))

;; TODO
;; method:
;; :center, :top-left(default), :bottom-right, :top-right, :bottom-left
;; (defun calc-offsets (rows1 cols1 rows2 cols2 method)
;;   )
;; (defun calc-row-offset (row1 row2 method)
;;   )
;; (defun calc-col-offset (col1 col2 method)
;;   )

;; border-type:
;; :constant, :wrap, :replicate, :reflect, :reflect101
;; TODO: add multi-channel image support
;; TODO: optimize the structure and style of code
;; TODO: add support when pad size > image size
(defun copy-make-border (image top bottom left right
                         &optional (border-type :reflect101) (value 0))
  (let* ((rows (rows image))
         (cols (cols image))
         (channels (channels image))
         ;; the row bottom padding begins
         (bottom-begin (+ rows top))
         ;; the col right padding begins
         (right-begin (+ cols left))
         (dst-rows (+ rows top bottom))
         (dst-cols (+ cols left right))
         (dst-image (make-image-from image dst-rows dst-cols :x left :y top)))
    (ecase border-type
      (:reflect101
       ;; top
       (when (= channels 1)
         (loop for i below top
               do (loop for j from left below right-begin
                        do (setf (aref dst-image i j)
                                 (aref dst-image
                                       (+ top (- top i))
                                       j))))
         ;; bottom
         (loop for i from bottom-begin below dst-rows
               do (loop for j from left below right-begin
                        do (setf (aref dst-image i j)
                                 (aref dst-image
                                       (- bottom-begin (- i bottom-begin) 2)
                                       j))))
         ;; left and right
         (loop for i from top below bottom-begin
               do (loop for j below left
                        do (setf (aref dst-image i j)
                                 (aref dst-image
                                       i
                                       (+ left (- left j)))))
               do (loop for j from right-begin below dst-cols
                        do (setf (aref dst-image i j)
                                 (aref dst-image
                                       i
                                       (- right-begin (- j right-begin) 2)))))
         ;; top left and top right
         (loop for i below top
               do (loop for j below left
                        do (setf (aref dst-image i j)
                                 (aref dst-image
                                       (+ top (- top i))
                                       (+ left (- left j)))))
               do (loop for j from right-begin below dst-cols
                        do (setf (aref dst-image i j)
                                 (aref dst-image
                                       (+ top (- top i))
                                       (- right-begin (- j right-begin) 2)))))
         ;; bottom left and bottom right
         (loop for i from bottom-begin below dst-rows
               do (loop for j below left
                        do (setf (aref dst-image i j)
                                 (aref dst-image
                                       (- bottom-begin (- i bottom-begin) 2)
                                       (+ left (- left j)))))
               do (loop for j from right-begin below dst-cols
                        do (setf (aref dst-image i j)
                                 (aref dst-image
                                       (- bottom-begin (- i bottom-begin) 2)
                                       (- right-begin (- j right-begin) 2))))))
       (when (/= channels 1)
         (loop for k below channels
               do (loop for i below top
                        do (loop for j from left below right-begin
                                 do (setf (aref dst-image i j k)
                                          (aref dst-image
                                                (+ top (- top i))
                                                j
                                                k))))
                  ;; bottom
               do (loop for i from bottom-begin below dst-rows
                        do (loop for j from left below right-begin
                                 do (setf (aref dst-image i j k)
                                          (aref dst-image
                                                (- bottom-begin (- i bottom-begin) 2)
                                                j
                                                k))))
                  ;; left and right
               do (loop for i from top below bottom-begin
                        do (loop for j below left
                                 do (setf (aref dst-image i j k)
                                          (aref dst-image
                                                i
                                                (+ left (- left j))
                                                k)))
                        do (loop for j from right-begin below dst-cols
                                 do (setf (aref dst-image i j k)
                                          (aref dst-image
                                                i
                                                (- right-begin (- j right-begin) 2)
                                                k))))
                  ;; top left and top right
               do (loop for i below top
                        do (loop for j below left
                                 do (setf (aref dst-image i j k)
                                          (aref dst-image
                                                (+ top (- top i))
                                                (+ left (- left j))
                                                k)))
                        do (loop for j from right-begin below dst-cols
                                 do (setf (aref dst-image i j k)
                                          (aref dst-image
                                                (+ top (- top i))
                                                (- right-begin (- j right-begin) 2)
                                                k))))
                  ;; bottom left and bottom right
               do (loop for i from bottom-begin below dst-rows
                        do (loop for j below left
                                 do (setf (aref dst-image i j k)
                                          (aref dst-image
                                                (- bottom-begin (- i bottom-begin) 2)
                                                (+ left (- left j))
                                                k)))
                        do (loop for j from right-begin below dst-cols
                                 do (setf (aref dst-image i j k)
                                          (aref dst-image
                                                (- bottom-begin (- i bottom-begin) 2)
                                                (- right-begin (- j right-begin) 2)
                                                k)))))))
      (:reflect :reflect)
      (:replicate :replicate)
      (:wrap :wrap)
      (:constant
       ;; top, with tl and tr corners
       (loop for i below top
             do (loop for j below dst-cols
                      do (setf (aref dst-image i j) value)))
       ;; bottom, with bl and br corners
       (loop for i from bottom-begin below dst-rows
             do (loop for j below dst-cols
                      do (setf (aref dst-image i j) value)))
       ;; left and right
       (loop for i from top below bottom-begin
             do (loop for j below left
                      do (setf (aref dst-image i j) value))
             do (loop for j from right-begin below dst-cols
                      do (setf (aref dst-image i j) value)))))
    dst-image))


;; convolution
;; TODO: add multi-channel image support
;; TODO: add multi-channel kernel support
;; TODO: add more arguments, anchor, delta, border-type
;; use float type to calculate by default
(defun convolution (image kernel &optional (normalize t))
  (let* ((rows (rows image))
         (cols (cols image))
         (channels (channels image))
         (kernel-rows (rows kernel))
         (kernel-cols (cols kernel))
         (pad-rows (truncate kernel-rows 2))
         (pad-cols (truncate kernel-cols 2))
         (number (if normalize
                     (array-total-size kernel)
                     1))
         (padded (copy-make-border image
                                   pad-rows pad-rows
                                   pad-cols pad-cols))
         (dst-image (make-similar image)))
    (if (= channels 1)
        (loop for i below rows
              do (loop for j below cols
                       do (setf (aref dst-image i j)
                                (/ (loop for ii below kernel-rows
                                         sum (loop for jj below kernel-cols
                                                   sum (* (aref padded (+ i ii) (+ j jj))
                                                          (aref kernel ii jj))))
                                   number))))
        (loop for k below channels
              do (loop for i below rows
                       do (loop for j below cols
                                do (setf (aref dst-image i j k)
                                         (/ (loop for ii below kernel-rows
                                                  sum (loop for jj below kernel-cols
                                                            sum (* (aref padded (+ i ii) (+ j jj) k)
                                                                   (aref kernel ii jj))))
                                            number))))))
    dst-image))

;; FP style code
;; (setf (aref dst-image i j)
;;       (funcall #'(lambda (val) (alexandria:clamp val low high))
;;                (funcall #'(lambda (sum) (/ sum number))
;;                         (loop for ii below kernel-rows
;;                               sum (loop for jj below kernel-cols
;;                                         sum (* (aref padded (+ i ii) (+ j jj))
;;                                                (aref kernel ii jj)))))))

;; sobel
;; 3x3 dx
;; (convolution image #2A((-1 0 1)
;;                        (-2 0 2)
;;                        (-1 0 1)))
;; 3x3 dy
;; (convolution image #2A((-1 -2 -1)
;;                        ( 0  0  0)
;;                        ( 1  2  1)))
