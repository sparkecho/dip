;; hog.lisp
;; implementation of HoG algorithm
;; refs: https://www.learnopencv.com/histogram-of-oriented-gradients/

(in-package :dip)
;; preprocessing

;; (rgb->gray image)

(defun sobelx1 (image)
  (convolution image #2A((-1 0 1)) nil))

(defun sobely1 (image)
  (convolution image #2A((-1) (0) (1)) nil))

(defun norm2 (a b)
  (sqrt (+ (expt a 2) (expt b 2))))

(defun cart-to-polar (gx gy &optional angle-in-degrees-p)
  (declare (ignore angle-in-degrees-p))
  (let ((mag (make-similar gx))
        (angle (make-similar gx))
        (rows (rows gx))
        (cols (cols gx))
        (channels (channels gx)))
    (if (= channels 1)
        (loop for i below rows
              do (loop for j below cols
                       do (setf (aref mag i j)
                                (norm2 (aref gx i j) (aref gy i j))
                                (aref angle i j)
                                (atan (aref gy i j) (aref gx i j)))))
        (loop for k below channels
              do (loop for i below rows
                       do (loop for j below cols
                                do (setf (aref mag i j k)
                                         (norm2 (aref gx i j k) (aref gy i j k))
                                         (aref angle i j k)
                                         (atan (aref gy i j k) (aref gx i j k)))))))
    (values mag angle)))

;; (defparameter table (let ((tb (vector 0 20 40 60 80 100 120 140 160)))
;;                       (loop for i below 9
;;                             do (setf (svref tb i)
;;                                      (* pi (/ (svref tb i) 180))))
;;                       tb))
(defparameter table #(0 20 40 60 80 100 120 140 160))

;; radian -> degree
(defun radian-to-degree (radian)
  (values (round (* 180 (/ radian pi)))))

;; 0 <= degree < 180
(defun degree-abs (angle)
  (if (< angle 0)
      (+ angle 180)
      angle))

;; radian image to degree image
(defun rtd-image (radian-image)
  (copy-value radian-image
              (make-similar radian-image '(unsigned-byte 8))
              #'(lambda (radian)
                  (degree-abs (radian-to-degree radian)))))

(defun find-pos (angle table)
  (do ((i 0 (+ i 1)))
      ((or (> i 8)
           (> (aref table i) angle))
       i)))

;; calculate feature for a pixel
;; mag, angle: (image (* *) uint8)
(defun feature-pixel (mag angle table feature)
  (let* ((pos (find-pos angle table))
         (l (- pos 1))
         (r (mod pos 9))
         (valuel (aref table l))
         (ratior (/ (- angle valuel) 20))
         (ratiol (- 1 ratior)))
    (incf (aref feature l) (float (* mag ratiol)))
    (incf (aref feature r) (float (* mag ratior)))
    feature))

;; calculate feature vector for a cell
;; 2d image only
;; (defparameter mag-cns (split-image magnitude-uint8))
;; (defparameter ang-cns (split-image direction-uint8))
;; (feature-cell (car mag-cns) (car ang-cns) table 0 0 8 8)
(defun feature-cell (magnitude direction table x y rows cols)
  (let ((feature (make-array 9 :element-type 'single-float :initial-element 0.0)))
    (loop for i from y below (+ rows y)
          do (loop for j from x below (+ cols x)
                   do (feature-pixel (aref magnitude i j)
                                     (aref direction i j)
                                     table
                                     feature)))
    feature))

;; Step 0 : Load Image
(defparameter image (imread "../data/lena.jpg"))
;; Step 1 : Preprocessing
;; TODO: do cut thing

;; Step 2 : Calculate the Gradient Images
(defparameter fimage (convert-type image 'single-float))
(defparameter gx (sobelx1 fimage))
(defparameter gy (sobely1 fimage))
(defparameter magnitude nil)
(defparameter direction nil)
(multiple-value-bind (mag dire)
    (cart-to-polar gx gy)
  (progn (setf magnitude mag direction dire) t))
;; Step 3 : Calculate Histogram of Gradients in 8×8 cells
(defparameter magnitude-uint8 (convert-type magnitude '(unsigned-byte 8)))
(defparameter direction-uint8 (rtd-image direction))
(imshow magnitude-uint8 "magnitude")
(imshow direction-uint8 "direction")
;; calc a 9-dim feature for every 8x8 cell (no overlapping)
(feature-cell magnitude-uint8 direction-uint8 table 0 0 8 8)

(destroy-all-windows)
