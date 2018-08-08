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

(defparameter image (imread "../data/lena.jpg"))
;; Step 2 : Calculate the Gradient Images
(defparameter fimage (convert-type image 'single-float))
(defparameter gx (sobelx1 fimage))
(defparameter gy (sobely1 fimage))
(defparameter mag nil)
(defparameter angle nil)
(multiple-value-bind (lmag langle)
    (cart-to-polar gx gy)
  (progn (setf mag lmag angle langle) t))
(destroy-all-windows)
;; Step 3 : Calculate Histogram of Gradients in 8×8 cells
