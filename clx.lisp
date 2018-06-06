(in-package #:dip)


(defmacro with-display (host (display screen root-window) &body body)
  `(let* ((,display (xlib:open-display ,host))
          (,screen (first (xlib:display-roots ,display)))
          (,root-window (xlib:screen-root ,screen)))
     (unwind-protect (progn ,@body)
       (xlib:close-display ,display))))

(defun display-image (image &key (title "") (host ""))
  (with-display host (display screen root-window)
    (let* ((window (xlib:create-window :parent root-window
                                       :x 0 :y 0
                                       :width (xlib:image-width image)
                                       :height (xlib:image-height image)
                                       :event-mask (xlib:make-event-mask
                                                    :exposure
                                                    :key-press
                                                    :button-press)))
           (gcontext (xlib:create-gcontext :drawable window)))
      (setf (xlib:wm-name window) title)
      (xlib:map-window window)
      (unwind-protect
           (handler-case
               (xlib:event-case (display :force-output-p t :discard-p t)
                 ;; (:exposure () (xlib:put-image window gcontext image :x 0 :y 0))
                 ;; (t () t))
                 (:key-press () t)
                 (t () (xlib:put-image window gcontext image :x 0 :y 0)))
             (end-of-file ()))
        (xlib:destroy-window window)))))

;; Create xlib-image from dip-image (2d or 3d array)
(defun make-xlib-image (image)
  (let ((rgba (ecase (channels image)   ;support for grayscale image is needed
                (3 (rgb->rgba image))   ;still need to find out if it's possible to avoid this conversion
                (4 image))))
    (xlib:create-image :width (width image)
                       :height (height image)
                       :depth 24
                       :data (array-storage-vector (rgba->bgra rgba))
                       :bits-per-pixel 32)))

(defun imshow-clx (image title)
  (display-image (make-xlib-image image) :title title))


;;;; Other functions (maybe useful in the future)

(defun save-image-as-png (image path)
  (let* ((width (xlib:image-width image))
         (height (xlib:image-height image))
         (png (make-instance 'zpng:png
                             :width width
                             :height height
                             :color-type :truecolor-alpha
                             :image-data (xlib::image-x-data image)))
         (data (zpng:data-array png)))
    (dotimes (y height (zpng:write-png png path))
      (dotimes (x width)
        (rotatef (aref data y x 0) (aref data y x 2)) ; BGR -> RGB
        (setf (aref data y x 3) 255)))))


(defun take-screenshot (&optional (host ""))
  (with-display host (display screen root-window)
    (xlib:get-image root-window
                    :x 0
                    :y 0
                    :width (xlib:screen-width screen)
                    :height (xlib:screen-height screen))))
