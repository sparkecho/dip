(in-package #:dip)


;;;; imshow sdl2 backend
(defun imshow-sdl2 (image title)
  (let* ((width (width image))
         (height (height image))
         (vector (array-storage-vector image))
         (surface (sdl2:create-rgb-surface width height 24)) ;replace 24
         (pixels (sdl2:surface-pixels surface)))
    (dotimes (i (length vector))
      (setf (cffi:mem-aref pixels :uint8 i) (aref vector i)))
    (sdl2:with-init (:video)
                    (sdl2:with-window (window :title title :w width :h height :flags '(:shown))
                      (let ((screen-surface (sdl2:get-window-surface window)))
                        (sdl2:blit-surface surface nil screen-surface nil)
                        (sdl2:update-window window)
                        (sdl2:with-event-loop (:method :poll)
                          (:quit () t)
                          (:idle ()
                                 (sdl2:delay 100))))))))
