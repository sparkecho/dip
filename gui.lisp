(in-package #:dip)

(defparameter *default-backend* :glfw3)

(defun imshow (image &optional (title "") (backend *default-backend*))
  (case backend
    (:glfw3 (imshow-glfw3 image title))
    (:sdl2 (imshow-sdl2 image title))))
