(in-package #:dip)

;;;; After solved some clx issue (e.g. must have a rgb->bgra conversion now)
;;;; And add support of win32-api
;;;; Change definition of *default-backend* to:
;;;; (defparameter *default-backend* #+unix :clx #+windows :win32 #+darwin :glfw3)
(defparameter *default-backend* :glfw3)

(defun imshow (image &optional (title "") (backend *default-backend*))
  (case backend
    (:glfw3 (imshow-glfw3 image title))
    (:sdl2 (imshow-sdl2 image title))
    (:clx (imshow-clx image title))))
