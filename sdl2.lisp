(in-package #:dip)


(defparameter *window-pool* (make-hash-table :test #'equalp))
(defparameter *surface-pool* (make-hash-table :test #'equalp))

(declaim (inline get-window put-window))

(defun get-window (title width height)
  (let ((window (gethash title *window-pool*)))
    (when window
      (sdl2:set-window-size window width height)
      window)))

(defun put-window (title window)
  (setf (gethash title *window-pool*) window))

(declaim (inline get-surface put-surface))

(defun get-surface (title width height)
  (let ((surface (gethash title *surface-pool*)))
    (when surface
      (if (and (= (sdl2:surface-width surface) width)
               (= (sdl2:surface-height surface) height))
          surface
          (and (sdl2:free-surface surface)
               (setf (gethash title *surface-pool*) nil))))))

(defun put-surface (title surface)
  (setf (gethash title *surface-pool*) surface))

(defun list-all-windows ()
  (loop for title being the hash-key in *window-pool*
          using (hash-value window)
        collect (list title window)))

(flet ((%destroy-window (title window)
         (sdl2:destroy-window window)
         (remhash title *window-pool*))
       (%free-surface (title surface)
         (sdl2:free-surface surface)
         (remhash title *surface-pool*)))

  (defun destroy-window (title)
    (let ((surface (gethash title *surface-pool*))
          (window (gethash title *window-pool*)))
      (when surface
        (%free-surface title surface))
      (when window
        (%destroy-window title window))))

  (defun destroy-all-windows ()
    (maphash #'%free-surface *surface-pool*)
    (maphash #'%destroy-window *window-pool*)))

;;;; imshow sdl2 backend
(defun imshow-sdl2 (image title)
  (let* ((width (width image))
         (height (height image))
         (vector (array-storage-vector image))
         (surface (or (get-surface title width height)
                      (put-surface title
                                   ;; Display RGBA image
                                   ;; #+big-endian
                                   ;; (sdl2:create-rgb-surface width height 32)
                                   ;; #+little-endian
                                   ;; (sdl2:create-rgb-surface width height 32
                                   ;;                          :a-mask #xff000000
                                   ;;                          :b-mask #x00ff0000
                                   ;;                          :g-mask #x0000ff00
                                   ;;                          :r-mask #x000000ff))

                                   ;; Display RGB image
                                   #+big-endian
                                   (sdl2:create-rgb-surface width height 24)
                                   #+little-endian
                                   (sdl2:create-rgb-surface width height 24
                                                            :b-mask #xff0000
                                                            :g-mask #x00ff00
                                                            :r-mask #x0000ff))))
         (pixels (sdl2:surface-pixels surface)))
    (dotimes (i (length vector))
      (setf (cffi:mem-aref pixels :uint8 i) (aref vector i)))
    (when (null (sdl2:was-init))
      (sdl2:init :video))
    (let* ((window (or (get-window title width height)
                       (put-window title
                                   (sdl2:create-window :title title
                                                       :w width
                                                       :h height
                                                       :flags '(:shown)))))
           (screen-surface (sdl2:get-window-surface window)))
      (sdl2:blit-surface surface nil screen-surface nil)
      (sdl2:update-window window)
      ;; (sdl2:with-event-loop (:method :poll)
      ;;   (:quit () t)
      ;;   (:idle ()
      ;;          (sdl2:delay 100))))))
)))

;;;; Live coding

;;;; Acknowledgement:
;;;; Mainly inspired by https://github.com/nikki93/lgame/blob/master/game.lisp
;;;; Many help from Baggers's tutorials (https://github.com/cbaggers)

(defmacro with-main (&body body)
  `(sdl2:make-this-thread-main
    (lambda ()
      #+sbcl (sb-int:with-float-traps-masked (:invalid) ,@body)
      #-sbcl ,@body)))

(defun imshow-live (image title &key (delay 100))
  (with-main                            ;try displace this line
    (let* ((width (width image))
           (height (height image))
           (vector (array-storage-vector image))
           #+big-endian
           (surface (sdl2:create-rgb-surface width height 24))
           #+little-endian
           (surface (sdl2:create-rgb-surface width height 24
                                             :b-mask #xff0000
                                             :g-mask #x00ff00
                                             :r-mask #x0000ff))
           (pixels (sdl2:surface-pixels surface)))
      (sdl2:with-init (:video)
        (sdl2:with-window (window :title title :w width :h height :flags '(:shown))
          (let ((screen-surface (sdl2:get-window-surface window)))
            (sdl2:with-event-loop (:method :poll)
              (:quit () t)
              (:idle ()
                     (update-swank)
                     (dotimes (i (length vector))
                       (setf (cffi:mem-aref pixels :uint8 i) (aref vector i)))
                     (sdl2:blit-surface surface nil screen-surface nil)
                     (sdl2:update-window window)
                     (sdl2:delay delay)))))))))
