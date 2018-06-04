;;;; dip.asd

(asdf:defsystem #:dip
  :description "Digital image processing library"
  :author "sparkecho"
  :license  "MIT License"
  :version "0.0.1"
  :depends-on (alexandria trivial-main-thread cl-jpeg cl-opengl cl-glfw3 sdl2)
  :components ((:file "package")
               (:file "image" :depends-on ("package"))
               ;; io module
               (:file "jpeg"  :depends-on ("image"))
               (:file "io"    :depends-on ("jpeg"))
               ;; gui module
               (:file "glfw3" :depends-on ("image"))
               (:file "sdl2"  :depends-on ("image"))
               (:file "gui"   :depends-on ("glfw3" "sdl2"))
               ;; dip module
               (:file "dip"   :depends-on ("image"))))
