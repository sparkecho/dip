;;;; dip.asd

(asdf:defsystem #:dip
  :description "Digital image processing library"
  :author "sparkecho"
  :license  "MIT License"
  :version "0.0.1"
  :depends-on (alexandria trivial-main-thread cl-jpeg pngload zpng cl-opengl cl-glfw3 sdl2 clx)
  :serial nil
  :components ((:file "package")
               (:file "numeric" :depends-on ("package"))
               (:file "image"   :depends-on ("package"))
               ;; io module
               (:file "jpeg"    :depends-on ("image"))
               (:file "png"     :depends-on ("image"))
               (:file "io"      :depends-on ("jpeg" "png"))
               ;; gui module
               (:file "live"    :depends-on ("package"))
               (:file "glfw3"   :depends-on ("image"))
               (:file "sdl2"    :depends-on ("image" "live"))
               (:file "clx"     :depends-on ("image"))
               (:file "gui"     :depends-on ("glfw3" "sdl2" "clx"))
               ;; dip module
               (:file "dip"     :depends-on ("image" "numeric"))))
