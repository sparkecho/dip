;;;; dip.asd

(asdf:defsystem #:dip
  :description "Digital image processing library"
  :author "sparkecho"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :depends-on (cl-jpeg trivial-main-thread cl-glfw3 cl-opengl)
  :components ((:file "package")
               (:file "dip")))
