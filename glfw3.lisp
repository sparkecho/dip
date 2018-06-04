(in-package #:dip)

;; (defvar *raw-vertices* #(-1.0  1.0 0.0 0.0 1.0 ;tl
;;                          1.0   1.0 0.0 1.0 1.0 ;tr
;;                          -1.0 -1.0 0.0 0.0 0.0 ;bl
;;                          1.0  -1.0 0.0 1.0 0.0)) ;br
(defvar *raw-vertices* #(-1.0  1.0 0.0 0.0 0.0 ;tl
                         1.0   1.0 0.0 1.0 0.0 ;tr
                         -1.0 -1.0 0.0 0.0 1.0 ;bl
                         1.0  -1.0 0.0 1.0 1.0)) ;br
(defvar *raw-indices* #(0 1 2
                        1 2 3))
;;;; cl-glfw3/examples/fragment-shader.lisp
;;;; cl-opengl/examples/misc/shader-vao.lisp
;;;; cl-opengl/examples/misc/render-to-texture.lisp
(defun imshow-glfw3 (image title)
  (let ((height (height image))
        (width (width image)))
    (glfw:with-init-window (:title title :width width :height height
                                   :context-version-major 3
                                   :context-version-minor 3
                                   :opengl-profile :opengl-core-profile)
      (setf cl-opengl-bindings:*gl-get-proc-address* #'glfw:get-proc-address)
      (let ((vao (gl:gen-vertex-array))
            (vbo (gl:gen-buffer))
            (ebo (gl:gen-buffer))
            (texture (gl:gen-texture)))
        ;; Setting of data
        (gl:bind-buffer :array-buffer vbo) ;bind vbo
        (let* ((len (length *raw-vertices*))
               (vert (gl:alloc-gl-array :float len)))
          (dotimes (i len)
            (setf (gl:glaref vert i) (aref *raw-vertices* i)))
          (gl:buffer-data :array-buffer :static-draw vert)
          (gl:free-gl-array vert))
        (gl:bind-buffer :array-buffer 0) ;unbind vbo

        (gl:bind-buffer :element-array-buffer ebo) ;bind ebo
        (let* ((len (length *raw-indices*))
               (index (gl:alloc-gl-array :unsigned-short len)))
          (dotimes (i len)
            (setf (gl:glaref index i) (aref *raw-indices* i)))
          (gl:buffer-data :element-array-buffer :static-draw index)
          (gl:free-gl-array index))
        (gl:bind-buffer :element-array-buffer 0) ;unbind ebo

        (gl:bind-vertex-array vao)      ;bind vao
        (gl:bind-buffer :array-buffer vbo)
        (gl:enable-vertex-attrib-array 0)
        (gl:enable-vertex-attrib-array 1)
        (gl:vertex-attrib-pointer 0 3 :float nil
                                  (* 5 (cffi:foreign-type-size :float))
                                  0)    ;(cffi:null-pointer)
        (gl:vertex-attrib-pointer 1 2 :float nil
                                  (* 5 (cffi:foreign-type-size :float))
                                  (* 3 (cffi:foreign-type-size :float))) ;(cffi:make-pointer ...)
        (gl:bind-buffer :element-array-buffer ebo)
        (gl:bind-vertex-array 0)    ;unbind vao

        ;; Setting of texture
        (gl:bind-texture :texture-2d texture)
        (gl:tex-parameter :texture-2d :texture-min-filter :linear)
        (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
        ;; (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :bgr ;show BGR image correctly
        (gl:tex-image-2d :texture-2d 0 :rgb width height 0
                         #+little-endian :rgb #+big-endian :bgr
                         :unsigned-byte (array-storage-vector image) :raw t)
        (gl:generate-mipmap :texture-2d)
        (gl:bind-texture :texture-2d 0)

        ;; Set viewport
        (gl:viewport 0 0 width height)
        ;; Compile shaders and use the program
        (setup-shader)
        ;; Render
        (render vao texture)
        ;; Show on screen
        (glfw:swap-buffers)
        (loop until (glfw:window-should-close-p)
           do (glfw:poll-events))))))


(defparameter +vertex-shader-source+ "
#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 TexCoord;

void main()
{
    gl_Position = vec4(aPos, 1.0);
    TexCoord = aTexCoord;
}
")

(defparameter +fragment-shader-source+ "
#version 330 core
out vec4 FragColor;
in vec2 TexCoord;

uniform sampler2D Texture;

void main()
{
    FragColor = texture(Texture, TexCoord);
}
")

(defvar *shader-prog* -1)
(defvar *frag-shader* nil)
(defvar *vert-shader* nil)

(defun render (vao texture)
  (gl:clear-color 0.2 0.3 0.3 1.0)
  (gl:clear :color-buffer-bit)
  (gl:bind-texture :texture-2d texture)
  (gl:bind-vertex-array vao)
  (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)
  (gl:use-program *shader-prog*)
  )

;; Error handling for shader compilation events
(define-condition compile-error (error)
  ((message
    :initform nil
    :initarg :message
    :reader compile-error-message
    :documentation "The reason given for the error")))

(defun check-shader-error (shader)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (progn
        ;; Print to console & then throw error
        (format t "~A~%" error-string)
        (error 'compile-error :message error-string)))))

(defun is-invalid-shader (shader)
  (= shader -1))

(defun setup-shader ()
  (setf *shader-prog* -1)
  ;; Keep trying to load our shader (Allow user to fix compile errors)
  (loop while (is-invalid-shader *shader-prog*)
     do (with-simple-restart (retry "Retry compiling shaders.")

          ;; Create the OpenGL shader in memory
          (setf *vert-shader* (gl:create-shader :vertex-shader))
          (setf *frag-shader* (gl:create-shader :fragment-shader))

          ;; Copy shader source to the OpenGL shader
          (gl:shader-source *vert-shader* +vertex-shader-source+)
          (gl:shader-source *frag-shader* +fragment-shader-source+)

          ;; Compile shader sources into GPU bytecode
          (gl:compile-shader *vert-shader*)
          (gl:compile-shader *frag-shader*)

          (check-shader-error *vert-shader*)
          (check-shader-error *frag-shader*)

          ;; Create the program which controls the shader activation
          (setf *shader-prog* (gl:create-program))

          ;; Then add shaders to that program
          ;; The same shader can be attached to different programs
          (gl:attach-shader *shader-prog* *vert-shader*)
          (gl:attach-shader *shader-prog* *frag-shader*)

          ;; Linking shader puts everything together
          (gl:link-program *shader-prog*)

          ;; This shader will be used to draw in the future
          (gl:use-program *shader-prog*))))
