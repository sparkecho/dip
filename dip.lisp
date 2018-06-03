;;;; dip.lisp

(in-package #:dip)


(declaim (inline array-storage-vector))
(defun array-storage-vector (array)
  #+sbcl (sb-ext:array-storage-vector array))

(defun make-image (rows cols channels data)
  (let* ((dimensions (cond ((= channels 1) (list rows cols))
                          ((>= channels 3) (list rows cols 3))))
         (image (make-array dimensions :element-type (array-element-type data)))
         (vector (array-storage-vector image)))
    (if (<= channels 3)
        (loop for i below (array-total-size image)
           do (setf (aref vector i) (aref data i)))
        (loop for i below (array-total-size image) by channels
           do (setf (aref vector i) (aref data i)
                    (aref vector (+ i 1)) (aref data (+ i 1))
                    (aref vector (+ i 2)) (aref data (+ i 2)))))
    image))

(defun imread (filename)
  (multiple-value-bind (buffer height width ncomp)
      (cl-jpeg:decode-image filename)
    (make-image height width ncomp buffer)))


(declaim (inline height width rows cols channels))

(defun height (image)
  (array-dimension image 0))

(defun width (image)
  (array-dimension image 1))

(defun rows (image)
  (array-dimension image 0))

(defun cols (image)
  (array-dimension image 1))

(defun channels (image)
  (if (= (array-rank image) 2)
      1
      (array-dimension image 2)))

;; opticl/jpeg.lisp
;; (defparameter *rgb-sampling* '((1 1)(1 1)(1 1)))
;; (defparameter *rgb-q-tabs* (vector jpeg::+q-luminance-hi+
;;                                    jpeg::+q-chrominance-hi+))
;; (defparameter *gray-q-tabs* (vector jpeg::+q-luminance+))
(defun imwrite (filename image)
  (let ((ncomp (channels image)))
    (jpeg:encode-image filename (array-storage-vector image)
                       ncomp (height image) (width image))))

(defparameter *gui-backend* :glfw3)

(defun imshow (image &optional (title "") (backend *gui-backend*))
  (case backend
    (:glfw3 (imshow-glfw3 image title))
    (:sdl2 (imshow-sdl2 image title))))


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
        (gl:tex-image-2d :texture-2d 0 :rgb width height 0 :rgb
                         :unsigned-byte (array-storage-vector image) :raw t)
        (gl:generate-mipmap :texture-2d)
        (gl:bind-texture :texture-2d 0)

        ;; Set viewport
        (gl:viewport 0 0 width height)
        ;; Compile shaders and use the program
        (setup-shader)

        ;; render loop
        (loop until (glfw:window-should-close-p)
           do (render vao texture)
           do (glfw:swap-buffers)
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


;;;; imshow with sdl2 as backend
(defun imshow-sdl2 (image title)
  (declare (ignore image title)))



;;;; Utilities
(defun array3d->2d (image)
  (let* ((newimage (make-array (list (rows image) (* (cols image) (channels image)))
                               :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector newimage))
         (size (array-total-size image)))
    (loop for i below size
       do (setf (aref dst i) (aref src i)))
    newimage))

(defun array2d->3d (image channels)
  (assert (zerop (mod (cols image) channels)))
  (let* ((newimage (make-array (list (rows image) (/ (cols image) channels) channels)
                               :element-type (array-element-type image)))
         (src (array-storage-vector image))
         (dst (array-storage-vector newimage))
         (size (array-total-size image)))
    (loop for i below size
       do (setf (aref dst i) (aref src i)))
    newimage))

(defun copy (image)
  (let ((newimage (make-array (array-dimensions image)
                              :element-type (array-element-type image))))
    (let ((src (array-storage-vector image))
          (dst (array-storage-vector newimage)))
      (loop for i below (array-total-size image)
         do (setf (aref dst i) (aref src i))))
    newimage))

(defun bgr<->rgb (image)
  (let ((vector (array-storage-vector image)))
    (loop for i below (array-dimension vector 0) by 3
       do (rotatef (aref vector i) (aref vector (+ i 2))))
    image))

(defun convert (image method)
  (ecase method
    (:rgb->bgr (bgr<->rgb image))
    (:bgr->rgb (bgr<->rgb image))))

(defun flip (image code)
  (ecase code
    ((:h :x 0) (flip-horizontal image))
    ((:v :y 1) (flip-vertical image))
    ((:hv :xy -1) (flip-both image))))

(declaim (inline flip-vertical flip-horizontal flip-both))
(defun flip-vertical (image)
  (let ((rows (rows image))
        (cols (cols image))
        (channels (channels image)))
    (if (= channels 1)
        (loop for i below (truncate rows 2)
           with sum = (- rows 1)
           do (loop for j below cols
                 do (rotatef (aref image i j) (aref image (- sum i) j))))
        (loop for i below (truncate rows 2)
           with sum = (- rows 1)
           do (loop for j below cols
                 do (loop for k below channels
                       do (rotatef (aref image i j k) (aref image (- sum i) j k)))))))
  image)

(defun flip-horizontal (image)
  (let ((rows (rows image))
        (cols (cols image))
        (channels (channels image)))
    (if (= channels 1)
        (loop for i below rows
           with sum = (- cols 1)
           do (loop for j below (truncate cols 2)
                 do (rotatef (aref image i j) (aref image i (- sum j)))))
        (loop for i below rows
           with sum = (- cols 1)
           do (loop for j below (truncate cols 2)
                 do (loop for k below channels
                       do (rotatef (aref image i j k) (aref image i (- sum j) k)))))))
  image)

(defun flip-both (image)
  (let ((height (height image))
        (width (width image))
        (channels (channels image)))
    (if (= channels 1)
        (progn
          ;; tl <-> br
          (loop for y to (truncate height 2)
             with sumy = (- height 1)
             with sumx = (- width 1)
             do (loop for x below (truncate width 2)
                   do (rotatef (aref image y x) (aref image (- sumy y) (- sumx x)))))
          ;; tr <-> bl
          (loop for y below (truncate height 2)
             with sumy = (- height 1)
             with sumx = (- width 1)
             do (loop for x from (truncate width 2) below width
                   do (rotatef (aref image y x) (aref image (- sumy y) (- sumx x))))))
        (progn
          ;; tl <-> br
          (loop for y to (truncate height 2)
             with sumy = (- height 1)
             with sumx = (- width 1)
             do (loop for x below (truncate width 2)
                   do (loop for c below channels
                         do (rotatef (aref image y x c) (aref image (- sumy y) (- sumx x) c)))))
          ;; tr <-> bl
          (loop for y below (truncate height 2)
             with sumy = (- height 1)
             with sumx = (- width 1)
             do (loop for x from (truncate width 2) below width
                   do (loop for c below channels
                         do (rotatef (aref image y x c) (aref image (- sumy y) (- sumx x) c))))))))
  image)

(defun rotate (image angle)
  (declare (ignore image angle)))
