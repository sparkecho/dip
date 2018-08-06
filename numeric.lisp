(in-package #:dip)

;; get the interval of a type
;; e.g. interval of uint8 is [0 255]
(declaim (inline type-interval))
(defun type-interval (type)
  #+sbcl
  (let ((numeric-type (sb-kernel:specifier-type type)))
    (values (sb-kernel:numeric-type-low numeric-type)
            (sb-kernel:numeric-type-high numeric-type)))
  #-sbcl
  (error "only support sbcl right now"))


(declaim (inline clamp-low clamp-high clamp))

(defun clamp-low (number low)
  (if (and low (< number low))
      low
      number))

(defun clamp-high (number high)
  (if (and high (> number high))
      high
      number))

(defun clamp (number low high)
  (clamp-high (clamp-low number low) high))
