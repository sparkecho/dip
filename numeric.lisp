(in-package #:dip)

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
