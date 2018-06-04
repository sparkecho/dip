(in-package #:dip)

(defun imread-png (filename)
  (pngload:data (pngload:load-file filename)))

(defun imwrite-png (filename image)
  (let ((color-type (ecase (channels image)
                      (1 :grayscale)
                      (3 :truecolor)
                      (4 :truecolor-alpha))))
    (zpng:write-png (make-instance 'zpng:png
                                   :color-type color-type
                                   :height (height image)
                                   :width (width image)
                                   :bpp (second (array-element-type image))
                                   :image-data (array-storage-vector image))
                    filename)))
