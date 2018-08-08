(in-package :dip)

(defvar *type-info*)
(setf *type-info* (make-hash-table))

(defmacro register-type (name &body body)
  `(let ((base-type (car ,@body)))
     (when (eql base-type 'float)
       (error "Invalid base type use short-float, single-float or double-float."))
     (deftype ,name () ,@body)
     (setf (gethash ',name *type-info*) ,@body)
     ',name))

(defun clear-type (name)
  (remhash name *type-info*))

(defun type-low (type)
  (second (gethash type *type-info*)))

(defun type-high (type)
  (third (gethash type *type-info*)))

(register-type uint8 '(integer 0 255))
(register-type uint16 '(integer 0 65535))
(register-type int8 '(integer -128 127))
(register-type int16 '(integer -32768 32767))
(register-type single `(single-float ,most-negative-single-float
                                     ,most-positive-single-float))
(register-type double `(double-float ,most-negative-double-float
                                     ,most-positive-double-float))
