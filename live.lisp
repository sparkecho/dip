(in-package :dip)

(defun get-server-connection ()
  (or swank::*emacs-connection*
      (swank::default-connection)))

(defmacro continuable (&body body)
  "Allow continuing execution from errors."
  `(restart-case (progn ,@body)
     (continue () :report "Continue")))

(defun update-swank ()
  "Handle REPL requests."
  #+swank
  (continuable
    (let ((connection (get-server-connection)))
      (when connection
        (swank::handle-requests connection t)))))
