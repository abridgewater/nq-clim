;;;
;;; nq-clim/medium/graphics-method
;;;
;;; CLIM II 12.5, and supporting logic
;;;

(cl:defpackage :nq-clim/medium/graphics-method
  (:use :cl)
  (:export
   "DEFINE-GRAPHICS-METHOD"))
(cl:in-package :nq-clim/medium/graphics-method)


(defmacro define-graphics-method (name internal-fun fixed-args)
  `(defun ,name (medium ,@fixed-args &rest drawing-options)
     (flet ((thunk ()
              (,internal-fun medium ,@fixed-args)))
       (declare (dynamic-extent #'thunk))
       (apply #'invoke-with-drawing-options
              medium #'thunk drawing-options))))


;;; EOF
