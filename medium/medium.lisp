;;;
;;; nq-clim/medium/medium
;;;
;;; MEDIUM protocol class.
;;;

(cl:defpackage :nq-clim/medium/medium
  (:use :cl)
  (:export
   "MEDIUM"
   "MEDIUMP"))
(cl:in-package :nq-clim/medium/medium)


(defclass medium () ())

(defgeneric mediump (object))

(defmethod mediump ((object t))
  nil)

(defmethod mediump ((object medium))
  t)

;;; EOF
