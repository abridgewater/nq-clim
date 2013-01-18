;;;
;;; nq-clim/geometry/transformation
;;;
;;; Parts of CLIM II 5.1.
;;;

(cl:defpackage :nq-clim/geometry/transformation
  (:use :cl)
  (:export
   "TRANSFORMATION"
   "TRANSFORMATIONP"))
(cl:in-package :nq-clim/geometry/transformation)


;; The TRANSFORMATION protocol class.
(defclass transformation () ())

;; The TRANSFORMATION protocol predicate.
(defun transformationp (object)
  "Return TRUE iif OBJECT is a TRANSFORMATION."
  (typep object 'transformation))


;;; EOF
