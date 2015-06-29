;;;
;;; nq-clim/ink/color
;;;
;;; COLOR protocol class.
;;;

(cl:defpackage :nq-clim/ink/color
  (:use :cl)
  (:export
   "COLOR"
   "COLOR-RGB"
   "COLORP"))
(cl:in-package :nq-clim/ink/color)


(defclass color () ())

(defgeneric colorp (object)
  (:documentation "TRUE if OBJECT is a COLOR, otherwise FALSE")
  (:method ((object t)) nil)
  (:method ((object color)) t))


(defgeneric color-rgb (color)
  (:documentation "The RED, GREEN, and BLUE components of COLOR, as values between 0 and 1, inclusive"))


;;; EOF
