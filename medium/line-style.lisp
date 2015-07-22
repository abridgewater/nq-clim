;;;
;;; nq-clim/medium/line-style
;;;
;;; LINE-STYLE protocol class.
;;;

(cl:defpackage :nq-clim/medium/line-style
  (:use :cl)
  (:export
   "LINE-STYLE"
   "LINE-STYLE-P"))
(cl:in-package :nq-clim/medium/line-style)


(defclass line-style () ())

(defgeneric line-style-p (object))

(defmethod line-style-p ((object t))
  nil)

(defmethod line-style-p ((object line-style))
  t)

;;; EOF
