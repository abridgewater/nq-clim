;;;
;;; nq-clim/medium/standard-line-style
;;;
;;; The default implementation of the LINE-STYLE protocol
;;;

(cl:defpackage :nq-clim/medium/standard-line-style
  (:use :cl
        :nq-clim/medium/line-style
        :nq-clim/medium/line-style-protocol)
  (:export
   "STANDARD-LINE-STYLE"))
(cl:in-package :nq-clim/medium/standard-line-style)


(defclass standard-line-style (line-style)
  ((unit :initarg unit :reader line-style-unit)
   (thickness :initarg thickness :reader line-style-thickness)
   (joint-shape :initarg joint-shape :reader line-style-joint-shape)
   (cap-shape :initarg cap-shape :reader line-style-cap-shape)
   (dashes :initarg dashes :reader line-style-dashes)))


;;; EOF
