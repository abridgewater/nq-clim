;;;
;;; nq-clim/frame/application-frame
;;;
;;; The APPLICATION-FRAME protocol class (per CLIM II 28.2).
;;;

(cl:defpackage :nq-clim/frame/application-frame
  (:use :cl)
  (:export
   "APPLICATION-FRAME"
   "APPLICATION-FRAME-P"))
(cl:in-package :nq-clim/frame/application-frame)

;; The protocol class.  Users may subclass this to provide other
;; objects that behave as application-frames.
(defclass application-frame () ())

;; The protocol predicate.
(defun application-frame-p (object)
  "Return TRUE iif OBJECT is a APPLICATION-FRAME."
  (typep object 'application-frame))

;;; EOF
