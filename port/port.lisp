;;;
;;; nq-clim/port/port
;;;
;;; Part of CLIM II 9.2.
;;;

(cl:defpackage :nq-clim/port/port
  (:use :cl)
  (:export
   "PORT"
   "PORTP"))
(cl:in-package :nq-clim/port/port)

;; The protocol class.  Users may subclass this to provide other
;; objects that behave as ports.
(defclass port () ())

;; The protocol predicate.
(defun portp (object)
  (typep object 'port))

;; Several types of objects are said to "have" a port, so there's also
;; a generic reader function.
(defgeneric port (object)
  (:documentation "Return the port associated with OBJECT.  Defined to
work for sheets, mediums, and application frames.  May be NIL."))

;;; EOF
