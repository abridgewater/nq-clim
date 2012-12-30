;;;
;;; nq-clim/sheet/sheet
;;;
;;; The SHEET protocol class (most of CLIM II 7.1).
;;;

(cl:defpackage :nq-clim/sheet/sheet
  (:use :cl)
  (:export
   "SHEET"
   "SHEETP"))
(cl:in-package :nq-clim/sheet/sheet)

;; The protocol class.  Users may subclass this to provide other
;; objects that behave as sheets.
(defclass sheet () ())

;; The protocol predicate.
(defun sheetp (object)
  "Return TRUE iif OBJECT is a SHEET."
  (typep object 'sheet))

;;; EOF
