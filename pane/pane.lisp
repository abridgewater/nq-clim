;;;
;;; nq-clim/pane/pane
;;;
;;; The PANE protocol class (per CLIM II 29.2).
;;;

(cl:defpackage :nq-clim/pane/pane
  (:use :cl)
  (:export
   "PANE"
   "PANEP"))
(cl:in-package :nq-clim/pane/pane)

;; The protocol class.  Users may subclass this to provide other
;; objects that behave as panes.
(defclass pane () ())

;; The protocol predicate.
(defun panep (object)
  "Return TRUE iif OBJECT is a PANE."
  (typep object 'pane))

;;; EOF
