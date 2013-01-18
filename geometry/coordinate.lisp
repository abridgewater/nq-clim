;;;
;;; nq-clim/geometry/coordinate
;;;
;;; The COORDINATE type and coercion function (CLIM II 3.1)
;;;

(cl:defpackage :nq-clim/geometry/coordinate
  (:use :cl)
  (:export
   "COORDINATE"))
(cl:in-package :nq-clim/geometry/coordinate)

(deftype coordinate ()
  'integer)

(defun coordinate (n)
  (coerce n 'integer))

;;; EOF
