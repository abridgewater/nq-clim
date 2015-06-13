;;;
;;; nq-clim/geometry/region-set
;;;
;;; Parts of CLIM II 3.1.2.
;;;

(cl:defpackage :nq-clim/geometry/region-set
  (:use :cl
        :nq-clim/geometry/region
        :nq-clim/geometry/bounding-rectangle-protocol)
  (:export
   "REGION-SET"
   "REGION-SET-P"))
(cl:in-package :nq-clim/geometry/region-set)


;; The REGION-SET protocol class.
(defclass region-set (region bounding-rectangle) ())

;; The REGION-SET-P protocol predicate.
(defun region-set-p (object)
  "Return TRUE iif OBJECT is a REGION-SET."
  (typep object 'region-set))

;;; EOF
