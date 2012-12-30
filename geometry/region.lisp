;;;
;;; nq-clim/geometry/region
;;;
;;; Parts of CLIM II 3.1.
;;;

(cl:defpackage :nq-clim/geometry/region
  (:use :cl
        :nq-clim/geometry/bounding-rectangle-protocol)
  (:export
   "REGION"
   "REGIONP"
   "AREA"
   "AREAP"))
(cl:in-package :nq-clim/geometry/region)


;; The REGION protocol class.
(defclass region () ())

;; The REGION protocol predicate.
(defun regionp (object)
  "Return TRUE iif OBJECT is a REGION."
  (typep object 'region))

;; The AREA protocol class.
(defclass area (region bounding-rectangle) ())

;; The AREA protocol predicate.
(defun areap (object)
  "Return TRUE iif OBJECT is an AREA."
  (typep object 'area))

;;; EOF
