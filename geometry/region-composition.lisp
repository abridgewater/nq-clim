;;;
;;; nq-clim/geometry/region-composition
;;;
;;; Part of CLIM II 3.1.2
;;;

(cl:defpackage :nq-clim/geometry/region-composition
  (:use :cl)
  (:export
   "REGION-UNION"
   "REGION-INTERSECTION"
   "REGION-DIFFERENCE"))
(cl:in-package :nq-clim/geometry/region-composition)


(defgeneric region-union (region1 region2))
(defgeneric region-intersection (region1 region2))
(defgeneric region-difference (region1 region2))

;;; EOF
