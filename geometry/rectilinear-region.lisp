;;;
;;; nq-clim/geometry/rectilinear-region.lisp
;;;
;;; Regions composed entirely of rectangular subregions.
;;;

(cl:defpackage :nq-clim/geometry/rectilinear-region
  (:use :cl
        :nq-clim/geometry/bounding-rectangle-protocol
        :nq-clim/geometry/region)
  (:export
   "RECTILINEAR-REGION"
   "REGION-Y-SPANS"))
(cl:in-package :nq-clim/geometry/rectilinear-region)


(defclass rectilinear-region (area bounding-rectangle) ())

(defgeneric region-y-spans (region)
  (:documentation
   "Return the ``Y-SPANS'' or ``SHAPE'' of the region."))

;;; EOF
