;;;
;;; nq-clim/medium/basic-medium
;;;
;;; The "abstract" medium class dealing with "user transformations".
;;;

(cl:defpackage :nq-clim/medium/basic-medium
  (:use :cl
        :nq-clim/medium/medium
        :nq-clim/medium/drawing)
  (:export
   "BASIC-MEDIUM"))
(cl:in-package :nq-clim/medium/basic-medium)


(defclass basic-medium (medium)
  ;; FIXME: This is the hook for "user" transformations.
  ())

#+(or)
(defmethod medium-draw-line* :around ((medium basic-medium) x1 y1 x2 y2)
  ;; FIXME: Transform the points (x1,y1) and (x2,y2), then
  ;; call-next-method with the transformed points.
  )

;;; EOF
