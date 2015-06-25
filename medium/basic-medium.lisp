;;;
;;; nq-clim/medium/basic-medium
;;;
;;; The "abstract" medium class dealing with "user transformations".
;;;

(cl:defpackage :nq-clim/medium/basic-medium
  (:use :cl
        :nq-clim/medium/association
        :nq-clim/medium/drawing
        :nq-clim/medium/medium)
  (:export
   "BASIC-MEDIUM"))
(cl:in-package :nq-clim/medium/basic-medium)


(defclass basic-medium (medium)
  ;; FIXME: This is the hook for "user" transformations.
  ((sheet :initform nil :reader medium-sheet)))


(defmethod engraft-medium ((medium basic-medium) port sheet)
  ;; FIXME: Set medium properties from sheet defaults.
  (setf (slot-value medium 'sheet) sheet))

(defmethod degraft-medium ((medium basic-medium) port sheet)
  (setf (slot-value medium 'sheet) nil))

#+(or)
(defmethod medium-draw-line* :around ((medium basic-medium) x1 y1 x2 y2)
  ;; FIXME: Transform the points (x1,y1) and (x2,y2), then
  ;; call-next-method with the transformed points.
  )

;;; EOF
