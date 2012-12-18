;;;
;;; nq-clim/backend/clx/medium
;;;
;;; CLX medium implementation
;;;

(cl:defpackage :nq-clim/backend/clx/medium
  (:use :cl
        :nq-clim/medium/drawing
        :nq-clim/medium/basic-medium)
  (:export
   "CLX-MEDIUM"
   "MAKE-CLX-MEDIUM"))
(cl:in-package :nq-clim/backend/clx/medium)


(defclass clx-medium (basic-medium)
  ((drawable :initarg drawable)
   (gcontext :initarg gcontext)))

(defun make-clx-medium (drawable gcontext)
  (make-instance 'clx-medium 'drawable drawable 'gcontext gcontext))


(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-line drawable gcontext x1 y1 x2 y2)))

;;; EOF
