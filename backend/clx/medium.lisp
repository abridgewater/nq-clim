;;;
;;; nq-clim/backend/clx/medium
;;;
;;; CLX medium implementation
;;;

(cl:defpackage :nq-clim/backend/clx/medium
  (:use :cl
        :nq-clim/medium/association
        :nq-clim/medium/drawing
        :nq-clim/medium/basic-medium)
  (:import-from :xlib)
  (:export
   "CLX-MEDIUM"
   "MAKE-CLX-MEDIUM"))
(cl:in-package :nq-clim/backend/clx/medium)


(defclass clx-medium (basic-medium)
  ((drawable :initarg drawable :reader medium-drawable)
   (gcontext :initarg gcontext)))

(defun make-clx-medium (drawable gcontext)
  (make-instance 'clx-medium 'drawable drawable 'gcontext gcontext))


(defmethod medium-draw-point* ((medium clx-medium) x y)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-point drawable gcontext x y)))

(defmethod medium-draw-points* ((medium clx-medium) coord-seq)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-points drawable gcontext coord-seq)))

(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-line drawable gcontext x1 y1 x2 y2)))

(defmethod medium-draw-lines* ((medium clx-medium) coord-seq)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-segments drawable gcontext coord-seq)))

(defmethod medium-draw-polygon* ((medium clx-medium) coord-seq closed filled)
  (with-slots (drawable gcontext)
      medium
    (if (and closed (not filled))
        ;; X won't close a polyline automatically, we have to use
        ;; coincident first and last points explicitly.
        (let ((first-point (subseq coord-seq 0 2)))
          (xlib:draw-lines drawable gcontext (append coord-seq first-point)))
        (xlib:draw-lines drawable gcontext coord-seq :fill-p filled))))

(defmethod medium-draw-rectangle* ((medium clx-medium) x1 y1 x2 y2 filled)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-rectangle drawable gcontext x1 y1 (- x2 x1) (- y2 y1) filled)))

(defun fix-rectangle-coord-seq-for-x (coord-seq)
  ;; XLIB:DRAW-RECTANGLES takes x, y, width, height, but we have x, y,
  ;; x+width, y+height.
  (loop
     for (x1 y1 x2 y2 . rest) on (coerce coord-seq 'list) by #'cddddr
     collect x1
     collect y1
     collect (- x2 x1)
     collect (- y2 y1)))

(defmethod medium-draw-rectangles* ((medium clx-medium) coord-seq filled)
  (with-slots (drawable gcontext)
      medium
    (let ((fixed-coord-seq (fix-rectangle-coord-seq-for-x coord-seq)))
      (xlib:draw-rectangles drawable gcontext fixed-coord-seq filled))))

;;; EOF
