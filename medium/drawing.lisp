;;;
;;; nq-clim/medium/drawing
;;;
;;; Medium drawing protocol functions.
;;;

(cl:defpackage :nq-clim/medium/drawing
  (:use :cl)
  (:export
   "MEDIUM-DRAW-POINT*"
   "MEDIUM-DRAW-POINTS*"
   "MEDIUM-DRAW-LINE*"
   "MEDIUM-DRAW-LINES*"
   "MEDIUM-DRAW-POLYGON*"
   "MEDIUM-DRAW-RECTANGLE*"
   "MEDIUM-DRAW-RECTANGLES*"))
(cl:in-package :nq-clim/medium/drawing)


(defgeneric medium-draw-point* (medium x y))
(defgeneric medium-draw-points* (medium coord-seq))
(defgeneric medium-draw-line* (medium x1 y1 x2 y2))
(defgeneric medium-draw-lines* (medium coord-seq))
(defgeneric medium-draw-polygon* (medium coord-seq closed filled))
(defgeneric medium-draw-rectangle* (medium x1 y1 x2 y2 filled))
(defgeneric medium-draw-rectangles* (medium coord-seq filled))

;;; EOF
