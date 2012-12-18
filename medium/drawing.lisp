;;;
;;; nq-clim/medium/drawing
;;;
;;; Medium drawing protocol functions.
;;;

(cl:defpackage :nq-clim/medium/drawing
  (:use :cl)
  (:export
   "MEDIUM-DRAW-LINE*"))
(cl:in-package :nq-clim/medium/drawing)


(defgeneric medium-draw-line* (medium x1 y1 x2 y2))

;;; EOF
