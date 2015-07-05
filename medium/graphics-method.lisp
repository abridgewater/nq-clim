;;;
;;; nq-clim/medium/graphics-method
;;;
;;; CLIM II 12.5, and supporting logic
;;;

(cl:defpackage :nq-clim/medium/graphics-method
  (:use :cl
        :nq-clim/medium/drawing
        :nq-clim/medium/drawing-options)
  (:export
   "DEFINE-GRAPHICS-METHOD"
   "DRAW-POINT*"
   "DRAW-POINTS*"
   "DRAW-LINE*"
   "DRAW-LINES*"
   "DRAW-POLYGON*"
   "DRAW-RECTANGLE*"
   "DRAW-RECTANGLES*"))
(cl:in-package :nq-clim/medium/graphics-method)


(defmacro define-graphics-method (name internal-fun fixed-args &optional key-args allowed-options)
  (declare (ignore key-args allowed-options))
  `(defun ,name (medium ,@fixed-args &rest drawing-options)
     (flet ((thunk ()
              (,internal-fun medium ,@fixed-args)))
       (declare (dynamic-extent #'thunk))
       (apply #'invoke-with-drawing-options
              medium #'thunk drawing-options))))


(define-graphics-method draw-point*
    medium-draw-point* (x y))
(define-graphics-method draw-points*
    medium-draw-points* (coord-seq))
(define-graphics-method draw-line*
    medium-draw-line* (x1 y1 x2 y2))
(define-graphics-method draw-lines*
    medium-draw-lines* (coord-seq))

 ;; FIXME: The CLOSED and FILLED arguments below are supposed to be
 ;; keywords.  Or aren't supposed to be here at all in the case of
 ;; DRAW-RECTANGLES* But they aren't specified as parameters at all
 ;; for the corresponding MEDIUM- functions in the spec, nor are they
 ;; specified as medium properties...  And it turns out that some
 ;; other drawing functions that we'll need later require other
 ;; keyword arguments.
(define-graphics-method draw-polygon*
    medium-draw-polygon* (coord-seq closed filled))
(define-graphics-method draw-rectangle*
    medium-draw-rectangle* (x1 y1 x2 y2 filled))
(define-graphics-method draw-rectangles*
    medium-draw-rectangles* (coord-seq filled))

;;; EOF
