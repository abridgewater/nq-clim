;;;
;;; nq-clim/geometry/transformation-protocol
;;;
;;; Parts of CLIM II 5.1 and CLIM II 5.3.
;;;

(cl:defpackage :nq-clim/geometry/transformation-protocol
  (:use :cl
        :nq-clim/geometry/transformation)
  (:export
   "TRANSFORMATION-ERROR"
   "SINGULAR-TRANSFORMATION"
   "TRANSFORMATION-EQUAL"
   "IDENTITY-TRANSFORMATION-P"
   "INVERTIBLE-TRANSFORMATION-P"
   "TRANSLATION-TRANSFORMATION-P"
   "REFLECTION-TRANSFORMATION-P"
   "RIGID-TRANSFORMATION-P"
   "EVEN-SCALING-TRANSFORMATION-P"
   "SCALING-TRANSFORMATION-P"
   "RECTILINEAR-TRANSFORMATION-P"
   "COMPOSE-TRANSFORMATIONS"
   "INVERT-TRANSFORMATION"
   "TRANSFORM-REGION"
   "UNTRANSFORM-REGION"
   "TRANSFORM-POSITION"
   "UNTRANSFORM-POSITION"
   "TRANSFORM-DISTANCE"
   "UNTRANSFORM-DISTANCE"
   "TRANSFORM-RECTANGLE*"
   "UNTRANSFORM-RECTANGLE*"))
(cl:in-package :nq-clim/geometry/transformation-protocol)

(define-condition transformation-error (error) ())
(define-condition singular-transformation (transformation-error)
  ((transformation :initarg :transformation)))

(defgeneric transformation-equal (transformation1 transformation2))

(defgeneric identity-transformation-p (transformation)
  (:documentation "Returns true if TRANSFORMATION has no effect on its inputs."))
(defgeneric invertible-transformation-p (transformation)
  (:documentation "Returns true if TRANSFORMATION can be inverted (is not singular)."))
(defgeneric translation-transformation-p (transformation)
  (:documentation "Returns true if TRANSFORMATION is a pure translation (for each point x,y, transforms to x+dx,y+dy for some constant dx,dy)."))
(defgeneric reflection-transformation-p (transformation)
  (:documentation "Returns true if TRANSFORMATION affects the \"handedness\" of the coordinate system (causes angles to be measured in the opposite direction)."))
(defgeneric rigid-transformation-p (transformation)
  (:documentation "Returns true if TRANSFORMATION preserves magnitudes of all angles and distances (that is, is translations and rotations only, no scaling, no skewing)."))
(defgeneric even-scaling-transformation-p (transformation)
  (:documentation "Returns true if TRANSFORMATION is a scaling transformation that affects both axes equally."))
(defgeneric scaling-transformation-p (transformation)
  (:documentation "Returns true if TRANSFORMATION scales each axis independently by some factor."))
(defgeneric rectilinear-transformation-p (transformation)
  (:documentation "Returns true if TRANSFORMATION returns an axis-aligned rectangle when given an axis-aligned rectangle."))

(defgeneric compose-transformations (transformation1 transformation2))

(defgeneric invert-transformation (transformation)
  (:documentation "Returns a transformation that \"undoes\" the effect of TRANSFORMATION, or signals SINGULAR-TRANSFORMATION if this cannot be done."))

(defgeneric transform-region (transformation region))
(defgeneric untransform-region (transformation region)
  (:method ((transformation transformation) region)
    (transform-region (invert-transformation transformation) region)))

(defgeneric transform-position (transformation x y))
(defgeneric untransform-position (transformation x y)
  (:method ((transformation transformation) x y)
    (transform-position (invert-transformation transformation) x y)))

(defgeneric transform-distance (transformation dx dy))
(defgeneric untransform-distance (transformation dx dy)
  (:method ((transformation transformation) dx dy)
    (transform-distance (invert-transformation transformation) dx dy)))

(defgeneric transform-rectangle* (transformation x1 y1 x2 y2))
(defgeneric untransform-rectangle* (transformation x1 y1 x2 y2)
  (:method ((transformation transformation) x1 y1 x2 y2)
    (transform-rectangle* (invert-transformation transformation) x1 y1 x2 y2)))

;;; EOF
