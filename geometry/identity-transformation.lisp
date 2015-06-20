;;;
;;; nq-clim/geometry/identity-transformation.lisp
;;;
;;; Part of CLIM II 5.1.
;;;

(cl:defpackage :nq-clim/geometry/identity-transformation
  (:use :cl
        :nq-clim/clim-sys/named-constant-mixin
        :nq-clim/geometry/transformation
        :nq-clim/geometry/transformation-protocol)
  (:export
   "+IDENTITY-TRANSFORMATION+"))
(cl:in-package :nq-clim/geometry/identity-transformation)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass identity-transformation (transformation named-constant-mixin) ()))

(define-named-constant +identity-transformation+ identity-transformation)


(defmethod transformation-equal ((transformation1 identity-transformation)
                                 transformation2)
  (identity-transformation-p transformation2))

(defmethod identity-transformation-p ((transformation identity-transformation))
  t)

(defmethod invertible-transformation-p ((transformation identity-transformation))
  ;; The inverse of identity is identity.
  t)

(defmethod translation-transformation-p ((transformation identity-transformation))
  ;; Identity is a translation by zero in both axes.
  t)

(defmethod reflection-transformation-p ((transformation identity-transformation))
  ;; Identity does not invert "handedness".
  nil)

(defmethod rigid-transformation-p ((transformation identity-transformation))
  ;; Identity preserves magnitudes of lengths and angles.
  t)

(defmethod even-scaling-transformation-p ((transformation identity-transformation))
  ;; Identity scales both directions by the unit magnitude.
  t)

(defmethod scaling-transformation-p ((transformation identity-transformation))
  ;; Identity scales both directions by the unit magnitude.
  t)

(defmethod rectilinear-transformation-p ((transformation identity-transformation))
  ;; Identity always transforms axis-aligned rectangles to
  ;; axis-aligned rectangles.
  t)

;; Composing anything with the identity transformation leaves it
;; untouched, no matter which side of the composition it takes.
(defmethod compose-transformations ((transformation1 identity-transformation)
                                    transformation2)
  transformation2)

(defmethod compose-transformations (transformation1
                                    (transformation2 identity-transformation))
  transformation1)

(defmethod invert-transformation ((transformation identity-transformation))
  ;; Identity is its own inverse.
  transformation)

(defmethod transform-region ((transformation identity-transformation)
                             region)
  region)

(defmethod untransform-region ((transformation identity-transformation)
                               region)
  region)

(defmethod transform-position ((transformation identity-transformation)
                               x y)
  (values x y))

(defmethod untransform-position ((transformation identity-transformation)
                                 x y)
  (values x y))

(defmethod transform-distance ((transformation identity-transformation)
                               dx dy)
  (values dx dy))

(defmethod untransform-distance ((transformation identity-transformation)
                                 dx dy)
  (values dx dy))

(defmethod transform-rectangle* ((transformation identity-transformation)
                                 x1 y1 x2 y2)
  (values x1 y1 x2 y2))

(defmethod untransform-rectangle* ((transformation identity-transformation)
                                   x1 y1 x2 y2)
  (values x1 y1 x2 y2))

;;; EOF
