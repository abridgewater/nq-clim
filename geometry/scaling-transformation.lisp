;;;
;;; nq-clim/geometry/scaling-transformation.lisp
;;;
;;; Parts of CLIM II 5.2.
;;;

(cl:defpackage :nq-clim/geometry/scaling-transformation
  (:use :cl
        :nq-clim/geometry/transformation
        :nq-clim/geometry/transformation-protocol
        :nq-clim/geometry/coordinate
        :nq-clim/geometry/rectangle-protocol
        :nq-clim/geometry/standard-rectangle)
  (:export
   "MAKE-TRANSLATION-TRANSFORMATION"
   "MAKE-SCALING-TRANSFORMATION"
   "MAKE-SCALING-TRANSFORMATION*"))
(cl:in-package :nq-clim/geometry/scaling-transformation)

(defclass scaling-transformation (transformation)
  (($m_{xx}$ :initarg :scale-x)
   ($m_{yy}$ :initarg :scale-y)
   ($t_x$ :initarg :translate-x)
   ($t_y$ :initarg :translate-y)))

;; This is a bit of a hack, defining a translation transformation to
;; be a scaling transformation with unit scale, but it saves quite a
;; bit of work, and there's no specified way to tell the difference.
(defun make-translation-transformation (translation-x translation-y)
  (make-instance 'scaling-transformation
                 :scale-x 1
                 :scale-y 1
                 :translate-x translation-x
                 :translate-y translation-y))

;; FIXME: MAKE-SCALING-TRANSFORMATION would go here if we had POINT objects.

(defun make-scaling-transformation* (scale-x scale-y &optional (origin-x 0) (origin-y 0))
  (make-instance 'scaling-transformation
                 :scale-x scale-x
                 :scale-y scale-y
                 ;; We need to translate origin to zero, then scale,
                 ;; then translate zero back to origin.  An equivalent
                 ;; is to bias the final translation by the distance
                 ;; between the origin and zero.
                 :translate-x (- origin-x (* scale-x origin-x))
                 :translate-y (- origin-y (* scale-y origin-y))))

#+(or)
(defmethod transformation-equal ((transformation1 scaling-transformation)
                                 transformation2)
  ;; FIXME: Implement
  )

(defmethod identity-transformation-p ((transformation scaling-transformation))
  ;; A unit scaling transformation with no translation is an identity
  ;; transformation.
  (with-slots ($m_{xx}$ $m_{yy}$ $t_x$ $t_y$) transformation
    (and (= $m_{xx}$ $m_{yy}$ 1)
         (= $t_x$ $t_y$ 0))))

(defmethod invertible-transformation-p ((transformation scaling-transformation))
  ;; So long as neither scaling axis is zero, the transformation can
  ;; be inverted.
  (with-slots ($m_{xx}$ $m_{yy}$) transformation
    (and (not (zerop $m_{xx}$))
         (not (zerop $m_{yy}$)))))

(defmethod translation-transformation-p ((transformation scaling-transformation))
  ;; A unit scaling transformation is a pure translation.  Possibly by
  ;; zero, but that counts.
  (with-slots ($m_{xx}$ $m_{yy}$) transformation
    (= $m_{xx}$ $m_{yy}$ 1)))

(defmethod reflection-transformation-p ((transformation scaling-transformation))
  ;; Scaling only inverts "handedness" if the signs of the axis scale
  ;; factors differ.  And we'll consider the reflectivity of a
  ;; singular translation to be undetermined at this point.
  (with-slots ($m_{xx}$ $m_{yy}$) transformation
    ;; If these were integers, LOGXORing them together would yield a
    ;; negative result if the signs differed and a positive result if
    ;; they were the same, but they're not guaranteed to be integers
    ;; (and in some cases can be guaranteed to not be integers).
    (if (< $m_{xx}$ 0)
        (> $m_{yy}$ 0)
        (< $m_{yy}$ 0))))

(defmethod rigid-transformation-p ((transformation scaling-transformation))
  ;; Scaling preserves magnitudes of angles, but only preserves
  ;; magnitudes of lengths if it's a unit scaling transformation.
  (with-slots ($m_{xx}$ $m_{yy}$) transformation
    (= $m_{xx}$ $m_{yy}$ 1)))

(defmethod even-scaling-transformation-p ((transformation scaling-transformation))
  ;; A scaling transformation is "even" when both axes are scaled by
  ;; the same amount.
  (with-slots ($m_{xx}$ $m_{yy}$) transformation
    (= $m_{xx}$ $m_{yy}$)))

(defmethod scaling-transformation-p ((transformation scaling-transformation))
  ;; By definition, a scaling transformation is a scaling transformation.
  t)

(defmethod rectilinear-transformation-p ((transformation scaling-transformation))
  ;; Scaling always transforms axis-aligned rectangles to axis-aligned
  ;; rectangles.
  t)


;; Composition with IDENTITY-TRANSFORMATION is handled by methods
;; defined in identity-transformation.lisp.  Composition with other
;; transformation types (not that we have any at this point) are the
;; responsibility of those other transformations.
(defmethod compose-transformations ((transformation1 scaling-transformation)
                                    (transformation2 scaling-transformation))
  (with-slots
        (($m^1_{xx}$ $m_{xx}$)
         ($m^1_{yy}$ $m_{yy}$)
         ($t^1_x$ $t_x$)
         ($t^1_y$ $t_y$))
      transformation1
    (with-slots
          (($m^2_{xx}$ $m_{xx}$)
           ($m^2_{yy}$ $m_{yy}$)
           ($t^2_x$ $t_x$)
           ($t^2_y$ $t_y$))
        transformation2
      ;; Return a transformation equivalent to applying
      ;; transformation2 followed by transformation1.  For the scale
      ;; components, this is simply multiplying the two together.  For
      ;; the translation components, the transformation2 components
      ;; should be multiplied by transformation1's scale components
      ;; and then added to transformation1's translation components.
      ;; Note that this amounts to a TRANSFORM-DISTANCE of the scale
      ;; factors and a TRANSFORM-POINT of the translation factors.
      (make-instance 'scaling-transformation
                     :scale-x (* $m^1_{xx}$ $m^2_{xx}$)
                     :scale-y (* $m^1_{yy}$ $m^2_{yy}$)
                     :translate-x (+ $t^1_x$ (* $m^1_{xx}$ $t^2_x$))
                     :translate-y (+ $t^1_y$ (* $m^1_{yy}$ $t^2_y$))))))

#+(or) ;; Don't use this one, it loses precision if coordinates are
       ;; clamped to integers, and they are.
(defmethod compose-transformations ((transformation1 scaling-transformation)
                                    (transformation2 scaling-transformation))
  ;; Return a transformation equivalent to applying
  ;; transformation2 followed by transformation1.  For the scale
  ;; components, this is simply multiplying the two together.  For
  ;; the translation components, the transformation2 components
  ;; should be multiplied by transformation1's scale components
  ;; and then added to transformation1's translation components.
  ;; Note that this amounts to a TRANSFORM-DISTANCE of the scale
  ;; factors and a TRANSFORM-POSITION of the translation factors.
  (with-slots ($m_{xx}$ $m_{yy}$ $t_x$ $t_y$) transformation2
    (multiple-value-bind
          (scale-x scale-y)
        (transform-distance transformation1 $m_{xx}$ $m_{yy}$)
      (multiple-value-bind
            (translate-x translate-y)
          (transform-position transformation1 $t_x$ $t_y$)
        (make-instance 'scaling-transformation
                       :scale-x scale-x
                       :scale-y scale-y
                       :translate-x translate-x
                       :translate-y translate-y)))))

(defmethod invert-transformation ((transformation scaling-transformation))
  ;; We're a scaling transformation, so to invert the scaling part,
  ;; take the reciprocal of our scale factors.  For the translation
  ;; part, consider what happens if we transform the zero point.  Zero
  ;; scaled is zero, and translated gives us our translation.  To
  ;; reverse that, we need to subtract the translation, but this is
  ;; done post-scaling in the inverted transformation, so we need to
  ;; rescale the final translation.
  (with-slots ($m_{xx}$ $m_{yy}$ $t_x$ $t_y$) transformation
    ;; We are a singular (not-invertible) transformation if either of
    ;; our scale factors are zero.
    (when (or (zerop $m_{xx}$)
              (zerop $m_{yy}$))
      (error 'singular-transformation
             :transformation transformation))
    (make-instance 'scaling-transformation
                   :scale-x (/ 1 $m_{xx}$)
                   :scale-y (/ 1 $m_{yy}$)
                   :translate-x (- (/ $t_x$ $m_{xx}$))
                   :translate-y (- (/ $t_y$ $m_{yy}$)))))

#+(or) ;; This is disingenuous, it needs to handle each region type
       ;; specifically.  That said, composite region types might be
       ;; able to handle themselves.
(defmethod transform-region ((transformation scaling-transformation)
                             region)
  ;; FIXME: Implement.
  )

(defmethod transform-region ((transformation scaling-transformation)
                             (region rectangle))
  (multiple-value-bind
        (min-x min-y max-x max-y)
      (rectangle-edges* region)
    ;; NOTE: Using MULTIPLE-VALUE-CALL is usually a BAD idea if there
    ;; are any circumstances under which the FORMs (in this case, two
    ;; calls to TRANSFORM-POSITION) might be revised to return a
    ;; different number of values.  In this case, the number of values
    ;; returned from TRANSFORM-POSITION is specified quite precisely,
    ;; and thus unlikely to change.
    (multiple-value-call
        #'make-rectangle*
      (transform-position transformation min-x min-y)
      (transform-position transformation max-x max-y))))

#+(or) ;; Relying (at least temporarily) on the default method.
(defmethod untransform-region ((transformation scaling-transformation)
                               region)
  region)

(defmethod transform-position ((transformation scaling-transformation)
                               x y)
  (with-slots ($m_{xx}$ $m_{yy}$ $t_x$ $t_y$) transformation
    (values (coordinate (+ (* $m_{xx}$ x) $t_x$))
            (coordinate (+ (* $m_{yy}$ y) $t_y$)))))

#+(or) ;; Relying (at least temporarily) on the default method.
(defmethod untransform-position ((transformation scaling-transformation)
                                 x y)
  (values x y))

(defmethod transform-distance ((transformation scaling-transformation)
                               dx dy)
  (with-slots ($m_{xx}$ $m_{yy}$) transformation
    (values (coordinate (+ (* $m_{xx}$ dx)))
            (coordinate (+ (* $m_{yy}$ dy))))))

#+(or) ;; Relying (at least temporarily) on the default method.
(defmethod untransform-distance ((transformation scaling-transformation)
                                 dx dy)
  (values dx dy))

(defmethod transform-rectangle* ((transformation scaling-transformation)
                                 x1 y1 x2 y2)
  (with-slots ($m_{xx}$ $m_{yy}$ $t_x$ $t_y$) transformation
    (values (coordinate (+ (* $m_{xx}$ x1) $t_x$))
            (coordinate (+ (* $m_{yy}$ y1) $t_y$))
            (coordinate (+ (* $m_{xx}$ x2) $t_x$))
            (coordinate (+ (* $m_{yy}$ y2) $t_y$)))))

#+(or) ;; Relying (at least temporarily) on the default method.
(defmethod untransform-rectangle* ((transformation scaling-transformation)
                                   x1 y1 x2 y2)
  (values x1 y1 x2 y2))

;;; EOF
