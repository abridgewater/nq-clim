;;;
;;; nq-clim/sheet/sheet-geometry-mixin
;;;
;;; What we're doing instead of CLIM II 7.3.2.
;;;

(cl:defpackage :nq-clim/sheet/sheet-geometry-mixin
  (:use :cl
        :nq-clim/sheet/sheet-geometry-protocol)
  (:export
   "SHEET-GEOMETRY-MIXIN"))
(cl:in-package :nq-clim/sheet/sheet-geometry-mixin)


(defclass sheet-geometry-mixin ()
  ((transformation :accessor sheet-transformation)
   (region :accessor sheet-region)))


;;; The following is a bit of a scratch area for implementing the
;;; geometry protocol.  Most or all of this is, once implemented,
;;; expected to be promoted to :METHOD options of the generic function
;;; definitions rather than remain here.

#|
 (defgeneric map-over-sheets-containing-position (function sheet x y))
 (defgeneric map-over-sheets-overlapping-region (function sheet region))
|#

#+(or) ;; Need region predicates for this.
(defmethod child-containing-position ((sheet sheet-geometry-mixin) x y)
  ;; Relies on SHEET-CHILDREN being in topmost-to-bottommost order.
  (some (lambda (child)
          (when (multiple-value-call
                    ;; FIXME: REGION-CONTAINS-POSITION-P is a region
                    ;; predicate protocol function, we don't have that
                    ;; protocol yet.
                    #'region-contains-position-p
                  (sheet-region child)
                  (map-sheet-position-to-child child x y))
            child))
        (sheet-children sheet)))

#|
 (defgeneric children-overlapping-region (sheet region))
 (defgeneric children-overlapping-rectangle* (sheet x1 y1 x2 y2))

 (defgeneric sheet-delta-transformation (sheet ancestor))

 (defgeneric sheet-allocated-region (sheet child))
|#

;;; EOF
