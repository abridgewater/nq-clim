;;;
;;; nq-clim/geometry/nowhere.lisp
;;;
;;; The empty region (from CLIM II 3.1)
;;;

(cl:defpackage :nq-clim/geometry/nowhere
  (:use :cl
        :nq-clim/clim-sys/named-constant-mixin
        :nq-clim/geometry/region
        :nq-clim/geometry/region-composition
        :nq-clim/geometry/transformation-protocol)
  (:export
   "+NOWHERE+"))
(cl:in-package :nq-clim/geometry/nowhere)


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The NOWHERE class needs to be available at compile-time in order
  ;; to set up +NOWHERE+ properly.
  (defclass nowhere (region named-constant-mixin) ()))

(define-named-constant +nowhere+ nowhere)

(setf (documentation '+nowhere+ 'variable)
  "The empty region, which contains no points and has no bounding
  rectangle.")


;; Region composition with +NOWHERE+ can be specified purely in terms
;; of which region is +NOWHERE+.
(defmethod region-union ((region-1 nowhere) region-2) region-2)
(defmethod region-union (region-1 (region-2 nowhere)) region-1)

(defmethod region-intersection ((region-1 nowhere) region-2) +nowhere+)
(defmethod region-intersection (region-1 (region-2 nowhere)) +nowhere+)

(defmethod region-difference ((region-1 nowhere) region-2) +nowhere+)
(defmethod region-difference (region-1 (region-2 nowhere)) region-1)


;; Transformations apply to points within a region, and +NOWHERE+ is
;; empty.
(defmethod transform-region (transformation (region nowhere))
  (declare (ignore transformation))
  region)

(defmethod untransform-region (transformation (region nowhere))
  (declare (ignore transformation))
  region)


;;; EOF
