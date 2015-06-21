;;;
;;; nq-clim/geometry/everywhere.lisp
;;;
;;; The all-inclusive region (from CLIM II 3.1)
;;;

(cl:defpackage :nq-clim/geometry/everywhere
  (:use :cl
        :nq-clim/clim-sys/named-constant-mixin
        :nq-clim/geometry/nowhere
        :nq-clim/geometry/region
        :nq-clim/geometry/region-composition
        :nq-clim/geometry/transformation-protocol)
  (:export
   "+EVERYWHERE+"))
(cl:in-package :nq-clim/geometry/everywhere)


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The EVERYWHERE class needs to be available at compile-time in
  ;; order to set up +EVERYWHERE+ properly.
  (defclass everywhere (region named-constant-mixin) ()))

(define-named-constant +everywhere+ everywhere)

(setf (documentation '+everywhere+ 'variable)
  "The all-inclusive region, which contains all points and has no
bounding rectangle.")


;; Region composition with +EVERYWHERE+ can be specified purely in
;; terms of which region is +EVERYWHERE+...  Except for one of the
;; cases of REGION-DIFFERENCE, for which CLIM II is inconsistent.
(defmethod region-union ((region-1 everywhere) region-2) +everywhere+)
(defmethod region-union (region-1 (region-2 everywhere)) +everywhere+)

(defmethod region-intersection ((region-1 everywhere) region-2) region-2)
(defmethod region-intersection (region-1 (region-2 everywhere)) region-1)

(defmethod region-difference ((region-1 everywhere) region-2)
  (cond ((eq region-2 +nowhere+)
         +everywhere+)
        ((eq region-2 +everywhere+)
         +nowhere+)
        (t
         (error "Per the definitions of REGION-DIFFERENCE and REGION-SET in CLIM II 3.1.2, REGION-DIFFERENCE with REGION-1 of +EVERYWHERE+ must have REGION-2 of +NOWHERE+ or +EVERYWHERE+, not anything else"))))
(defmethod region-difference (region-1 (region-2 everywhere)) +nowhere+)


;; Transformations apply to points within a region, and while
;; +EVERYWHERE+ contains points, it contains all points, so a
;; transformation has no effect.
(defmethod transform-region (transformation (region everywhere))
  (declare (ignore transformation))
  region)

(defmethod untransform-region (transformation (region everywhere))
  (declare (ignore transformation))
  region)

;;; EOF
