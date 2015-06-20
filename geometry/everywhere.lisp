;;;
;;; nq-clim/geometry/everywhere.lisp
;;;
;;; The all-inclusive region (from CLIM II 3.1)
;;;

(cl:defpackage :nq-clim/geometry/everywhere
  (:use :cl
        :nq-clim/clim-sys/named-constant-mixin
        :nq-clim/geometry/region
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
