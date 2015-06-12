;;;
;;; nq-clim/geometry/everywhere.lisp
;;;
;;; The all-inclusive region (from CLIM II 3.1)
;;;

(cl:defpackage :nq-clim/geometry/everywhere
  (:use :cl
        :nq-clim/geometry/region
        :nq-clim/geometry/transformation-protocol)
  (:export
   "+EVERYWHERE+"))
(cl:in-package :nq-clim/geometry/everywhere)


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The EVERYWHERE class and corresponding MAKE-LOAD-FORM method both
  ;; need to be available at compile-time in order to set up
  ;; +EVERYWHERE+ properly.
  (defclass everywhere (region) ())

  ;; Any references to an instance of EVERYWHERE are to be dumped as a
  ;; symbol value access to +EVERYWHERE+ (as a singleton instance).
  (defmethod make-load-form ((object everywhere) &optional environment)
    (declare (ignore environment))
    '(symbol-value +everywhere+)))

;; If this is the first definition of +EVERYWHERE+, create an instance
;; of EVERYWHERE to use.  Otherwise, use the existing value.  This
;; stunt is usually (and illegally) pulled for non-EQL "constant"
;; values.  In this case, we're making sure that the value will always
;; be EQ, even through print/read and compile-file/load.
(defconstant +everywhere+ (if (boundp '+everywhere+)
                           (symbol-value '+everywhere+)
                           (make-instance 'everywhere))
  "The empty region, which contains no points and has no bounding
  rectangle.")

;; When printing a EVERYWHERE, make it evaluate to the value of
;; +EVERYWHERE+, for print/read consistency.
(defmethod print-object ((object everywhere) stream)
  (if (and *print-readably* (not *read-eval*))
      (error 'print-not-readable :object object)
      (format stream "#.~S" '+everywhere+)))


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
