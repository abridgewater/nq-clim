;;;
;;; nq-clim/geometry/nowhere.lisp
;;;
;;; The empty region (from CLIM II 3.1)
;;;

(cl:defpackage :nq-clim/geometry/nowhere
  (:use :cl
        :nq-clim/geometry/region
        :nq-clim/geometry/region-composition
        :nq-clim/geometry/transformation-protocol)
  (:export
   "+NOWHERE+"))
(cl:in-package :nq-clim/geometry/nowhere)


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The NOWHERE class and corresponding MAKE-LOAD-FORM method both
  ;; need to be available at compile-time in order to set up +NOWHERE+
  ;; properly.
  (defclass nowhere (region) ())

  ;; Any references to an instance of NOWHERE are to be dumped as a
  ;; symbol value access to +NOWHERE+ (as a singleton instance).
  (defmethod make-load-form ((object nowhere) &optional environment)
    (declare (ignore environment))
    '(symbol-value +nowhere+)))

;; If this is the first definition of +NOWHERE+, create an instance of
;; NOWHERE to use.  Otherwise, use the existing value.  This stunt is
;; usually (and illegally) pulled for non-EQL "constant" values.  In
;; this case, we're making sure that the value will always be EQ, even
;; through print/read and compile-file/load.
(defconstant +nowhere+ (if (boundp '+nowhere+)
                           (symbol-value '+nowhere+)
                           (make-instance 'nowhere))
  "The empty region, which contains no points and has no bounding
  rectangle.")

;; When printing a NOWHERE, make it evaluate to the value of
;; +NOWHERE+, for print/read consistency.
(defmethod print-object ((object nowhere) stream)
  (if (and *print-readably* (not *read-eval*))
      (error 'print-not-readable :object object)
      (format stream "#.~S" '+nowhere+)))


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
