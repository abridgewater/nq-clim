;;;
;;; nq-clim/clim-sys/named-constant-mixin.lisp
;;;
;;; Support for defining constants that are CLOS instances.
;;;

(cl:defpackage :nq-clim/clim-sys/named-constant-mixin
  (:use :cl)
  (:export
   "DEFINE-NAMED-CONSTANT"
   "NAMED-CONSTANT-MIXIN"))
(cl:in-package :nq-clim/clim-sys/named-constant-mixin)

(defclass named-constant-mixin ()
  ((constant-name :initarg constant-name
                  :reader named-constant-mixin-constant-name)))

;; Any references to an instance of NAMED-CONSTANT-MIXIN are to be
;; dumped as a symbol value access to the named constant (as a
;; singleton instance).
(defmethod make-load-form ((object named-constant-mixin) &optional environment)
  (declare (ignore environment))
  `(symbol-value ,(named-constant-mixin-constant-name object)))

;; When printing a NAMED-CONSTANT-MIXIN, make it evaluate to the value
;; of the name, for print/read consistency.
(defmethod print-object ((object named-constant-mixin) stream)
  (if (and *print-readably* (not *read-eval*))
      (error 'print-not-readable :object object)
      (format stream "#.~S" (named-constant-mixin-constant-name object))))


;; If this is the first definition of the constant, create an instance
;; of CLASS to use.  Otherwise, use the existing value.  This stunt is
;; usually (and illegally) pulled for non-EQL "constant" values.  In
;; this case, we're making sure that the value will always be EQ, even
;; through print/read and compile-file/load.  INITARGS are not
;; evaluated unless a new instances is being created.
(defmacro define-named-constant (name class &rest initargs)
  `(defconstant ,name
     (if (boundp ',name)
         (symbol-value ',name)
         (apply #'make-instance ',class 'constant-name ',name
                (funcall (lambda () (list ,@initargs)))))))

;;; EOF
