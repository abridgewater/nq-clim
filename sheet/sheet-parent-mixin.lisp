;;;
;;; nq-clim/sheet/sheet-parent-mixin
;;;
;;; From CLIM II 7.2.2.
;;;

(cl:defpackage :nq-clim/sheet/sheet-parent-mixin
  (:use :cl
        :nq-clim/sheet/sheet-hierarchy-protocol)
  (:export
   "SHEET-PARENT-MIXIN"))
(cl:in-package :nq-clim/sheet/sheet-parent-mixin)

(defclass sheet-parent-mixin ()
  ((parent :reader sheet-parent)))

(defmethod sheet-adopt-child :around (sheet (child sheet-parent-mixin))
  (when (sheet-parent child)
    (error 'sheet-already-has-parent :sheet child))
  (call-next-method)
  (setf (slot-value child 'parent) sheet))

(defmethod sheet-disown-child :around (sheet (child sheet-parent-mixin) &key (errorp t))
  (if (eq (sheet-parent child) sheet)
      (progn
        (call-next-method)
        (setf (slot-value child 'parent) nil))
      (when errorp
        (error 'sheet-is-not-child :parent sheet :sheet child))))

;;; EOF
