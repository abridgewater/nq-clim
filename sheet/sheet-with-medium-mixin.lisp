;;;
;;; nq-clim/sheet/sheet-with-medium-mixin
;;;
;;; Part of CLIM II 8.3.3.
;;;

(cl:defpackage :nq-clim/sheet/sheet-with-medium-mixin
  (:use :cl
        :nq-clim/medium/association
        :nq-clim/medium/medium
        :nq-clim/port/port)
  (:export
   "SHEET-WITH-MEDIUM-MIXIN"))
(cl:in-package :nq-clim/sheet/sheet-with-medium-mixin)


(defclass sheet-with-medium-mixin ()
  ((medium :reader sheet-medium :initform nil)))


(defmethod engraft-medium ((medium medium) (port port) (sheet sheet-with-medium-mixin))
  (setf (slot-value sheet 'medium) medium))

(defmethod degraft-medium ((medium medium) (port port) (sheet sheet-with-medium-mixin))
  (setf (slot-value sheet 'medium) nil))


;;; EOF
