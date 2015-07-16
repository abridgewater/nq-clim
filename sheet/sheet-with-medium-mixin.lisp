;;;
;;; nq-clim/sheet/sheet-with-medium-mixin
;;;
;;; Part of CLIM II 8.3.3.
;;;

(cl:defpackage :nq-clim/sheet/sheet-with-medium-mixin
  (:use :cl
        :nq-clim/medium/association
        :nq-clim/medium/medium
        :nq-clim/port/port
        :nq-clim/sheet/sheet)
  (:export
   "SHEET-WITH-MEDIUM-MIXIN"))
(cl:in-package :nq-clim/sheet/sheet-with-medium-mixin)


(defclass sheet-with-medium-mixin (sheet)
  ((medium :reader sheet-medium :initform nil)))


(defmethod engraft-medium :after ((medium medium) (port port) (sheet sheet-with-medium-mixin))
  (setf (slot-value sheet 'medium) medium))

(defmethod degraft-medium :after ((medium medium) (port port) (sheet sheet-with-medium-mixin))
  (setf (slot-value sheet 'medium) nil))


;;; EOF
