;;;
;;; nq-clim/sheet/mirrored-sheet-mixin
;;;
;;; CLIM II 9.4.
;;;

(cl:defpackage :nq-clim/sheet/mirrored-sheet-mixin
  (:use :cl
        :nq-clim/sheet/mirror-functions)
  (:export
   "MIRRORED-SHEET-MIXIN"))
(cl:in-package :nq-clim/sheet/mirrored-sheet-mixin)

(defclass mirrored-sheet-mixin ()
  ((port :reader port :initform nil)
   (mirror :reader sheet-direct-mirror :initform nil)))

(defmethod realize-mirror :around (port (mirrored-sheet mirrored-sheet-mixin))
  (let ((mirror (call-next-method)))
    (setf (slot-value mirrored-sheet 'port) port)
    (setf (slot-value mirrored-sheet 'mirror) mirror)
    mirror))

(defmethod destroy-mirror :after (port (mirrored-sheet mirrored-sheet-mixin))
  (setf (slot-value mirrored-sheet 'port) nil)
  (setf (slot-value mirrored-sheet 'mirror) nil))

;;; EOF
