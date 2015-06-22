;;;
;;; nq-clim/sheet/mirrored-sheet-mixin
;;;
;;; CLIM II 9.4.
;;;

(cl:defpackage :nq-clim/sheet/mirrored-sheet-mixin
  (:use :cl
        :nq-clim/port/port
        :nq-clim/sheet/mirror-functions
        :nq-clim/sheet/sheet-hierarchy-protocol
        :nq-clim/sheet/sheet-notification-protocol)
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

(defmethod note-sheet-grafted :before ((sheet mirrored-sheet-mixin))
  ;; Realize the mirror when the sheet is grafted.  We can't call PORT
  ;; on SHEET, as it's a straight slot reader for us, but we CAN call
  ;; it for its parent, which should already have been mirrored either
  ;; directly or indirectly.
  (realize-mirror (port (sheet-parent sheet)) sheet))

;;; EOF
