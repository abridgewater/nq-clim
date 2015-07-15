;;;
;;; nq-clim/sheet/permanent-medium-sheet-output-mixin
;;;
;;; Part of CLIM II 8.3.3.
;;;

(cl:defpackage :nq-clim/sheet/permanent-medium-sheet-output-mixin
  (:use :cl
        :nq-clim/medium/association
        :nq-clim/medium/medium
        :nq-clim/port/port
        :nq-clim/sheet/sheet-notification-protocol
        :nq-clim/sheet/sheet-with-medium-mixin)
  (:export
   "PERMANENT-MEDIUM-SHEET-OUTPUT-MIXIN"))
(cl:in-package :nq-clim/sheet/permanent-medium-sheet-output-mixin)


(defclass permanent-medium-sheet-output-mixin (sheet-with-medium-mixin)
  ())

(defmethod note-sheet-grafted :after ((sheet permanent-medium-sheet-output-mixin))
  (engraft-medium (allocate-medium (port sheet)
                                   sheet)
                  (port sheet)
                  sheet))

(defmethod note-sheet-degrafted :after ((sheet permanent-medium-sheet-output-mixin))
  (let ((medium (sheet-medium sheet)))
    (degraft-medium medium (port medium) sheet)
    (deallocate-medium (port medium) medium)))

;;; EOF
