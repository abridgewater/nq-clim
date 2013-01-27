;;;
;;; nq-clim/sheet/sheet-notification-protocol
;;;
;;; CLIM II 8.5.1 and 8.5.2.
;;;

(cl:defpackage :nq-clim/sheet/sheet-notification-protocol
  (:use :cl
        :nq-clim/sheet/sheet)
  (:export
   "NOTE-SHEET-GRAFTED"
   "NOTE-SHEET-DEGRAFTED"
   "NOTE-SHEET-ADOPTED"
   "NOTE-SHEET-DISOWNED"
   "NOTE-SHEET-ENABLED"
   "NOTE-SHEET-DISABLED"
   "NOTE-SHEET-REGION-CHANGED"
   "NOTE-SHEET-TRANSFORMATION-CHANGED"))
(cl:in-package :nq-clim/sheet/sheet-notification-protocol)

(defgeneric note-sheet-grafted (sheet)
  (:documentation "Called when SHEET has been grafted.")
  (:method ((sheet sheet))
    ;; Don't error out if no subclass implements this.
    ))

(defgeneric note-sheet-degrafted (sheet)
  (:documentation "Called when SHEET has been degrafted.")
  (:method ((sheet sheet))
    ;; Don't error out if no subclass implements this.
    ))

(defgeneric note-sheet-adopted (sheet)
  (:documentation "Called when SHEET has been adopted.")
  (:method ((sheet sheet))
    ;; Don't error out if no subclass implements this.
    ))

(defgeneric note-sheet-disowned (sheet)
  (:documentation "Called when SHEET has been disowned.")
  (:method ((sheet sheet))
    ;; Don't error out if no subclass implements this.
    ))

(defgeneric note-sheet-enabled (sheet)
  (:documentation "Called when SHEET has been enabled.")
  (:method ((sheet sheet))
    ;; Don't error out if no subclass implements this.
    ))

(defgeneric note-sheet-disabled (sheet)
  (:documentation "Called when SHEET has been disabled.")
  (:method ((sheet sheet))
    ;; Don't error out if no subclass implements this.
    ))

(defgeneric note-sheet-region-changed (sheet)
  (:documentation "Called when SHEET's region has been changed.")
  (:method ((sheet sheet))
    ;; Don't error out if no subclass implements this.
    ))

(defgeneric note-sheet-transformation-changed (sheet)
  (:documentation "Called when SHEET's transformation has been changed.")
  (:method ((sheet sheet))
    ;; Don't error out if no subclass implements this.
    ))

;;; EOF
