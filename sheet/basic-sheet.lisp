;;;
;;; nq-clim/sheet/basic-sheet
;;;
;;; The BASIC-SHEET class (per CLIM II 7.1).
;;;

(cl:defpackage :nq-clim/sheet/basic-sheet
  (:use :cl
        :nq-clim/sheet/sheet
        :nq-clim/sheet/sheet-geometry-mixin
        :nq-clim/sheet/sheet-hierarchy-protocol
        :nq-clim/sheet/sheet-parent-mixin)
  (:export
   "BASIC-SHEET"))
(cl:in-package :nq-clim/sheet/basic-sheet)


(defclass basic-sheet (sheet-geometry-mixin
                       sheet-parent-mixin
                       sheet)
  ((enabled-p :initform t :reader sheet-enabled-p)))

(defmethod (setf sheet-enabled-p) (enabled-p (sheet basic-sheet))
  (let ((was-enabled (sheet-enabled-p sheet))
        (is-enabled (and enabled-p t)))
    (setf (slot-value sheet 'enabled-p) is-enabled)
    (unless (eq was-enabled is-enabled)
      (if is-enabled
        (note-sheet-enabled sheet)
        (note-sheet-disabled sheet)))))

;;; EOF
