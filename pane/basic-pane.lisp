;;;
;;; nq-clim/pane/basic-pane
;;;
;;; The BASIC-PANE class (per CLIM II 29.2).
;;;

(cl:defpackage :nq-clim/pane/basic-pane
  (:use :cl)
  (:export
   "BASIC-PANE"))
(cl:in-package :nq-clim/pane/basic-pane)

(defclass basic-pane (pane
                      mirrored-sheet-mixin
                      sheet-geometry-mixin
                      sheet-parent-mixin
                      sheet)
  ())

;;; EOF
