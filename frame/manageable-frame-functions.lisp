;;;
;;; nq-clim/frame/manageable-frame-functions
;;;
;;; Functions to support managing a frame (per CLIM II 28.3 and 28.5).
;;;

(cl:defpackage :nq-clim/frame/manageable-frame-functions
  (:use :cl)
  (:export
   "FRAME-TOP-LEVEL-SHEET"))
(cl:in-package :nq-clim/frame/manageable-frame-functions)

(defgeneric frame-top-level-sheet (frame))
(defgeneric (setf frame-top-level-sheet) (name frame))

;;; EOF
