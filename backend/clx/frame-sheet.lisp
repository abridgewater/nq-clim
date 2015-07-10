;;;
;;; nq-clim/backend/clx/frame-sheet.lisp
;;;
;;; The top-level sheet used for frames in the CLX port.
;;;

(cl:defpackage :nq-clim/backend/clx/frame-sheet
  (:use :cl
        :nq-clim/sheet/basic-sheet
        :nq-clim/sheet/mirrored-sheet-mixin
        :nq-clim/sheet/sheet-multiple-child-mixin)
  (:export
   "CLX-FRAME-SHEET"
   "CLX-FRAME-SHEET-FRAME"))
(cl:in-package :nq-clim/backend/clx/frame-sheet)

(defclass clx-frame-sheet (mirrored-sheet-mixin
                           ;; KLUDGE: "Should" be
                           ;; SHEET-SINGLE-CHILD-MIXIN
                           sheet-multiple-child-mixin
                           basic-sheet)
  ((frame :initarg :frame :accessor clx-frame-sheet-frame)))

;;; EOF
