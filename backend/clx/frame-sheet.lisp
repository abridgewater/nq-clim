;;;
;;; nq-clim/backend/clx/frame-sheet.lisp
;;;
;;; The top-level sheet used for frames in the CLX port.
;;;

(cl:defpackage :nq-clim/backend/clx/frame-sheet
  (:use :cl
        :nq-clim/sheet/mirrored-sheet-mixin
        :nq-clim/sheet/sheet
        :nq-clim/sheet/sheet-geometry-mixin
        :nq-clim/sheet/sheet-parent-mixin)
  (:export
   "CLX-FRAME-SHEET"
   "CLX-FRAME-SHEET-FRAME"))
(cl:in-package :nq-clim/backend/clx/frame-sheet)

(defclass clx-frame-sheet (mirrored-sheet-mixin
                           sheet-parent-mixin
                           sheet-geometry-mixin
                           sheet)
  ((frame :initarg :frame :accessor clx-frame-sheet-frame)))

;;; EOF
