;;;
;;; nq-clim/frame/manageable-frame-mixin
;;;
;;; Application-frame side support for frame management.
;;;

(cl:defpackage :nq-clim/frame/manageable-frame-mixin
  (:use :cl
        :nq-clim/frame/application-frame
        :nq-clim/frame/manageable-frame-functions)
  (:export
   "MANAGEABLE-FRAME-MIXIN"))
(cl:in-package :nq-clim/frame/manageable-frame-mixin)

(defclass manageable-frame-mixin ()
  ((top-level-sheet :initform nil :accessor frame-top-level-sheet)))

;;; EOF
