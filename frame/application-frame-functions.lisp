;;;
;;; nq-clim/frame/application-frame-functions
;;;
;;; Functions that an application-frame must handle (per CLIM II 28.3).
;;;

(cl:defpackage :nq-clim/frame/application-frame-functions
  (:use :cl)
  (:export
   "FRAME-PRETTY-NAME"))
(cl:in-package :nq-clim/frame/application-frame-functions)

(defgeneric frame-pretty-name (frame))
(defgeneric (setf frame-pretty-name) (name frame))

;;; EOF
