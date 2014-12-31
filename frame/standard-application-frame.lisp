;;;
;;; nq-clim/frame/standard-application-frame
;;;
;;; The normal superclass for an application frame class.
;;;

(cl:defpackage :nq-clim/frame/standard-application-frame
  (:use :cl
        :nq-clim/frame/application-frame
        :nq-clim/frame/application-frame-functions
        :nq-clim/frame/manageable-frame-mixin)
  (:export
   "STANDARD-APPLICATION-FRAME"))
(cl:in-package :nq-clim/frame/standard-application-frame)

(defclass standard-application-frame (application-frame
                                      manageable-frame-mixin)
  ((pretty-name :initarg :pretty-name :accessor frame-pretty-name)))

;;; EOF
