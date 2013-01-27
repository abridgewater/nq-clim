;;;
;;; nq-clim/port/port-protocol
;;;
;;; Part of CLIM II 9.2.
;;;

(cl:defpackage :nq-clim/port/port-protocol
  (:use :cl
        :nq-clim/port/port)
  (:export
   "PORT-SERVER-PATH"
   "PORT-NAME"
   "PORT-TYPE"
   "PORT-PROPERTIES"
   "DESTROY-PORT"))
(cl:in-package :nq-clim/port/port-protocol)

(defgeneric port-server-path (port))

(defgeneric port-name (port))

(defgeneric port-type (port))

;; The &OPTIONAL parameter on these two functions is an extension to
;; CLIM, but one which makes sense by parallel with the use of GETF.
(defgeneric port-properties (port indicator &optional default))
(defgeneric (setf port-properties) (property port indicator &optional default))

(defgeneric destroy-port (port))

;;; EOF
