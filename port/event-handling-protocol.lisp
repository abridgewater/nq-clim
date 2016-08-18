;;;
;;; nq-clim/port/event-handling-protocol
;;;
;;; Part of CLIM II 8.1.1.
;;;

(cl:defpackage :nq-clim/port/event-handling-protocol
  (:use :cl)
  (:export
   "PROCESS-NEXT-EVENT"
   "PORT-KEYBOARD-INPUT-FOCUS"))
(cl:in-package :nq-clim/port/event-handling-protocol)


(defgeneric process-next-event (port &key wait-function timeout))

(defgeneric port-keyboard-input-focus (port))
(defgeneric (setf port-keyboard-input-focus) (focus port))

;;; EOF
