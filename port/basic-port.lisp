;;;
;;; nq-clim/port/basic-port
;;;
;;; Part of CLIM II 9.2.
;;;

(cl:defpackage :nq-clim/port/basic-port
  (:use :cl
        :nq-clim/port/port
        :nq-clim/port/port-protocol)
  (:export
   "BASIC-PORT"))
(cl:in-package :nq-clim/port/basic-port)

(defclass basic-port (port)
  ((name :initarg :name)
   (server-path :initarg :server-path :reader port-server-path)
   (plist :initarg :plist :initform nil)))

(defmethod port-name ((port basic-port))
  (slot-value port 'name))

(defmethod port-type ((port basic-port))
  (car (port-server-path port)))

(defmethod port-properties ((port basic-port) indicator &optional (default nil))
  (getf (slot-value port 'plist) indicator default))

(defmethod (setf port-properties) (property (port basic-port) indicator &optional default)
  (declare (ignore default))
  (setf (getf (slot-value port 'plist) indicator) property))


;;; EOF
