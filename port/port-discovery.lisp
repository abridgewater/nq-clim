;;;
;;; nq-clim/port/port-discovery
;;;
;;; Part of CLIM II 9.2.
;;;

(cl:defpackage :nq-clim/port/port-discovery
  (:use :cl
        :nq-clim/port/port
        :nq-clim/port/port-protocol)
  (:export
   "*DEFAULT-SERVER-PATH*"
   "FIND-PORT"
   "MAP-OVER-PORTS"

   ;; The following are nq-clim specific implementation hooks.
   "RESOLVE-SERVER-PATH"
   "CREATE-PORT"))
(cl:in-package :nq-clim/port/port-discovery)

(defvar *all-ports* nil "All currently-open ports.")

(defun map-over-ports (function)
  (dolist (port *all-ports*)
    (funcall function port)))

(defmethod destroy-port :after (port)
  (setf *all-ports* (remove port *all-ports*)))


(defparameter *default-server-path* '(:clx))

(defgeneric resolve-server-path (port-type &key)
  (:documentation "To be implemented by each port type, convert the
spread server path passed as arguments to what would be returned from
PORT-SERVER-PATH called on the port should a port be opened with this
path designator."))

(defgeneric create-port (port-type &key)
  (:documentation "To be implemented by each port type, create a port
object corresponding to the server path passed as parameters."))

(defun find-port (&rest initargs
                  &key (server-path *default-server-path*)
                  &allow-other-keys)
  (let ((server-path (apply #'resolve-server-path server-path)))
    (map-over-ports
     (lambda (port)
       (when (equal server-path (port-server-path port))
         (return-from find-port port))))
    (let ((port (apply #'create-port (car server-path)
                 :server-path server-path
                 initargs)))
      (push port *all-ports*)
      port)))

;;; EOF
