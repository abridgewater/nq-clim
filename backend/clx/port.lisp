;;;
;;; nq-clim/backend/clx/port
;;;
;;; CLX port implementation
;;;

(cl:defpackage :nq-clim/backend/clx/port
  (:use :cl
        :nq-clim/port/port
        :nq-clim/port/port-protocol
        :nq-clim/port/port-discovery
        :nq-clim/port/basic-port)
  (:export
   "CLX-PORT"
   "CLX-PORT-DISPLAY"))
(cl:in-package :nq-clim/backend/clx/port)


(defgeneric clx-port-display (clx-port)
  #+(or)
  (:documentation ""))

(defclass clx-port (basic-port)
  ((display :initarg display :reader clx-port-display)))


;; The resolve-server-path / create-port pair here parallels the
;; behavior of xlib:open-default-display in CLX.

(defmethod resolve-server-path ((port-type (eql :clx))
                                &key display)
  (destructuring-bind (host display screen protocol)
      (xlib::get-default-display display)
    `(:clx :host ,host :display ,display :protocol ,protocol :screen ,screen)))

(defmethod create-port ((port-type (eql :clx)) &rest initargs
                        &key server-path &allow-other-keys)
  (destructuring-bind (type &key host display protocol screen) server-path
    (declare (ignore type))
    (let* ((clx-display (xlib:open-display host :display display
                                           :protocol protocol))
           (port (apply #'make-instance 'clx-port
                        'display clx-display
                        initargs)))
      (setf (xlib:display-default-screen clx-display)
            (nth screen (xlib:display-roots clx-display)))
      ;; FIXME: Start up message-handling loop here.
      port)))


;; When we are done with a CLX port, close the display.
(defmethod destroy-port ((port clx-port))
  (xlib:close-display (clx-port-display port)))

;;; EOF
