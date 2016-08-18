;;;
;;; nq-clim/backend/clx/event-handling
;;;
;;; CLX port event-handling implementation
;;;

(cl:defpackage :nq-clim/backend/clx/event-handling
  (:use :cl
        :nq-clim/backend/clx/port
        :nq-clim/event/event-queue-protocol
        :nq-clim/port/event-handling-protocol
        :nq-clim/sheet/sheet)
  (:import-from :xlib))
(cl:in-package :nq-clim/backend/clx/event-handling)


(defun convert-clx-event (&rest event-plist)
  (let ((type (getf event-plist :event-key)))
    (case type
      ;; FIXME: We should NEVER need this OTHERWISE clause.
      (otherwise
       (apply #'list event-plist)))))

(defun handle-clx-event (&rest event-plist)
  (let* ((event (apply #'convert-clx-event event-plist))
         (window (getf event :window))
         (sheet (getf (xlib:window-plist window) 'sheet)))
    ;; FIXME: We should be being clever about finding the appropriate
    ;; sheet (for example, for keyboard events).
    (dispatch-event sheet event)))

(defmethod process-next-event ((port clx-port) &key wait-function timeout)
  (declare (ignore timeout)) ;; FIXME: Implement timeouts
  (or (funcall wait-function)
      (xlib:process-event (clx-port-display port)
                          :handler #'handle-clx-event)))

;;; EOF
