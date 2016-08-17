;;;
;;; nq-clim/event/event-queue
;;;
;;; The implementation of CLIM II 8.1.1 event queues.
;;;

(cl:defpackage :nq-clim/event/event-queue
  (:use :cl
        :nq-clim/event/event-queue-protocol)
  (:export
   "MAKE-EVENT-QUEUE"))
(cl:in-package :nq-clim/event/event-queue)


;; FIXME: This is a very bare-bones implementation, not thread-safe in
;; the slightest.  It will need to be revisited when we move to a
;; multi-threaded system.

(defclass event-queue ()
  ((events :initform nil)))

(defun make-event-queue ()
  (make-instance 'event-queue))


;; These two methods deliberately not implemented for event-queues
;; (they are policy methods, intended for event client
;; implementation).
;;
;; (defmethod dispatch-event (client event))
;; (defmethod handle-event (client event))

(defmethod queue-event ((client event-queue) event)
  (with-slots (events) client
    (setf events (concatenate 'list events (list event)))))

(defmethod event-read ((client event-queue))
  (with-slots (events) client
    (unless events
      ;; FIXME: Technically should block here, possibly entering
      ;; PROCESS-NEXT-EVENT.
      (error "Attempting to read from empty queue ~S (should block, not implemented)" client))
    (pop events)))

(defmethod event-read-no-hang ((client event-queue))
  (with-slots (events) client
    (when events
      (pop events))))

;; These two methods not implemented for laziness reasons.  When we
;; find that they are necessary, they may be implemented.
;;
;; (defmethod event-peek (client &optional event-type))
;; (defmethod event-unread (client event))

(defmethod event-listen ((client event-queue))
  (with-slots (events) client
    (when events
      (values t))))

;;; EOF
