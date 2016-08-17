;;;
;;; nq-clim/event/event-queue-protocol
;;;
;;; The parts of CLIM II 8.1.1 that deal with event queues proper.
;;;

(cl:defpackage :nq-clim/event/event-queue-protocol
  (:use :cl)
  (:export
   "SHEET-EVENT-QUEUE"
   "DISPATCH-EVENT"
   "QUEUE-EVENT"
   "HANDLE-EVENT"
   "EVENT-READ"
   "EVENT-READ-NO-HANG"
   "EVENT-PEEK"
   "EVENT-UNREAD"
   "EVENT-LISTEN"))
(cl:in-package :nq-clim/event/event-queue-protocol)

;; Technically a sheet function, not an event-queue function, but it
;; IS described in CLIM II 8.1.1, and we don't have a better place to
;; put it at the moment.
(defgeneric sheet-event-queue (sheet))

(defgeneric dispatch-event (client event))
(defgeneric queue-event (client event))
(defgeneric handle-event (client event))
(defgeneric event-read (client))
(defgeneric event-read-no-hang (client))
(defgeneric event-peek (client &optional event-type))
(defgeneric event-unread (client event))
(defgeneric event-listen (client))

;;; EOF
