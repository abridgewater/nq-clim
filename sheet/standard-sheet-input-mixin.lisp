;;;
;;; nq-clim/sheet/standard-sheet-input-mixin
;;;
;;; One of four classes described in CLIM II 8.1.2.
;;;

(cl:defpackage :nq-clim/sheet/standard-sheet-input-mixin
  (:use :cl
        :nq-clim/event/event-queue-protocol
        :nq-clim/sheet/sheet-hierarchy-protocol)
  (:export
   "STANDARD-SHEET-INPUT-MIXIN"))
(cl:in-package :nq-clim/sheet/standard-sheet-input-mixin)


(defclass standard-sheet-input-mixin ()
  ())

;; Another option here (instead of tracing up the parent tree until
;; something has an event queue) is to stash the queue information on
;; the mirror.  A third option is to make it a slot that gets set
;; "somehow".
(defmethod sheet-event-queue ((sheet standard-sheet-input-mixin))
  (sheet-event-queue (sheet-parent sheet)))

;; FIXME: CLIM 8.1.2 says that we queue DEVICE-EVENTs but call
;; HANDLE-EVENT for "configuration events"...  And presumably we need
;; to do something else for repaint events.
(defmethod dispatch-event ((client standard-sheet-input-mixin) event)
  (queue-event client event))

(defmethod handle-event ((client standard-sheet-input-mixin) event)
  ;; Default behavior at this point is to ignore all events.  We may
  ;; need to handle some events by default in the future (although
  ;; that is more likely to be a matter for mirrored sheets).
  )

(defmethod queue-event ((client standard-sheet-input-mixin) event)
  (queue-event (sheet-event-queue client) event))

(defmethod event-read ((client standard-sheet-input-mixin))
  (event-read (sheet-event-queue client)))

(defmethod event-read-no-hang ((client standard-sheet-input-mixin))
  (event-read-no-hang (sheet-event-queue client)))

(defmethod event-peek ((client standard-sheet-input-mixin) &optional (event-type nil event-type-p))
  (if event-type-p
      (event-peek (sheet-event-queue client) event-type)
      (event-peek (sheet-event-queue client))))

(defmethod event-unread ((client standard-sheet-input-mixin) event)
  (event-unread (sheet-event-queue client) event))

(defmethod event-listen ((client standard-sheet-input-mixin))
  (event-listen (sheet-event-queue client)))

;;; EOF
