;;;
;;; nq-clim/layout/pane-layout-protocol
;;;
;;; Protocol operations for controlling pane layout.
;;;

(cl:defpackage :nq-clim/layout/pane-layout-protocol
  (:use :cl
        :nq-clim/sheet/sheet
        :nq-clim/pane/pane)
  (:export
   "COMPOSE-SPACE"
   "NOTE-SPACE-REQUIREMENTS-CHANGED"))
(cl:in-package :nq-clim/layout/pane-layout-protocol)

(defgeneric compose-space (pane &key width height))
(defgeneric note-space-requirements-changed (sheet pane)
  (:method ((sheet sheet) (pane pane))
           ;; Do nothing as a default action.
           ))

;;; EOF
