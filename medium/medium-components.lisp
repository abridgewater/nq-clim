;;;
;;; nq-clim/medium/medium-components
;;;
;;; CLIM II 10.1
;;;

(cl:defpackage :nq-clim/medium/medium-components
  (:use :cl)
  (:export
   "MEDIUM-BACKGROUND"
   "MEDIUM-FOREGROUND"
   "MEDIUM-INK"))
(cl:in-package :nq-clim/medium/medium-components)


(defgeneric medium-background (medium))
(defgeneric (setf medium-background) (design medium))

(defgeneric medium-foreground (medium))
(defgeneric (setf medium-foreground) (design medium))

(defgeneric medium-ink (medium))
(defgeneric (setf medium-ink) (design medium))


;;; EOF
