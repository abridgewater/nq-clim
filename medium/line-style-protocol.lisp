;;;
;;; nq-clim/medium/line-style-protocol
;;;
;;; LINE-STYLE protocol operations
;;;

(cl:defpackage :nq-clim/medium/line-style-protocol
  (:use :cl)
  (:export
   "LINE-STYLE-UNIT"
   "LINE-STYLE-THICKNESS"
   "LINE-STYLE-JOINT-SHAPE"
   "LINE-STYLE-CAP-SHAPE"
   "LINE-STYLE-DASHES"))
(cl:in-package :nq-clim/medium/line-style-protocol)


(defgeneric line-style-unit (line-style))
(defgeneric line-style-thickness (line-style))
(defgeneric line-style-joint-shape (line-style))
(defgeneric line-style-cap-shape (line-style))
(defgeneric line-style-dashes (line-style))


;;; EOF
