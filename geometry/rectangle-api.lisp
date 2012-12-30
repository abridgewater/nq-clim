;;;
;;; nq-clim/geometry/rectangle-api
;;;
;;; Parts of CLIM II 3.2.4.1.
;;;

(cl:defpackage :nq-clim/geometry/rectangle-api
  (:use :cl
        :nq-clim/geometry/rectangle-protocol)
  (:export
   ;; "RECTANGLE-MIN-POINT"
   ;; "RECTANGLE-MAX-POINT"
   "RECTANGLE-MIN-X"
   "RECTANGLE-MIN-Y"
   "RECTANGLE-MAX-X"
   "RECTANGLE-MAX-Y"
   "RECTANGLE-WIDTH"
   "RECTANGLE-HEIGHT"
   "RECTANGLE-SIZE"))
(cl:in-package :nq-clim/geometry/rectangle-api)

;; FIXME: RECTANGLE-{MIN,MAX}-POINT go here, but return POINT objects,
;; which we have yet to define.

(defgeneric rectangle-min-x (rectangle)
  (:documentation
   "Return the minimum X coordinate of RECTANGLE.")
  (:method ((rectangle rectangle))
    (nth-value 0 (rectangle-edges* rectangle))))

(defgeneric rectangle-min-y (rectangle)
  (:documentation
   "Return the minimum Y coordinate of RECTANGLE.")
  (:method ((rectangle rectangle))
    (nth-value 1 (rectangle-edges* rectangle))))

(defgeneric rectangle-max-x (rectangle)
  (:documentation
   "Return the maximum X coordinate of RECTANGLE.")
  (:method ((rectangle rectangle))
    (nth-value 2 (rectangle-edges* rectangle))))

(defgeneric rectangle-max-y (rectangle)
  (:documentation
   "Return the maximum Y coordinate of RECTANGLE.")
  (:method ((rectangle rectangle))
    (nth-value 3 (rectangle-edges* rectangle))))

(defgeneric rectangle-width (rectangle)
  (:documentation
   "Return the width of RECTANGLE.")
  (:method ((rectangle rectangle))
    (multiple-value-bind (min-x ignore max-x)
        (rectangle-edges* rectangle)
      (declare (ignore ignore))
      (- max-x min-x))))

(defgeneric rectangle-height (rectangle)
  (:documentation
   "Return the height of RECTANGLE.")
  (:method ((rectangle rectangle))
    (multiple-value-bind (ignore-1 min-y ignore-2 max-y)
        (rectangle-edges* rectangle)
      (declare (ignore ignore-1 ignore-2))
      (- max-y min-y))))

(defgeneric rectangle-size (rectangle)
  (:documentation
   "Return the width and height of RECTANGLE as (VALUES WIDTH HEIGHT).")
  (:method ((rectangle rectangle))
    (multiple-value-bind (min-x min-y max-x max-y)
        (rectangle-edges* rectangle)
      (values (- max-x min-x) (- max-y min-y)))))

;;; EOF
