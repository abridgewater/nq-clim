;;;
;;; nq-clim/geometry/bounding-rectangle-protocol
;;;
;;; The BOUNDING-RECTANGLE protocol (CLIM II 4.1 and 4.1.1)
;;;

(cl:defpackage :nq-clim/geometry/bounding-rectangle-protocol
  (:use :cl)
  (:export
   "BOUNDING-RECTANGLE"
   "BOUNDING-RECTANGLE-P"
   "BOUNDING-RECTANGLE*"))
(cl:in-package :nq-clim/geometry/bounding-rectangle-protocol)

;; The protocol class.  Users may subclass this to provide other
;; objects that behave as bounding rectangles.
(defclass bounding-rectangle () ())

;; The protocol predicate.
(defun bounding-rectangle-p (object)
  "Return TRUE iif OBJECT is a BOUNDING-RECTANGLE."
  (typep object 'bounding-rectangle))

;; The one function in the protocol.
(defgeneric bounding-rectangle* (region)
  (:documentation
   "Return the bounding rectangle of REGION as the X and Y coordinates
   of the min and max points of the rectangle.  REGION must be some
   object that conforms to the BOUNDING-RECTANGLE protocol.
   Returns (VALUES MIN-X MIN-Y MAX-X MAX-Y), where (<= MIN-X MAX-X)
   and (<= MIN-Y MAX-Y)."))

;;; EOF
