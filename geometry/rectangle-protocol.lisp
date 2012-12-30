;;;
;;; nq-clim/geometry/rectangle-protocol
;;;
;;; Parts of CLIM II 3.2.4 and 3.2.4.1.
;;;

(cl:defpackage :nq-clim/geometry/rectangle-protocol
  (:use :cl
        :nq-clim/geometry/region)
  (:export
   "RECTANGLE"
   "RECTANGLEP"
   "RECTANGLE-EDGES*"))
(cl:in-package :nq-clim/geometry/rectangle-protocol)


;; The protocol class.
(defclass rectangle (area) ())

;; The protocol predicate.
(defun rectanglep (object)
  "Return TRUE iif OBJECT is a RECTANGLE."
  (typep object 'rectangle))

;; The one function in the protocol.
(defgeneric rectangle-edges* (rectangle)
  (:documentation
   "Return the edges of RECTANGLE as the X and Y coordinates of the
   min and max points of the rectangle.  REGION must be some object
   that conforms to the RECTANGLE protocol.  Returns (VALUES MIN-X
   MIN-Y MAX-X MAX-Y), where (<= MIN-X MAX-X) and (<= MIN-Y MAX-Y)."))

;; All RECTANGLEs inherit from BOUNDING-RECTANGLE, and thus need an
;; implementation of the bounding-rectangle protocol.
(defmethod bounding-rectangle* ((region rectangle))
  (rectangle-edges* region))

;;; EOF
