;;;
;;; nq-clim/geometry/rectangle-protocol
;;;
;;; Parts of CLIM II 3.2.4 and 3.2.4.1.
;;;

(cl:defpackage :nq-clim/geometry/rectangle-protocol
  (:use :cl
        :nq-clim/geometry/bounding-rectangle-protocol
        :nq-clim/geometry/rectilinear-region
        :nq-clim/geometry/region)
  (:export
   "RECTANGLE"
   "RECTANGLEP"
   "RECTANGLE-EDGES*"))
(cl:in-package :nq-clim/geometry/rectangle-protocol)


;; The protocol class.
(defclass rectangle (rectilinear-region) ())

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

;; Implement the one method in the RECTILINEAR-REGION protocol as
;; well.
(defmethod region-y-spans ((region rectangle))
  (multiple-value-bind (min-x min-y max-x max-y) (rectangle-edges* region)
    ;; Per CLIM II 3.1, a user is permitted to create a RECTANGLE with
    ;; zero area, and while the system is permitted to return
    ;; +NOWHERE+ for such a construction, if the user creates a custom
    ;; implementation of the RECTANGLE protocol we could still end up
    ;; with coincident horizontal or vertical boundaries.  The "shape"
    ;; for such a RECTANGLE is NIL.
    (unless (or (= min-x max-x)
                (= min-y max-y))
      `(((,min-y . ,max-y) (,min-x . ,max-x))))))

;;; EOF
