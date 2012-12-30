;;;
;;; nq-clim/geometry/standard-bounding-rectangle
;;;
;;; Parts of CLIM II 4.1.
;;;

(cl:defpackage :nq-clim/geometry/standard-bounding-rectangle
  (:use :cl
        :nq-clim/geometry/rectangle-protocol
        :nq-clim/geometry/bounding-rectangle-protocol)
  (:export
   "STANDARD-BOUNDING-RECTANGLE"
   "MAKE-BOUNDING-RECTANGLE"))
(cl:in-package :nq-clim/geometry/standard-bounding-rectangle)

;; This is the definition of a standard bounding-rectangle, which
;; happens to also conform to the rectangle protocol, as opposed to
;; standard-rectangle, which only conforms to the rectangle protocol
;; (which happens to also require implementing the bounding-rectangle
;; protocol).  The two classes therefore are completely different,
;; their almost-identical (and copy/paste) construction
;; notwithstanding.

(defclass standard-bounding-rectangle (rectangle)
  ((min-x :initarg min-x)
   (min-y :initarg min-y)
   (max-x :initarg max-x)
   (max-y :initarg max-y)))

;; RECTANGLE contains a method for implementing BOUNDING-RECTANGLE* in
;; terms of RECTANGLE-EDGES*, which covers our BOUNDING-RECTANGLE
;; contract.  We just need to cover the RECTANGLE contract here:
(defmethod rectangle-edges* ((rectangle standard-bounding-rectangle))
  (with-slots (min-x min-y max-x max-y)
      rectangle
    (values min-x min-y max-x max-y)))

(defun make-bounding-rectangle (x1 y1 x2 y2)
  (make-instance 'standard-bounding-rectangle
                 'min-x (min x1 x2)
                 'min-y (min y1 y2)
                 'max-x (max x1 x2)
                 'max-y (max y1 y2)))

;;; EOF
