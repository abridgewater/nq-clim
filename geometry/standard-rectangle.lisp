;;;
;;; nq-clim/geometry/standard-rectangle
;;;
;;; Parts of CLIM II 3.2.4.
;;;

(cl:defpackage :nq-clim/geometry/standard-rectangle
  (:use :cl
        :nq-clim/geometry/bounding-rectangle-protocol
        :nq-clim/geometry/nowhere
        :nq-clim/geometry/rectangle-protocol)
  (:export
   "STANDARD-RECTANGLE"
   ;; "MAKE-RECTANGLE"
   "MAKE-RECTANGLE*"))
(cl:in-package :nq-clim/geometry/standard-rectangle)

;; This is the definition of a standard rectangle, which conforms to
;; the rectangle protocol (which happens to also require implementing
;; the bounding-rectangle protocol), as opposed to
;; standard-bounding-rectangle, which is a bounding-rectangle that
;; happens to also conform to the rectangle protocol.  The two classes
;; therefore are completely different, their almost-identical (and
;; copy/paste) construction notwithstanding.

(defclass standard-rectangle (rectangle)
  ((min-x :initarg min-x)
   (min-y :initarg min-y)
   (max-x :initarg max-x)
   (max-y :initarg max-y)))

(defmethod rectangle-edges* ((rectangle standard-rectangle))
  (with-slots (min-x min-y max-x max-y)
      rectangle
    (values min-x min-y max-x max-y)))

;; FIXME: MAKE-RECTANGLE should go here, but it's defined in terms of
;; POINTs, which we haven't implemented yet.

(defun make-rectangle* (x1 y1 x2 y2)
  (if (or (= x1 x2)
          (= y1 y2))
      +nowhere+
      (make-instance 'standard-rectangle
                     'min-x (min x1 x2)
                     'min-y (min y1 y2)
                     'max-x (max x1 x2)
                     'max-y (max y1 y2))))

;;; EOF
