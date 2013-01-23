;;;
;;; nq-clim/geometry/bounding-rectangle-api
;;;
;;; The BOUNDING-RECTANGLE API (parts of CLIM II 4.1.1 and 4.1.2)
;;;

(cl:defpackage :nq-clim/geometry/bounding-rectangle-api
  (:use :cl
        :nq-clim/geometry/bounding-rectangle-protocol
        :nq-clim/geometry/standard-bounding-rectangle)
  (:export
   "BOUNDING-RECTANGLE"
   "WITH-BOUNDING-RECTANGLE*"
   ;; "BOUNDING-RECTANGLE-POSITION"
   "BOUNDING-RECTANGLE-MIN-X"
   "BOUNDING-RECTANGLE-MIN-Y"
   "BOUNDING-RECTANGLE-MAX-X"
   "BOUNDING-RECTANGLE-MAX-Y"
   "BOUNDING-RECTANGLE-WIDTH"
   "BOUNDING-RECTANGLE-HEIGHT"
   "BOUNDING-RECTANGLE-SIZE"))
(cl:in-package :nq-clim/geometry/bounding-rectangle-api)


(defgeneric bounding-rectangle (region)
  (:documentation
   "Return the bounding rectangle of REGION (which is required to
   conform to the bounding-rectangle protocol) as a subclass of
   RECTANGLE.")
  (:method ((region bounding-rectangle))
    (multiple-value-call #'make-bounding-rectangle
      (bounding-rectangle* region))))

(defmacro with-bounding-rectangle* ((min-x min-y max-x max-y) region &body body)
  `(multiple-value-bind (,min-x ,min-y ,max-x ,max-y)
       (bounding-rectangle* ,region)
     ,@body))

;; FIXME: BOUNDING-RECTANGLE-POSITION goes here, but is defined to
;; return a POINT, which we have yet to implement.

(defgeneric bounding-rectangle-min-x (region)
  (:documentation
   "Return the minimum X coordinate of the bounding-rectangle of
   REGION (which is required to conform to the bounding-rectangle
   protocol).")
  (:method ((region bounding-rectangle))
    (nth-value 0 (bounding-rectangle* region))))

(defgeneric bounding-rectangle-min-y (region)
  (:documentation
   "Return the minimum Y coordinate of the bounding-rectangle of
   REGION (which is required to conform to the bounding-rectangle
   protocol).")
  (:method ((region bounding-rectangle))
    (nth-value 1 (bounding-rectangle* region))))

(defgeneric bounding-rectangle-max-x (region)
  (:documentation
   "Return the maximum X coordinate of the bounding-rectangle of
   REGION (which is required to conform to the bounding-rectangle
   protocol).")
  (:method ((region bounding-rectangle))
    (nth-value 2 (bounding-rectangle* region))))

(defgeneric bounding-rectangle-max-y (region)
  (:documentation
   "Return the maximum Y coordinate of the bounding-rectangle of
   REGION (which is required to conform to the bounding-rectangle
   protocol).")
  (:method ((region bounding-rectangle))
    (nth-value 3 (bounding-rectangle* region))))

(defgeneric bounding-rectangle-width (region)
  (:documentation
   "Return the width of the bounding rectangle of REGION (which is
   required to conform to the bounding-rectangle protocol).")
  (:method ((region bounding-rectangle))
    (multiple-value-bind (min-x ignore max-x)
        (bounding-rectangle* region)
      (declare (ignore ignore))
      (- max-x min-x))))

(defgeneric bounding-rectangle-height (region)
  (:documentation
   "Return the height of the bounding rectangle of REGION (which is
   required to conform to the bounding-rectangle protocol).")
  (:method ((region bounding-rectangle))
    (multiple-value-bind (ignore-1 min-y ignore-2 max-y)
        (bounding-rectangle* region)
      (declare (ignore ignore-1 ignore-2))
      (- max-y min-y))))

(defgeneric bounding-rectangle-size (region)
  (:documentation
   "Return the width and height of the bounding rectangle of
   REGION (which is required to conform to the bounding-rectangle
   protocol) as (VALUES WIDTH HEIGHT).")
  (:method ((region bounding-rectangle))
    (multiple-value-bind (min-x min-y max-x max-y)
        (bounding-rectangle* region)
      (values (- max-x min-x) (- max-y min-y)))))

;;; EOF
