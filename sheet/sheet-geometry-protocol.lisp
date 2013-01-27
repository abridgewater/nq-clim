;;;
;;; nq-clim/sheet/geometry-protocol
;;;
;;; CLIM II 7.3.1.
;;;

(cl:defpackage :nq-clim/sheet/sheet-geometry-protocol
  (:use :cl
        :nq-clim/sheet/sheet
        :nq-clim/sheet/sheet-notification-protocol
        :nq-clim/geometry/bounding-rectangle-protocol
        :nq-clim/geometry/transformation-composition
        :nq-clim/geometry/transformation-protocol
        :nq-clim/geometry/standard-rectangle)
  (:export
   "SHEET-TRANSFORMATION"
   "SHEET-REGION"
   "MOVE-SHEET"
   "RESIZE-SHEET"
   "MOVE-AND-RESIZE-SHEET"
   "MAP-SHEET-POSITION-TO-PARENT"
   "MAP-SHEET-POSITION-TO-CHILD"
   "MAP-SHEET-RECTANGLE*-TO-PARENT"
   "MAP-SHEET-RECTANGLE*-TO-CHILD"
   "MAP-OVER-SHEETS-CONTAINING-POSITION"
   "MAP-OVER-SHEETS-OVERLAPPING-REGION"
   "CHILD-CONTAINING-POSITION"
   "CHILDREN-OVERLAPPING-REGION"
   "CHILDREN-OVERLAPPING-RECTANGLE*"
   "SHEET-DELTA-TRANSFORMATION"
   "SHEET-ALLOCATED-REGION"))
(cl:in-package :nq-clim/sheet/sheet-geometry-protocol)


(defgeneric sheet-transformation (sheet)
  (:documentation "A TRANSFORMATION that maps sheet coordinates to parent coordinates."))
(defgeneric (setf sheet-transformation) (transformation sheet)
  (:method :after (transformation (sheet sheet))
    (declare (ignore transformation))
    (note-sheet-transformation-changed sheet)))

(defgeneric sheet-region (sheet)
  (:documentation "The extent of SHEET, in terms of sheet coordinates."))
(defgeneric (setf sheet-region) (region sheet)
  (:method :after (region (sheet sheet))
    (declare (ignore region))
    (note-sheet-region-changed sheet)))

;; This is the earliest point that we can implement the
;; bounding-rectangle protocol for sheets.
(defmethod bounding-rectangle* ((region sheet))
  "The bounding-rectangle of a sheet is the bounding rectangle of its
region in terms of its parent's coordinate system."
  (bounding-rectangle*
   (transform-region (sheet-transformation region)
                     (sheet-region region))))

(defgeneric move-sheet (sheet x y)
  (:documentation "Move SHEET by altering its TRANSFORMATION by way of translation such that the point (0,0) in its coordinate system is (x,y) in the parent coordinate system.")
  (:method ((sheet sheet) x y)
    (let ((transformation (sheet-transformation sheet)))
      (multiple-value-bind (old-x old-y)
          (transform-position transformation 0 0)
        (setf (sheet-transformation sheet)
              (compose-transformation-with-translation
               transformation (- x old-x) (- y old-y)))))))

(defgeneric resize-sheet (sheet width height)
  (:documentation "Resize SHEET by altering its REGION to a RECTANGLE ((0,0),(WIDTH,HEIGHT)).")
  (:method ((sheet sheet) width height)
    (setf (sheet-region sheet)
          (make-rectangle* 0 0 width height))))

(defgeneric move-and-resize-sheet (sheet x y width height)
  (:method ((sheet sheet) x y width height)
    (move-sheet sheet x y)
    (resize-sheet sheet width height)))

;; NOTE: The MAP-SHEET-<whatever>-TO-{PARENT,CHILD} functions are
;; rather unfortunately named, as they have nothing to do with what
;; Lisp programmers think of when they see a function that starts with
;; "MAP-".

(defgeneric map-sheet-position-to-parent (sheet x y)
  (:documentation "Return the values of the child coordinates X and Y as parent coordinates.")
  (:method ((sheet sheet) x y)
    (transform-position (sheet-transformation sheet) x y)))

(defgeneric map-sheet-position-to-child (sheet x y)
  (:documentation "Return the values of the parent coordinates X and Y as child coordinates.")
  (:method ((sheet sheet) x y)
    (untransform-position (sheet-transformation sheet) x y)))

(defgeneric map-sheet-rectangle*-to-parent (sheet x1 y1 x2 y2)
  (:method ((sheet sheet) x1 y1 x2 y2)
    (transform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2)))

(defgeneric map-sheet-rectangle*-to-child (sheet x1 y1 x2 y2)
  (:method ((sheet sheet) x1 y1 x2 y2)
    (untransform-rectangle* (sheet-transformation sheet) x1 y1 x2 y2)))

;; NOTE: The MAP-OVER-SHEETS-<whatever> functions are also
;; unfortunately named.  Surely, SHEETS should be CHILDREN or
;; CHILD-SHEETS.

(defgeneric map-over-sheets-containing-position (function sheet x y))
(defgeneric map-over-sheets-overlapping-region (function sheet region))

(defgeneric child-containing-position (sheet x y)
  (:documentation "Return the topmost child of SHEET that contains the position (X,Y), or NIL."))

(defgeneric children-overlapping-region (sheet region))
(defgeneric children-overlapping-rectangle* (sheet x1 y1 x2 y2))

(defgeneric sheet-delta-transformation (sheet ancestor))

(defgeneric sheet-allocated-region (sheet child))

;;; EOF
