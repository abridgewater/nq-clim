;;;
;;; nq-clim/geometry/standard-rectangle-set.lisp
;;;
;;; Composite regions comprised of axis-aligned rectangles.
;;;

(cl:defpackage :nq-clim/geometry/standard-rectangle-set
  (:use :cl
        :nq-clim/geometry/bounding-rectangle-protocol
        :nq-clim/geometry/nowhere
        :nq-clim/geometry/rectangle-protocol
        :nq-clim/geometry/rectilinear-region
        :nq-clim/geometry/region-set
        :nq-clim/geometry/standard-rectangle)
  (:export
   "STANDARD-RECTANGLE-SET"))
(cl:in-package :nq-clim/geometry/standard-rectangle-set)


(defclass standard-rectangle-set (region-set rectilinear-region)
  ((y-spans :initarg y-spans :reader region-y-spans)))

(defmethod bounding-rectangle* ((region standard-rectangle-set))
  ;; Unfortunately, this relies on intimate knowledge of the structure
  ;; of the SHAPE held in Y-SPANS.  It is also quite thoroughly
  ;; opaque.
  (let* ((y-spans (region-y-spans region))
         (min-x (apply #'min (mapcar #'caadr y-spans)))
         (min-y (caaar y-spans))
         (max-x (apply #'max (mapcar #'cdar (mapcar #'last y-spans))))
         (max-y (cdaar (last y-spans))))
    (values min-x min-y max-x max-y)))


(defun box-y-spans-as-region (y-spans)
  (cond
   ((null y-spans)
    ;; Empty region
    +nowhere+)
   ((and (= 1 (length y-spans))
         (= 2 (length (car y-spans))))
    ;; A single rectangle
    (destructuring-bind (((min-y . max-y) (min-x . max-x))) y-spans
      (make-rectangle* min-x min-y max-x max-y)))
   (t
    ;; The general case
    (make-instance 'standard-rectangle-set
                   'y-spans y-spans))))

;;; EOF
