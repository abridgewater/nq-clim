;;;
;;; nq-clim/geometry/rectilinear-region-composition
;;;
;;; Part of CLIM II 3.1.2
;;;

(cl:defpackage :nq-clim/geometry/rectilinear-region-composition
  (:use :cl
        :nq-clim/geometry/nowhere
        :nq-clim/geometry/rectilinear-region
        :nq-clim/geometry/region-composition
        :nq-clim/geometry/shape-operations
        :nq-clim/geometry/standard-rectangle
        :nq-clim/geometry/standard-rectangle-set))
(cl:in-package :nq-clim/geometry/rectilinear-region-composition)


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
                   :y-spans y-spans))))


(defmethod region-union ((region-1 rectilinear-region) (region-2 rectilinear-region))
  (box-y-spans-as-region
   (unite-y-span-sets
    (region-y-spans region-1)
    (region-y-spans region-2))))

(defmethod region-intersection ((region-1 rectilinear-region) (region-2 rectilinear-region))
  (box-y-spans-as-region
   (intersect-y-span-sets
    (region-y-spans region-1)
    (region-y-spans region-2))))

(defmethod region-difference ((region-1 rectilinear-region) (region-2 rectilinear-region))
  (box-y-spans-as-region
   (differ-y-span-sets
    (region-y-spans region-1)
    (region-y-spans region-2))))


;;; EOF
