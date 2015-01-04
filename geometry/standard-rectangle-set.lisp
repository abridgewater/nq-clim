;;;
;;; nq-clim/geometry/standard-rectangle-set.lisp
;;;
;;; Composite regions comprised of axis-aligned rectangles.
;;;

(cl:defpackage :nq-clim/geometry/standard-rectangle-set
  (:use :cl)
  (:export
   ))
(cl:in-package :nq-clim/geometry/standard-rectangle-set)

;; We represent a rectangle-set as a "shape" (Foley, van Dam, Feiner,
;; Hughes, 1990, section 19.7, and certainly other sources) comprised
;; of a set of vertical Y-spans each with an associated set of
;; horizontal X-spans.

;; A "span" is represented as a CONS with the (inclusive) start
;; position and (exclusive) end position as the CAR and CDR.
;(deftype span (cons real real))

;; We represent a Y-span with its set of X-spans as a list, with the
;; first element being the actual span and the remaining elements
;; being the X-spans in order.

;; We represent a shape as a list of its Y-spans in order.

(defun unite-x-span-sets (set-1 set-2)
  "Given two sets of X-spans, each in order, produce a minimal set of
X-spans representing the union of both sets."
  (flet ((next-span ()
           ;; Remove and return the span with the lowest start
           ;; coordinate from either set-1 or set-2, or NIL if there
           ;; are no more spans.
           (cond
             ((null set-1) (pop set-2))
             ((null set-2) (pop set-1))
             ((< (caar set-1) (caar set-2))
              (pop set-1))
             (t (pop set-2)))))
    (loop
       with (start-1 . end-1) = (next-span)
       for (start-2 . end-2) = (next-span)

       ;; We're out of spans, so return what we've collected.
       unless start-2 collect (cons start-1 end-1)
       while start-2

       if (<= start-2 end-1)
         ;; The spans abut or overlap, merge them.
         do (setf end-1 (max end-1 end-2))
       else
         ;; Collect the current span, and select the new one.
         collect (cons start-1 end-1)
         and do (setf start-1 start-2
                      end-1 end-2))))

;;; EOF
