;;;
;;; nq-clim/medium/standard-line-style
;;;
;;; The default implementation of the LINE-STYLE protocol
;;;

(cl:defpackage :nq-clim/medium/standard-line-style
  (:use :cl
        :nq-clim/medium/line-style
        :nq-clim/medium/line-style-protocol)
  (:export
   "STANDARD-LINE-STYLE"
   "MAKE-LINE-STYLE"))
(cl:in-package :nq-clim/medium/standard-line-style)


(defclass standard-line-style (line-style)
  ((unit :initarg unit :reader line-style-unit)
   (thickness :initarg thickness :reader line-style-thickness)
   (joint-shape :initarg joint-shape :reader line-style-joint-shape)
   (cap-shape :initarg cap-shape :reader line-style-cap-shape)
   (dashes :initarg dashes :reader line-style-dashes)))

(defun make-line-style (&key (unit :normal) (thickness 1) (joint-shape :miter) (cap-shape :butt) (dashes nil))
  (declare (type (member :normal :point :coordinate) unit)
           (type (real 0) thickness)
           (type (member :miter :bevel :round :none) joint-shape)
           (type (member :butt :square :round :no-end-point) cap-shape)
           (type (or sequence null (eql t)) dashes))
  ;; FIXME: DASHES is supposed to be an even-lengthed sequence of
  ;; REALs (plausibly positive REALs), NIL (which is actually an
  ;; even-lengthed sequence containing no elements), or T...  But
  ;; we're only verifying that it's a SEQUENCE or T, not the length or
  ;; element constraints.
  (make-instance 'standard-line-style
                 'unit unit
                 'thickness thickness
                 'joint-shape joint-shape
                 'cap-shape cap-shape
                 'dashes dashes))

;;; EOF
