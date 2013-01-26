;;;
;;; nq-clim/geometry/transformation-composition
;;;
;;; Part of CLIM II 5.3.2.
;;;

(cl:defpackage :nq-clim/geometry/transformation-composition
  (:use :cl
        :nq-clim/geometry/transformation-protocol
        :nq-clim/geometry/scaling-transformation)
  (:export
   "COMPOSE-TRANSLATION-WITH-TRANSFORMATION"
   "COMPOSE-SCALING-WITH-TRANSFORMATION"
   ;; "COMPOSE-ROTATION-WITH-TRANSFORMATION"
   "COMPOSE-TRANSFORMATION-WITH-TRANSLATION"
   "COMPOSE-TRANSFORMATION-WITH-SCALING"
   ;; "COMPOSE-TRANSFORMATION-WITH-ROTATION"
   ))
(cl:in-package :nq-clim/geometry/transformation-composition)

(defun compose-translation-with-transformation (transformation dx dy)
  "Create a new transformation that has the effect of a translation by
DX, DY followed by applying TRANSFORMATION."
  (compose-transformations transformation
                           (make-translation-transformation dx dy)))

(defun compose-scaling-with-transformation (transformation sx sy #| &optional origin |#)
  "Create a new transformation that has the effect of a scaling by SX,
SY (centered around ORIGIN) followed by applying TRANSFORMATION."
  ;; FIXME: Should be modified to handle ORIGIN once we have POINTs.
  (compose-transformations transformation
                           #+(or)
                           (make-scaling-transformation sx sy origin)
                           #+(and)
                           (make-scaling-transformation* sx sy)))

#+(or) ;; Deferred, need rotation transformations.
(defun compose-rotation-with-transformation (transformation angle #| &optional origin |#)
  "Create a new transformation that has the effect of a rotation by
ANGLE (centered around ORIGIN) followed by applying TRANSFORMATION."
  ;; FIXME: Should be modified to handle ORIGIN once we have POINTs.
  (compose-transformations transformation
                           #+(or)
                           (make-rotation-transformation angle origin)
                           #+(and)
                           (make-rotation-transformation* angle)))

(defun compose-transformation-with-translation (transformation dx dy)
  "Create a new transformation that has the effect of TRANSFORMATION
followed by applying a translation by DX, DY."
  (compose-transformations (make-translation-transformation dx dy)
                           transformation))

(defun compose-transformation-with-scaling (transformation sx sy #| &optional origin |#)
  "Create a new transformation that has the effect of TRANSFORMATION
followed by applying a scaling by SX, SY (centered around ORIGIN)."
  ;; FIXME: Should be modified to handle ORIGIN once we have POINTs.
  (compose-transformations #+(or)
                           (make-scaling-transformation sx sy origin)
                           #+(and)
                           (make-scaling-transformation* sx sy)
                           transformation))

#+(or) ;; Deferred, need rotation transformations.
(defun compose-transformation-with-rotation (transformation angle #| &optional origin |#)
  "Create a new transformation that has the effect of TRANSFORMATION
followed by applying a rotation by ANGLE (centered around ORIGIN)."
  ;; FIXME: Should be modified to handle ORIGIN once we have POINTs.
  (compose-transformations #+(or)
                           (make-rotation-transformation angle origin)
                           #+(and)
                           (make-rotation-transformation* angle)
                           transformation))

;;; EOF
