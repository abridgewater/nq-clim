;;;
;;; nq-clim/medium/basic-medium
;;;
;;; The "abstract" medium class dealing with "user transformations".
;;;

(cl:defpackage :nq-clim/medium/basic-medium
  (:use :cl
        :nq-clim/ink/indirect-ink
        :nq-clim/ink/standard-color
        :nq-clim/medium/association
        :nq-clim/medium/medium-components
        :nq-clim/medium/drawing
        :nq-clim/medium/drawing-options
        :nq-clim/medium/medium)
  (:export
   "BASIC-MEDIUM"))
(cl:in-package :nq-clim/medium/basic-medium)


(defclass basic-medium (medium)
  ;; FIXME: This is the hook for "user" transformations.
  ((sheet :initform nil :reader medium-sheet)
   (foreground :initform +black+ :accessor medium-foreground)
   (background :initform +white+ :accessor medium-background)
   (ink :initform +foreground-ink+ :accessor medium-ink)))


(defmethod engraft-medium ((medium basic-medium) port sheet)
  ;; FIXME: Set medium properties from sheet defaults.
  (setf (slot-value medium 'sheet) sheet))

(defmethod degraft-medium ((medium basic-medium) port sheet)
  (setf (slot-value medium 'sheet) nil))

(defmethod invoke-with-drawing-options ((medium basic-medium) continuation &rest drawing-options)
  (let (old-ink)
    (destructuring-bind
          (&key ink)
        drawing-options
      ;; FIXME: Support other drawing options as well.
      (when ink
        (shiftf old-ink (medium-ink medium) ink))
      (unwind-protect
           (funcall continuation)
        (when old-ink
          (setf (medium-ink medium) old-ink))))))

#+(or)
(defmethod medium-draw-line* :around ((medium basic-medium) x1 y1 x2 y2)
  ;; FIXME: Transform the points (x1,y1) and (x2,y2), then
  ;; call-next-method with the transformed points.
  )

;;; EOF
