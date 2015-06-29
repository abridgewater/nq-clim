;;;
;;; nq-clim/ink/standard-color
;;;
;;; "Standard" color representation.
;;;

(cl:defpackage :nq-clim/ink/standard-color
  (:use :cl
        :nq-clim/ink/color)
  (:export
   "MAKE-RGB-COLOR"
   "MAKE-GRAY-COLOR"))
(cl:in-package :nq-clim/ink/standard-color)


(defclass standard-color (color)
  ((red :initarg red :reader standard-color-red)
   (green :initarg green :reader standard-color-green)
   (blue :initarg blue :reader standard-color-blue)))

(defmethod color-rgb ((color standard-color))
  (values (standard-color-red color)
          (standard-color-green color)
          (standard-color-blue color)))


(defun make-rgb-color (red green blue)
  (declare (type (real 0 1) red green blue))
  (make-instance 'standard-color
                 'red red
                 'green green
                 'blue blue))

(defun make-gray-color (luminance)
  (declare (type (real 0 1) luminance))
  (make-instance 'standard-color
                 'red luminance
                 'green luminance
                 'blue luminance))

;;; EOF
