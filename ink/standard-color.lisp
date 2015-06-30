;;;
;;; nq-clim/ink/standard-color
;;;
;;; "Standard" color representation.
;;;

(cl:defpackage :nq-clim/ink/standard-color
  (:use :cl
        :nq-clim/clim-sys/named-constant-mixin
        :nq-clim/ink/color)
  (:export
   "+BLACK+"
   "+BLUE+"
   "+CYAN+"
   "+GREEN+"
   "+MAGENTA+"
   "+RED+"
   "+WHITE+"
   "+YELLOW+"
   "MAKE-RGB-COLOR"
   "MAKE-GRAY-COLOR"))
(cl:in-package :nq-clim/ink/standard-color)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass standard-color (color)
    ((red :initarg red :reader standard-color-red)
     (green :initarg green :reader standard-color-green)
     (blue :initarg blue :reader standard-color-blue))))

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass constant-standard-color (standard-color named-constant-mixin) ()))

(define-named-constant +black+ constant-standard-color
  'red 0 'green 0 'blue 0)

(define-named-constant +red+ constant-standard-color
  'red 1 'green 0 'blue 0)

(define-named-constant +green+ constant-standard-color
  'red 0 'green 1 'blue 0)

(define-named-constant +blue+ constant-standard-color
  'red 0 'green 0 'blue 1)

(define-named-constant +yellow+ constant-standard-color
  'red 1 'green 1 'blue 0)

(define-named-constant +magenta+ constant-standard-color
  'red 1 'green 0 'blue 1)

(define-named-constant +cyan+ constant-standard-color
  'red 0 'green 1 'blue 1)

(define-named-constant +white+ constant-standard-color
  'red 1 'green 1 'blue 1)

;;; EOF
