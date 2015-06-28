;;;
;;; nq-clim/medium/drawing-options
;;;
;;; CLIM II 10.2 (and possibly 10.2.1 and 10.2.2)
;;;

(cl:defpackage :nq-clim/medium/drawing-options
  (:use :cl)
  (:export
   "WITH-DRAWING-OPTIONS"
   "INVOKE-WITH-DRAWING-OPTIONS"))
(cl:in-package :nq-clim/medium/drawing-options)


(defgeneric invoke-with-drawing-options (medium continuation &rest drawing-options))


(defmacro with-drawing-options ((medium &rest drawing-options) &body body)
  (declare (type symbol medium))
  (let ((thunk-name (gensym "WITH-DRAWING-OPTIONS-THUNK-")))
    `(flet ((,thunk-name () ,@body))
       (declare (dynamic-extent #',thunk-name))
       (invoke-with-drawing-options
        ,(if (eq medium t) '*standard-output* medium)
        ,@drawing-options))))

;;; EOF
