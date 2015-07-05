;;;
;;; nq-clim/medium/graphics-method
;;;
;;; CLIM II 12.5, and supporting logic
;;;

(cl:defpackage :nq-clim/medium/graphics-method
  (:use :cl
        :nq-clim/medium/drawing
        :nq-clim/medium/drawing-options)
  (:export
   "DEFINE-GRAPHICS-METHOD"
   "DRAW-POINT*"
   "DRAW-POINTS*"
   "DRAW-LINE*"
   "DRAW-LINES*"
   "DRAW-POLYGON*"
   "DRAW-RECTANGLE*"
   "DRAW-RECTANGLES*"))
(cl:in-package :nq-clim/medium/graphics-method)


(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun parse-keyword-parameter (keyword-parameter)
   ;; Given KEYWORD-PARAMETER, an ordinary lambda-list entry for a
   ;; keyword parameter, return the variable name, the indicator, the
   ;; default value, and the presence variable as values.  If there is
   ;; no default value or presence variable, then return NIL for those
   ;; terms.
   (when (symbolp keyword-parameter)
     ;; Canonicalize to list form.
     (setf keyword-parameter (list keyword-parameter)))
   (destructuring-bind (parameter-name &optional default-value present-p)
       keyword-parameter
     (when (symbolp parameter-name)
       ;; Canonicalize to list form.
       (setf parameter-name
             (list (intern (symbol-name parameter-name) :keyword)
                   parameter-name)))
     (values (cadr parameter-name)
             (car parameter-name)
             default-value
             present-p)))

 (defun process-keyargs-for-recovery (keyword-parameter-list)
   ;; From a keyword parameter list segment (each element of which is a
   ;; keyword parameter specifier for an ordinary lambda list), produce
   ;; a new parameter list segment which includes a presence indicator,
   ;; and a code fragment which produces a "compatible" list of
   ;; arguments based on the parameter variables.
   (loop
      for parameter in keyword-parameter-list
      for (parameter-var parameter-keyword default-value raw-presence-term)
        = (multiple-value-list (parse-keyword-parameter parameter))
      for presence-term = (or raw-presence-term (gensym))
      for gensym = (gensym)
      for output-code = `(when ,presence-term
                           (list ',parameter-keyword ,parameter-var))
      then `(let ((,gensym ,output-code))
              (if ,presence-term
                  (list* ',parameter-keyword ,parameter-var ,gensym)
                  ,gensym))
      collect `((,parameter-keyword ,parameter-var)
                ,default-value ,presence-term) into output-parameters
      finally (return
                (values output-parameters output-code)))))

(defmacro define-graphics-method (name internal-fun fixed-args key-args allowed-options)
  (multiple-value-bind (option-args rebuild-options)
      (process-keyargs-for-recovery allowed-options)
    (multiple-value-bind (drawing-args rebuild-drawing-args)
        (process-keyargs-for-recovery key-args)
      `(defun ,name (medium ,@fixed-args &key ,@drawing-args ,@option-args)
         (flet ((thunk ()
                  (apply #',internal-fun
                         medium ,@fixed-args
                         ,rebuild-drawing-args)))
           (declare (dynamic-extent #'thunk))
           (apply #'invoke-with-drawing-options
                  medium #'thunk ,rebuild-options))))))


(define-graphics-method draw-point*
    medium-draw-point* (x y) ()
    (ink clipping-region transformation
         line-style line-thickness line-unit))
(define-graphics-method draw-points*
    medium-draw-points* (coord-seq) ()
    (ink clipping-region transformation
         line-style line-thickness line-unit))
(define-graphics-method draw-line*
    medium-draw-line* (x1 y1 x2 y2) ()
    (ink clipping-region transformation
         line-style line-thickness line-unit line-dashes line-cap-shape))
(define-graphics-method draw-lines*
    medium-draw-lines* (coord-seq) ()
    (ink clipping-region transformation
         line-style line-thickness line-unit line-dashes line-cap-shape))

 ;; FIXME: The CLOSED and FILLED arguments below are supposed to be
 ;; keywords.  Or aren't supposed to be here at all in the case of
 ;; DRAW-RECTANGLES* But they aren't specified as parameters at all
 ;; for the corresponding MEDIUM- functions in the spec, nor are they
 ;; specified as medium properties...  And it turns out that some
 ;; other drawing functions that we'll need later require other
 ;; keyword arguments.
(define-graphics-method draw-polygon*
    medium-draw-polygon* (coord-seq closed filled) ()
    (ink clipping-region transformation
         line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape))
(define-graphics-method draw-rectangle*
    medium-draw-rectangle* (x1 y1 x2 y2 filled) ()
    (ink clipping-region transformation
         line-style line-thickness line-unit line-dashes line-joint-shape))
(define-graphics-method draw-rectangles*
    medium-draw-rectangles* (coord-seq filled) ()
    (ink clipping-region transformation
         line-style line-thickness line-unit line-dashes line-joint-shape))

;;; EOF
