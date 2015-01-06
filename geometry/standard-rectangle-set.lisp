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

(defmacro collecting-x-spans (nil &body body)
  "Return the set of X-spans accumulated by calling (COLLECT-X-SPAN
START END) within BODY, coalescing adjacent X-spans."
  (let ((collected-spans-var (gensym))
        (start-var (gensym))
        (end-var (gensym))
        (finish-x-span-fun (gensym)))
    `(let (,collected-spans-var
           ,start-var
           ,end-var)
       (labels ((,finish-x-span-fun ()
                  (when ,start-var
                    (push (cons ,start-var ,end-var)
                          ,collected-spans-var)))
                (collect-x-span (start end)
                  (cond
                    ((and ,end-var
                          (= start ,end-var))
                     (setf ,end-var end))
                    (t
                     (,finish-x-span-fun)
                     (setf ,start-var start
                           ,end-var end)))))
         (progn
           ,@body)
         (,finish-x-span-fun)
         (nreverse ,collected-spans-var)))))

(defun operate-on-x-span-sets (set-1 set-2 operation)
  (collecting-x-spans ()
    (macrolet ((span-start (span)
                 (ecase span (span-1 'start-1) (span-2 'start-2)))
               (span-end (span)
                 (ecase span (span-1 'end-1) (span-2 'end-2)))
               (span-set (span)
                 (ecase span (span-1 'set-1) (span-2 'set-2)))
               (next-span (set)
                 `(let ((span (pop ,set)))
                    (values (car span) (cdr span)))))
      (symbol-macrolet ((span-1 (values start-1 end-1))
                        (span-2 (values start-2 end-2)))
        (let (start-1 end-1 start-2 end-2)
          (macrolet ((discard-span-before (span boundary)
                       ;; If SPAN extends beyond BOUNDARY, discard the
                       ;; portion of SPAN prior to BOUNDARY.
                       ;; Otherwise, pull the next span from the
                       ;; corresponding list.
                       `(if (<= (span-end ,span) ,boundary)
                            (setf ,span (next-span (span-set ,span)))
                            (setf (span-start ,span) ,boundary))))

            (setf span-1 (next-span set-1))
            (setf span-2 (next-span set-2))
            (loop
               while (and span-1 span-2)
               do (cond
                    ((< start-1 start-2)
                     (when (funcall operation t nil)
                       (collect-x-span start-1 (min end-1 start-2)))
                     (discard-span-before span-1 start-2))

                    ((> start-1 start-2)
                     (when (funcall operation nil t)
                       (collect-x-span start-2 (min end-2 start-1)))
                     (discard-span-before span-2 start-1))

                    (t
                     (let ((boundary (min end-1 end-2)))
                       (when (funcall operation t t)
                         (collect-x-span start-1 boundary))
                       (discard-span-before span-1 boundary)
                       (discard-span-before span-2 boundary)))))
            (when (funcall operation t nil)
              (loop
                 while span-1
                 do (collect-x-span start-1 end-1)
                   (setf span-1 (next-span set-1))))
            (when (funcall operation nil t)
              (loop
                 while span-2
                 do (collect-x-span start-2 end-2)
                   (setf span-2 (next-span set-2))))))))))

(defun union-operation (x y) (or x y))
(defun intersection-operation (x y) (and x y))

(defun unite-x-span-sets (set-1 set-2)
  "Given two sets of X-spans, each in order, produce a minimal set of
X-spans representing the union of both sets."
  (operate-on-x-span-sets set-1 set-2 #'union-operation))

(defun intersect-x-span-sets (set-1 set-2)
  "Given two sets of X-spans, each in order, produce a minimal set of
X-spans representing the intersection of both sets."
  (operate-on-x-span-sets set-1 set-2 #'intersection-operation))

(defmacro collecting-y-spans (nil &body body)
  "Return the set of Y-spans accumulated by calling (COLLECT-Y-SPAN
START END X-SPANS) within BODY, coalescing identical adjacent
Y-spans."
  (let ((collected-spans-var (gensym))
        (start-var (gensym))
        (end-var (gensym))
        (x-spans-var (gensym))
        (finish-y-span-fun (gensym)))
    `(let (,collected-spans-var
           ,start-var
           ,end-var
           ,x-spans-var)
       (labels ((,finish-y-span-fun ()
                  (when ,start-var
                    (push (cons (cons ,start-var ,end-var) ,x-spans-var)
                          ,collected-spans-var)))
                (collect-y-span (start end x-spans)
                  (cond
                    ((and ,end-var
                          (= start ,end-var)
                          (equal x-spans ,x-spans-var))
                     (setf ,end-var end))
                    (t
                     (,finish-y-span-fun)
                     (setf ,start-var start
                           ,end-var end
                           ,x-spans-var x-spans)))))
         (progn
           ,@body)
         (,finish-y-span-fun)
         (nreverse ,collected-spans-var)))))

(defun unite-y-span-sets (set-1 set-2)
  "Given two sets of Y-spans, each in order, produce a minimal set of
Y-spans representing the union of both sets."
  (collecting-y-spans ()
    ;; This mess, from the first MACROLET through the LET, allows us
    ;; to work with two Y-spans, each drawn from their specific list,
    ;; being able to refer to each Y-span in aggregate as SPAN-[12] or
    ;; in terms of the components START-[12], END-[12], and
    ;; X-SPANS-[12], to retrieve the next Y-span using NEXT-SPAN on
    ;; the corresponding list variable SET-[12], to be able to mutate
    ;; each Y-span without affecting the original data (the input sets
    ;; are treated as immutable), and to treat SPAN-{START,END,SET} as
    ;; accessors.  Arguably, it is an exercise in excess cleverness.
    (macrolet ((span-start (span)
                 (ecase span (span-1 'start-1) (span-2 'start-2)))
               (span-end (span)
                 (ecase span (span-1 'end-1) (span-2 'end-2)))
               (span-set (span)
                 (ecase span (span-1 'set-1) (span-2 'set-2)))
               (next-span (set)
                 `(let ((span (pop ,set)))
                    (values (caar span) (cdar span) (cdr span)))))
      (symbol-macrolet ((span-1 (values start-1 end-1 x-spans-1))
                        (span-2 (values start-2 end-2 x-spans-2)))
        (let (start-1 end-1 x-spans-1 start-2 end-2 x-spans-2)
          (macrolet ((discard-span-before (span boundary)
                       ;; If SPAN extends beyond BOUNDARY, discard the
                       ;; portion of SPAN prior to BOUNDARY.
                       ;; Otherwise, pull the next span from the
                       ;; corresponding list.
                       `(if (<= (span-end ,span) ,boundary)
                            (setf ,span (next-span (span-set ,span)))
                            (setf (span-start ,span) ,boundary))))

            (setf span-1 (next-span set-1))
            (setf span-2 (next-span set-2))
            (loop
               while (and span-1 span-2)
               do (cond
                    ((< start-1 start-2)
                     (collect-y-span start-1 (min end-1 start-2) x-spans-1)
                     (discard-span-before span-1 start-2))

                    ((> start-1 start-2)
                     (collect-y-span start-2 (min end-2 start-1) x-spans-2)
                     (discard-span-before span-2 start-1))

                    (t
                     (let ((boundary (min end-1 end-2)))
                       (collect-y-span start-1 boundary
                                       (unite-x-span-sets x-spans-1 x-spans-2))
                       (discard-span-before span-1 boundary)
                       (discard-span-before span-2 boundary)))))
            (loop
               while span-1
               do (collect-y-span start-1 end-1 x-spans-1)
                 (setf span-1 (next-span set-1)))
            (loop
               while span-2
               do (collect-y-span start-2 end-2 x-spans-2)
                 (setf span-2 (next-span set-2)))))))))

;;; EOF
