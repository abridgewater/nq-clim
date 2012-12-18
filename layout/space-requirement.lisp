;;;
;;; layout/space-requirement.lisp
;;;
;;; The space-requirement class part of CLIM II 29.3.4 (layout protocol).
;;;

(cl:defpackage :nq-clim/layout/space-requirement
  (:use :cl)
  (:export

   ;; The protocol class
   "SPACE-REQUIREMENT"

   ;; The constructor
   "MAKE-SPACE-REQUIREMENT"

   ;; The accessors
   "SPACE-REQUIREMENT-WIDTH"
   "SPACE-REQUIREMENT-MIN-WIDTH"
   "SPACE-REQUIREMENT-MAX-WIDTH"
   "SPACE-REQUIREMENT-HEIGHT"
   "SPACE-REQUIREMENT-MIN-HEIGHT"
   "SPACE-REQUIREMENT-MAX-HEIGHT"
   "SPACE-REQUIREMENT-COMPONENTS"

   ;; Manipulation functions
   "SPACE-REQUIREMENT-COMBINE"
   "SPACE-REQUIREMENT-+"
   "SPACE-REQUIREMENT-+*"
   ))
(cl:in-package :nq-clim/layout/space-requirement)


;; The protocol class.  Users are permitted to subclass this in order
;; to allow other objects to serve as space-requirement objects.
(defclass space-requirement () ())


;; The accessor functions.  Forward-declared, as their methods are all
;; defined in defclass forms as slot readers.
(defgeneric space-requirement-width      (space-req))
(defgeneric space-requirement-min-width  (space-req))
(defgeneric space-requirement-max-width  (space-req))
(defgeneric space-requirement-height     (space-req))
(defgeneric space-requirement-min-height (space-req))
(defgeneric space-requirement-max-height (space-req))

;; The "other" accessor function, forward-declared for consistency.
(defgeneric space-requirement-components (space-req))


;; The implementation class.
(defclass standard-space-requirement (space-requirement)
  ((width      :initarg width      :reader space-requirement-width)
   (max-width  :initarg max-width  :reader space-requirement-max-width)
   (min-width  :initarg min-width  :reader space-requirement-min-width)
   (height     :initarg height     :reader space-requirement-height)
   (max-height :initarg max-height :reader space-requirement-max-height)
   (min-height :initarg min-height :reader space-requirement-min-height)))


;; A standard way to grab all of the slots of a space-requirement at
;; one time.
(defmethod space-requirement-components
    ((space-req standard-space-requirement))
  (with-slots (width  min-width  max-width
	       height min-height max-height)
      space-req
    (values width  min-width  max-width
	    height min-height max-height)))

;; The constructor.  Returns an immutable space-requirement object.
(defun make-space-requirement (&key
			       (width 0)  (max-width 0)  (min-width 0)
			       (height 0) (max-height 0) (min-height 0))
  (make-instance 'standard-space-requirement
		 'width  width  'max-width  max-width  'min-width  min-width
		 'height height 'max-height max-height 'min-height min-height))

;; Combining two space requirements by some function.  While this
;; doesn't seem particularly useful at first glance, a
;; vertical-stacking layout could obtain the space-requirements of its
;; children, combine with #'+ and #'MAX, destructure the two results,
;; take the width data from the MAX result and the height data from
;; the + result, and pass the mess off to make-space-requirement.
(defun space-requirement-combine (function sr1 sr2)
  ;; Rather than write anything out long-hand, why not be clever for
  ;; once?  We have a list of slots-that-matter, a way to obtain a
  ;; list of the values of the slots from a space-requirement, and a
  ;; constructor that takes keyword arguments.  Historically, I'd have
  ;; used a LOOP form, but for some reason the mapping functions
  ;; called to me this time...
  (apply #'make-space-requirement
	 (mapcan (lambda (key sr1-value sr2-value)
		   (list key (funcall function sr1-value sr2-value)))
		 '(:width  :min-width  :max-width
		   :height :min-height :max-height)
		 (multiple-value-list (space-requirement-components sr1))
		 (multiple-value-list (space-requirement-components sr2)))))

;; Combine two space-requirements by adding each corresponding slot.
(defun space-requirement-+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

;; Combine two space requirements, one of which is specified by the
;; keyword arguments (as if they were to MAKE-SPACE-REQUIREMENT).
;; This is supposed to somehow be more efficient than calling
;; MAKE-SPACE-REQUIREMENT on the caller-side, and it probably would be
;; if we inlined and unrolled SPACE-REQUIREMENT-COMBINE, but it also
;; probably would be lost in the noise due to space requirements not
;; changing all that often in normal operation.
(defun space-requirement-+* (space-req &rest args &key
			     width min-width max-width
			     height min-height max-height)
  (declare (ignore width  min-width  max-width
		   height min-height max-height))
  (space-requirement-+ space-req
		       (apply #'make-space-requirement args)))

;;; EOF
