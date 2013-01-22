;;;
;;; nq-clim/sheet/sheet-hierarchy-protocol
;;;
;;; Parts of CLIM II 7.2.1.
;;;

(cl:defpackage :nq-clim/sheet/sheet-hierarchy-protocol
  (:use :cl
        :nq-clim/sheet/sheet)
  (:export
   "SHEET-ALREADY-HAS-PARENT"
   "SHEET-ALREADY-HAS-PARENT-SHEET"
   "SHEET-IS-NOT-CHILD"
   "SHEET-IS-NOT-CHILD-PARENT"
   "SHEET-IS-NOT-CHILD-SHEET"
   "SHEET-PARENT"
   "SHEET-CHILDREN"
   "SHEET-ADOPT-CHILD"
   "SHEET-DISOWN-CHILD"
   "SHEET-SIBLINGS"
   "SHEET-ENABLED-CHILDREN"
   "SHEET-ANCESTOR-P"
   ;; "RAISE-SHEET"
   ;; "BURY-SHEET"
   ;; "REORDER-SHEETS"
   "SHEET-ENABLED-P"
   "SHEET-VIEWABLE-P"
   ;; "SHEET-OCCLUDING-SHEETS"
   "MAP-OVER-SHEETS"))
(cl:in-package :nq-clim/sheet/sheet-hierarchy-protocol)

;; In general, we're just specifying the generic functions and errors,
;; and providing default implementations for the SHEET class where
;; appropriate.  Full implementations of various aspects of this
;; protocol will be provided by SHEET-PARENT-MIXIN and
;; SHEET-MULTIPLE-CHILD-MIXIN.

;; FIXME: Add docstring and printer.
(define-condition sheet-already-has-parent (error)
  ((sheet :initarg :sheet :reader sheet-already-has-parent-sheet)))

;; FIXME: Add docstring and printer.
(define-condition sheet-is-not-child (error)
  ((parent :initarg :parent :reader sheet-is-not-child-parent)
   (sheet :initarg :sheet :reader sheet-is-not-child-sheet)))

(defgeneric sheet-parent (sheet)
  (:documentation
   "Return the parent sheet of SHEET, or NIL if SHEET has no parent.")
  (:method ((sheet sheet))
    nil))

(defgeneric sheet-children (sheet)
  (:documentation
   "Return a list of the child sheets of SHEET; this list must not be
   modified.")
  (:method ((sheet sheet))
    nil))

(defgeneric sheet-adopt-child (sheet child)
  (:documentation
   "Make the sheet SHEET the parent of the sheet CHILD, updating the
   list of child sheets of SHEET and the parent of CHILD.")
  (:method ((sheet sheet) child)
    (error "~S cannot adopt ~S because it does not support children"
           sheet child))
  (:method (sheet (child sheet))
    (error "~S cannot be adopted by ~S because it does not accept parenting"
           child sheet)))

(defgeneric sheet-disown-child (sheet child &key errorp)
  (:documentation
   "Makes the sheet SHEET no longer the parent of the sheet CHILD,
   updating the list of child sheets of SHEET and the parent of CHILD.
   If CHILD is not a child of SHEET to begin with and ERRORP is
   true, SHEET-IS-NOT-CHILD will be signalled.")
  (:method ((sheet sheet) child &key (errorp t))
    (when errorp
      (error 'sheet-is-not-child :parent sheet :sheet child)))
  (:method (sheet (child sheet) &key (errorp t))
    (when errorp
      (error 'sheet-is-not-child :parent sheet :sheet child))))

(defgeneric sheet-siblings (sheet)
  (:documentation
   "Return a list of the children of the parent sheet of SHEET,
   excluding SHEET itself.  This list will be freshly consed, and may
   be modified by the caller.")
  (:method ((sheet sheet))
    (let ((parent (sheet-parent sheet)))
      (when parent
        (remove sheet (sheet-children parent))))))

(defgeneric sheet-ancestor-p (sheet putative-ancestor)
  (:documentation
   "Return true iif the sheet PUTATIVE-ANCESTOR is an ancestor sheet
   of the sheet SHEET.")
  (:method ((sheet sheet) (putative-ancestor sheet))
    (let ((parent (sheet-parent sheet)))
      (when parent
        (or (eq parent putative-ancestor)
            (sheet-ancestor-p parent putative-ancestor))))))

;; Skipping the sheet ordering stuff for now.

(defgeneric sheet-enabled-p (sheet)
  ;; NOTE: We cannot provide a generic implementation, as this
  ;; requires slot storage.
  (:documentation
   "Return true iif the sheet SHEET is enabled with respect to its
   parent."))

(defgeneric (setf sheet-enabled-p) (enabled-p sheet)
  ;; NOTE: We cannot provide a generic implementation, as this
  ;; requires slot storage.
  (:documentation
   "Enable or disable the sheet SHEET based on the generalized boolean
   ENABLED-P."))

(defgeneric sheet-viewable-p (sheet)
  (:documentation
   "Return true iif the sheet SHEET is enabled, all of its ancestors
   are enabled, and one of its ancestors is a graft.")
  (:method ((sheet sheet))
    (when (sheet-enabled-p sheet)
      (let ((parent (sheet-parent sheet)))
        (when parent
          (sheet-viewable-p parent))))))

(defgeneric sheet-enabled-children (sheet)
  (:documentation
   "Return a list of children of the sheet SHEET which are enabled.
   This list will be freshly consed, and may be modified by the
   caller.")
  (:method ((sheet sheet))
    (remove-if-not #'sheet-enabled-p (sheet-children sheet))))

;; SHEET-OCCLUDING-SHEETS might go here, but it seems to require parts
;; of the sheet geometry protocol.

(defgeneric map-over-sheets (function sheet)
  (:documentation
   "Calls FUNCTION for the entire sheet hierarchy starting at SHEET,
   passing each sheet as the single parameter to FUNCTION.")
  ;; This is fun: CLIM II 7.2.1 says that SHEET is the first sheet to
  ;; be passed to FUNCTION, but doesn't explicitly specify any further
  ;; order.  However, the description of "descendents" of SHEET
  ;; suggests breadth-first search.  We'll go with depth-first.
  (:method (function (sheet sheet))
    (funcall function sheet)
    (map 'nil
         (lambda (child)
           (map-over-sheets function child))
         (sheet-children sheet))))

;;; EOF
