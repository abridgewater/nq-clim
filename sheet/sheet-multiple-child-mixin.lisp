;;;
;;; nq-clim/sheet/sheet-multiple-child-mixin
;;;
;;; From CLIM II 7.2.2.
;;;

(cl:defpackage :nq-clim/sheet/sheet-multiple-child-mixin
  (:use :cl
        :nq-clim/sheet/sheet-hierarchy-protocol
        :nq-clim/sheet/sheet-notification-protocol)
  (:export
   "SHEET-MULTIPLE-CHILD-MIXIN"))
(cl:in-package :nq-clim/sheet/sheet-multiple-child-mixin)

(defclass sheet-multiple-child-mixin ()
  ((children :initform nil :reader sheet-children)))

(defmethod sheet-adopt-child ((sheet sheet-multiple-child-mixin) child)
  ;; Allow the child to veto the adoption (by raising an error) and to
  ;; take care of its own bookkeeping.
  (call-next-method)
  ;; And, now that we're committed to the adoption, add the sheet to
  ;; our list of children.
  (push child (slot-value sheet 'children)))

(defmethod sheet-disown-child ((sheet sheet-multiple-child-mixin) child &key (errorp t))
  (declare (ignore errorp))
  ;; Remove the child from our list of children, or silently do
  ;; nothing if it's not actually on said list.
  (with-slots (children) sheet
    (setf children (delete child children)))
  ;; And allow the child to raise an error if it isn't actually a
  ;; child and to take care of its own bookkeeping.
  (call-next-method))

(defmethod note-sheet-grafted :after ((sheet sheet-multiple-child-mixin))
  (map nil #'note-sheet-grafted (sheet-children sheet)))

(defmethod note-sheet-degrafted :before ((sheet sheet-multiple-child-mixin))
  (map nil #'note-sheet-degrafted (sheet-children sheet)))

;;; EOF
