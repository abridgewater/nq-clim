;;;
;;; nq-clim/backend/clx/graft
;;;
;;; CLX graft implementation
;;;

(cl:defpackage :nq-clim/backend/clx/graft
  (:use :cl
        :nq-clim/geometry/identity-transformation
        :nq-clim/sheet/sheet
        :nq-clim/sheet/sheet-multiple-child-mixin
        :nq-clim/sheet/mirrored-sheet-mixin
        :nq-clim/sheet/sheet-geometry-mixin
        :nq-clim/sheet/sheet-geometry-protocol
        :nq-clim/sheet/mirror-functions
        :nq-clim/backend/clx/port)
  (:export "CLX-GRAFT"
           "MAKE-CLX-GRAFT"))
(cl:in-package :nq-clim/backend/clx/graft)

;; The superclass list here is subject to revision, as I'm still
;; trying to find my way through the mess of mixins and
;; responsibilities in the sheet classes and protocols.
(defclass clx-graft (sheet
                     sheet-multiple-child-mixin
                     mirrored-sheet-mixin
                     sheet-geometry-mixin)
  ())

(defmethod realize-mirror ((port clx-port) (mirrored-sheet clx-graft))
  ;; This is a bit of a KLUDGE: We presume that there is only one
  ;; graft for a CLX port, and that it is for the default screen root.
  ;; We also presume that our geometry has already been set correctly,
  ;; and that all we have to do is to return the root window for the
  ;; default screen.
  (xlib:screen-root
   (xlib:display-default-screen
    (clx-port-display port))))

(defun make-clx-graft (clx-port)
  (let* ((graft (make-instance 'clx-graft))
         (display (clx-port-display clx-port))
         (screen (xlib:display-default-screen display))
         (width (xlib:screen-width screen))
         (height (xlib:screen-height screen)))
    ;; There's no defined way to set the sheet geometry on creation,
    ;; it has to be done post-facto by setting the region and
    ;; transformation explicitly.  We can't use MOVE-SHEET or its
    ;; derivative MOVE-AND-RESIZE-SHEET as they use the existing
    ;; transformation and bias it by a translation, and there is no
    ;; existing transformation.
    (setf (sheet-transformation graft) +identity-transformation+)

    ;; We can, however, use RESIZE-SHEET, since it simply sets the
    ;; region without reference to the existing region.
    (resize-sheet graft width height)

    ;; Force the graft to be associated with the screen root window.
    (realize-mirror clx-port graft)

    graft))

;;; EOF
