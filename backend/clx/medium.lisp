;;;
;;; nq-clim/backend/clx/medium
;;;
;;; CLX medium implementation
;;;

(cl:defpackage :nq-clim/backend/clx/medium
  (:use :cl
        :nq-clim/backend/clx/port
        :nq-clim/medium/association
        :nq-clim/medium/basic-medium
        :nq-clim/medium/drawing
        :nq-clim/sheet/mirror-functions)
  (:import-from :xlib)
  (:export
   "CLX-MEDIUM"))
(cl:in-package :nq-clim/backend/clx/medium)


(defclass clx-medium (basic-medium)
  ((drawable :initarg drawable :reader medium-drawable)
   (gcontext :initarg gcontext)))

(defun make-clx-medium (drawable gcontext)
  (make-instance 'clx-medium 'drawable drawable 'gcontext gcontext))


(defmethod allocate-medium ((port clx-port) sheet)
  (declare (optimize (debug 3)))
  (let* ((window (sheet-mirror sheet))
         ;; There are a few dreadful things going on here.  First is
         ;; that we are setting default values for various things to
         ;; suit what is required for our initial test program, until
         ;; such time as we have a "real" interface for setting them.
         ;; Second is that we need to ask the screen for a couple of
         ;; those values, but we can't get the screen when we start
         ;; from a window (sheet-mirror), so we obtain the root window
         ;; for the mirror and then find it as the root window in the
         ;; list of screens associated with the display.  Third is
         ;; that the two root windows are DISJOINT.  They have the
         ;; same resource ID, they're on the same server, accessed
         ;; through the same display connection, but are not EQ!  So
         ;; we compare them specifically by resource ID.  What a
         ;; performance!
         (root-window (xlib:drawable-root window))
         (display (clx-port-display port))
         (screen (find (xlib:drawable-id root-window)
                       (xlib:display-roots display)
                       :key #'(lambda (x) (xlib:drawable-id
                                           (xlib:screen-root x))))))
    (make-clx-medium
     nil
     (xlib:create-gcontext
      :foreground (xlib:screen-black-pixel screen)
      :background (xlib:screen-white-pixel screen)
      :line-width 1 :cap-style :projecting
      :drawable window))))

(defmethod deallocate-medium ((port clx-port) (medium clx-medium))
  (xlib:free-gcontext (slot-value medium 'gcontext)))


(defmethod engraft-medium :after ((medium clx-medium) port sheet)
  ;; FIXME: Set clipping region and transform based on SHEET's
  ;; relation to its mirror.
  (setf (slot-value medium 'drawable) (sheet-mirror sheet)))

(defmethod degraft-medium :after ((medium clx-medium) port sheet)
  (setf (slot-value medium 'drawable) nil))


(defun synchronize-medium (clx-medium)
  (declare (ignore clx-medium)))

(defmethod medium-draw-point* ((medium clx-medium) x y)
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-point drawable gcontext x y)))

(defmethod medium-draw-points* ((medium clx-medium) coord-seq)
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-points drawable gcontext coord-seq)))

(defmethod medium-draw-line* ((medium clx-medium) x1 y1 x2 y2)
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-line drawable gcontext x1 y1 x2 y2)))

(defmethod medium-draw-lines* ((medium clx-medium) coord-seq)
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-segments drawable gcontext coord-seq)))

(defmethod medium-draw-polygon* ((medium clx-medium) coord-seq closed filled)
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (if (and closed (not filled))
        ;; X won't close a polyline automatically, we have to use
        ;; coincident first and last points explicitly.
        (let ((first-point (subseq coord-seq 0 2)))
          (xlib:draw-lines drawable gcontext (append coord-seq first-point)))
        (xlib:draw-lines drawable gcontext coord-seq :fill-p filled))))

(defmethod medium-draw-rectangle* ((medium clx-medium) x1 y1 x2 y2 filled)
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (xlib:draw-rectangle drawable gcontext x1 y1 (- x2 x1) (- y2 y1) filled)))

(defun fix-rectangle-coord-seq-for-x (coord-seq)
  ;; XLIB:DRAW-RECTANGLES takes x, y, width, height, but we have x, y,
  ;; x+width, y+height.
  (loop
     for (x1 y1 x2 y2 . rest) on (coerce coord-seq 'list) by #'cddddr
     collect x1
     collect y1
     collect (- x2 x1)
     collect (- y2 y1)))

(defmethod medium-draw-rectangles* ((medium clx-medium) coord-seq filled)
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (let ((fixed-coord-seq (fix-rectangle-coord-seq-for-x coord-seq)))
      (xlib:draw-rectangles drawable gcontext fixed-coord-seq filled))))

;;; EOF
