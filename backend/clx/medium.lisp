;;;
;;; nq-clim/backend/clx/medium
;;;
;;; CLX medium implementation
;;;

(cl:defpackage :nq-clim/backend/clx/medium
  (:use :cl
        :nq-clim/backend/clx/port
        :nq-clim/ink/color
        :nq-clim/medium/association
        :nq-clim/medium/basic-medium
        :nq-clim/medium/drawing
        :nq-clim/medium/medium-components
        :nq-clim/sheet/mirror-functions
        :nq-clim/sheet/sheet-with-medium-mixin)
  (:import-from :xlib)
  (:export
   "CLX-MEDIUM"))
(cl:in-package :nq-clim/backend/clx/medium)


(defclass clx-medium (basic-medium)
  ((drawable :initarg drawable :reader medium-drawable)
   (colormap :initarg colormap)
   (gcontext :initarg gcontext)
   (last-ink :initform nil)
   (last-background :initform nil)))

(defun make-clx-medium (drawable colormap gcontext)
  (make-instance 'clx-medium
                 'drawable drawable
                 'colormap colormap
                 'gcontext gcontext))


(defmethod allocate-medium ((port clx-port) (sheet sheet-with-medium-mixin))
  (declare (optimize (debug 3)))
  (let ((window (sheet-mirror sheet)))
    (make-clx-medium
     nil
     (xlib:window-colormap window)
     (xlib:create-gcontext
      :line-width 1 :cap-style :projecting
      :drawable window))))

(defmethod deallocate-medium ((port clx-port) (medium clx-medium))
  (xlib:free-gcontext (slot-value medium 'gcontext)))


(defmethod engraft-medium :after ((medium clx-medium) (port clx-port) (sheet sheet-with-medium-mixin))
  ;; FIXME: Set clipping region and transform based on SHEET's
  ;; relation to its mirror.
  (setf (slot-value medium 'drawable) (sheet-mirror sheet)))

(defmethod degraft-medium :after ((medium clx-medium) (port clx-port) (sheet sheet-with-medium-mixin))
  (setf (slot-value medium 'drawable) nil))


(defun find-x-color (colormap color)
  (multiple-value-bind (red green blue) (color-rgb color)
    (xlib:alloc-color colormap
                      (xlib:make-color :red red :green green :blue blue))))

(defun synchronize-medium (clx-medium)
  (let ((last-ink (slot-value clx-medium 'last-ink))
        (ink (medium-ink clx-medium)))
    (unless (and last-ink
                 (or (eq last-ink ink)
                     (every #'=
                            (multiple-value-list (color-rgb last-ink))
                            (multiple-value-list (color-rgb ink)))))
      (setf (slot-value clx-medium 'last-ink) ink)
      (setf (xlib:gcontext-foreground (slot-value clx-medium 'gcontext))
            (find-x-color (slot-value clx-medium 'colormap) ink))))
  
  (let ((last-background (slot-value clx-medium 'last-background))
        (background (medium-background clx-medium)))
    (unless (and last-background
                 (or (eq last-background background)
                     (every #'=
                            (multiple-value-list (color-rgb last-background))
                            (multiple-value-list (color-rgb background)))))
      (setf (slot-value clx-medium 'last-background) background)
      (setf (xlib:gcontext-background (slot-value clx-medium 'gcontext))
            (find-x-color (slot-value clx-medium 'colormap) background)))))

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

(defmethod medium-draw-polygon* ((medium clx-medium) coord-seq &key (filled t) (closed t))
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (if (and closed (not filled))
        ;; X won't close a polyline automatically, we have to use
        ;; coincident first and last points explicitly.
        (let ((first-point (subseq coord-seq 0 2)))
          (xlib:draw-lines drawable gcontext (append coord-seq first-point)))
        (xlib:draw-lines drawable gcontext coord-seq :fill-p filled))))

(defmethod medium-draw-rectangle* ((medium clx-medium) x1 y1 x2 y2 &key (filled t))
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

(defmethod medium-draw-rectangles* ((medium clx-medium) coord-seq &key (filled t))
  (synchronize-medium medium)
  (with-slots (drawable gcontext)
      medium
    (let ((fixed-coord-seq (fix-rectangle-coord-seq-for-x coord-seq)))
      (xlib:draw-rectangles drawable gcontext fixed-coord-seq filled))))

;;; EOF
