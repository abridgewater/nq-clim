;;;
;;; rendering_1.lisp
;;;
;;; Sample implementation of the first rendering model
;;; for the Dungeon Crawl Project.
;;;
;;; Alastair Bridgewater, October 9th, 2004.
;;;
;;; Damaged again in may 2009.
;;;
;;; Damaged yet again in December, 2012.
;;;

(defpackage :rendering-1
  (:use :common-lisp
        :nq-clim/backend/clx/port
        :nq-clim/frame/application-frame-functions
        :nq-clim/medium/association
        :nq-clim/medium/drawing
        :nq-clim/medium/graphics-method
        :nq-clim/port/port
        :nq-clim/sheet/basic-sheet
        :nq-clim/sheet/mirror-functions
        :nq-clim/sheet/permanent-medium-sheet-output-mixin
        :nq-clim/layout/space-requirement
        :nq-clim/frame/standard-application-frame
        :nq-clim/clx-interface)
  (:export "START-EXAMPLE"))
(in-package :rendering-1)


(defparameter *raw-map-data*
  '("xxxxxxxxxxxxxxxx"
    "x.xxxxxxxxx...xx"
    "x....x.....xx..x"
    "x.xx...xxx.xxx.x"
    "x.xxxxxxxx...x.x"
    "x......xxxxx.x.x"
    "xxxxxx.xxxxx.x.x"
    "xxxxx....xx..x.x"
    "xxxxx.xx.xx.x..x"
    "xx....xxxxx.x.xx"
    "xx.xx...xxx.x..x"
    "xx.xxxx...x.xx.x"
    "xx.xxxxxx...xx.x"
    "xx.xx....xxx...x"
    "xx....xx.....xxx"
    "xxxxxxxxxxxxxxxx")
  "The raw map data, in easily editable form.")

(declaim (type (simple-array t (#x100)) *map-data*))
(defvar *map-data* (make-array #x100)
  "Map data. Each cell is either T for a wall or NIL for empty space.")

(defvar *position* #x11 "Position of player within *map-data*.")
(defvar *facing* :south "Direction player is facing.")
(defvar *frontstep* 0)
(defvar *leftstep* 0)

(defparameter *frontstep-list* '(:north -16 :south 16 :west -1 :east 1)
  "alist from directions to index change within map data to move forward.")
(defparameter *leftstep-list* '(:north -1 :south 1 :west 16 :east -16)
  "alist from directions to index change within map data to move left.")
(defparameter *leftturn-list* '(:north :west :west :south :south :east :east :north)
  "alist from direction to direction for turning left.")
(defparameter *rightturn-list* '(:north :east :east :south :south :west :west :north)
  "alist from direction to direction for turning right.")


(defvar *medium* nil "The CLIM MEDIUM we draw on.")


(defclass maze-pane (basic-sheet permanent-medium-sheet-output-mixin)
  ())

(defun init-map-data ()
  "Convert the raw map data in *raw-map-data* to the internal representation in *map-data*."
  (let ((row-number 0))
    (dolist (row-data *raw-map-data*)
      (dotimes (i 16)
	(setf (aref *map-data* (+ i (* 16 row-number)))
	      (char= (aref row-data i) #\x)))
      (incf row-number)))
  (values))

(defun set-facing (direction)
  "Set the player to be facing in DIRECTION. Sets up *frontstep* and *leftstep* for rendering and motion control."
  (setf *facing* direction)
  (setf *frontstep* (getf *frontstep-list* direction))
  (setf *leftstep* (getf *leftstep-list* direction)))

(defun turn-left ()
  "Turn the player 90 degrees to the left."
  (set-facing (getf *leftturn-list* *facing*)))

(defun turn-right ()
  "Turn the player 90 degrees to the right."
  (set-facing (getf *rightturn-list* *facing*)))

(defun move-forward ()
  "Move the player one space forward if there is no wall ahead."
  (if (not (aref *map-data* (+ *position* *frontstep*)))
      (setf *position* (+ *position* *frontstep*))))


(defun draw-left-side (medium position base size)
  (if (aref *map-data* (+ position *leftstep*))
      (progn
	;; There is a wall to the left of this position, so we draw it.
	(draw-line* medium base base (+ base size) (+ base size))
	(draw-line* medium base (- 255 base) (+ base size) (- 255 base size)))
      (progn
        ;; There is no wall to the left of this position, so there is one
        ;; ahead of it. We draw that one.
        (draw-line* medium base (+ base size) (+ base size) (+ base size))
        (draw-line* medium base (- 255 base size) (+ base size) (- 255 base size))))

  ;; Draw the vertical line for this wall segment.
  (draw-line* medium (+ base size) (+ base size) (+ base size) (- 255 base size)))

(defun draw-right-side (medium position base size)
  (if (aref *map-data* (- position *leftstep*))
      (progn
	;; There is a wall to the right of this position, so we draw it.
	(draw-line* medium (- 255 base) base (- 255 base size) (+ base size))
	(draw-line* medium (- 255 base) (- 255 base) (- 255 base size) (- 255 base size)))
      (progn
	;; There is no wall to the right of this position, so there is one
	;; ahead of it. We draw that one.
	(draw-line* medium (- 255 base) (+ base size) (- 255 base size) (+ base size))
	(draw-line* medium (- 255 base) (- 255 base size) (- 255 base size) (- 255 base size))))
  
  ;; Draw the vertical line for this wall segment.
  (draw-line* medium (- 255 base size) (+ base size) (- 255 base size) (- 255 base size)))

(defun draw-maze (medium)
  "Draw the maze as seen from the player's current position and facing."
  (let ((base 0)
	(position *position*))
    (dotimes (depth 4)
      ;; size values determined empirically.
      (let ((size (elt '(10 50 40 15) depth)))
	(draw-left-side medium position base size)
	(draw-right-side medium position base size)
	
	(incf position *frontstep*)
	(incf base size)
	
	;; Draw the facing wall if there is one.
	(when (aref *map-data* position)
	  (draw-line* medium base base (- 255 base) base)
	  (draw-line* medium base (- 255 base) (- 255 base) (- 255 base))
	  (return-from draw-maze)))))
  (values))


(defun force-redraw (medium)
  (draw-rectangle* medium 0 0 256 256 :ink nq-clim/ink/standard-color:+white+)
  (draw-maze medium))

(defun handle-key-press (frame key-code)
  (let* ((display (clx-port-display (port (frame-panes frame))))
         (keysym (xlib:keycode->keysym display key-code 0)))
    (declare (integer keysym))
    (cond
      ;; For some reason, the keysyms I need aren't defined in CLX.
      ((= keysym +xk-left+)  (turn-left)    (force-redraw *medium*))
      ((= keysym +xk-right+) (turn-right)   (force-redraw *medium*))
      ((= keysym +xk-up+)    (move-forward) (force-redraw *medium*)))))

(defgeneric event-type (event))

(defmethod event-type ((event cons))
  "Compatibility shim for CLX event plists"
  (getf event :event-key))

(defun convert-clx-event (&rest event-plist)
  (let ((type (getf event-plist :event-key)))
    (case type
      (otherwise
       (apply #'list event-plist)))))

(defun read-event (frame)
  ;; FIXME: Ideally, we'd call PORT on the FRAME directly, but that
  ;; requires a working frame-manager class.
  (let ((display (clx-port-display (port (frame-panes frame)))))
    (xlib:process-event display :handler #'convert-clx-event)))

(defun handle-one-event (frame)
  (let ((event (read-event frame)))
    (case (event-type event)
      (:exposure
       (draw-maze *medium*))
      (:button-release
       (throw '%exit-event-loop nil))
      (:key-press
       (let ((code (getf event :code)))
         (handle-key-press frame code))))))

(defun run-event-loop (frame)
  (catch '%exit-event-loop
    (loop
       (handle-one-event frame))))

(defun start-example ()
  "run the example renderer, connecting to an X display on HOST."
  (init-map-data)
  (setf *position* #x11)
  (set-facing :south)
  (let ((frame (make-instance 'standard-application-frame
                              :pretty-name "Dungeon Crawl -- Rendering 1"))
        (pane (make-instance 'maze-pane)))
    (setf (frame-panes frame) pane)
    (with-x11-display (:space-requirement
                       (make-space-requirement
                        :width 256 :height 256
                        :min-width 256 :min-height 256
                        :max-width 256 :max-height 256)
                       :frame frame)
      (setf *medium* (sheet-medium pane))
      (setf (xlib:window-event-mask (sheet-mirror pane))
            (xlib:make-event-mask :button-press :button-release
                                  :exposure :key-press))
      (run-event-loop frame)))
  (values))

;;; EOF
