;;;
;;; clx-interface.lisp
;;;
;;; Interface to CLX for gaming stuff.
;;;

(cl:defpackage :game-stuff/clx-interface
  (:use :cl :nq-clim/layout/space-requirement)
  (:export
   "*DISPLAY*"
   "*WINDOW*"
   "WITH-X11-DISPLAY"

   ;; For some reason, these don't appear to be defind in CLX.
   "+XK-UP+"
   "+XK-DOWN+"
   "+XK-LEFT+"
   "+XK-RIGHT+"
   "+XK-ESC+"
   "+XK-ENTER+"))
(cl:in-package :game-stuff/clx-interface)

;;; Important external variables.

(defvar *display* nil "The X display connection.")
(defvar *window* nil "The X window we draw in.")

;; For some reason, CLX doesn't appear to have these keysyms defined.
(defconstant +xk-up+    #xff52)
(defconstant +xk-left+  #xff51)
(defconstant +xk-right+ #xff53)
(defconstant +xk-down+  #xff54)
(defconstant +xk-esc+   #xff1b)
(defconstant +xk-enter+ #xff0d)

;; Internal default values.

(defparameter *default-window-height* 240)
(defparameter *default-window-width* 256)
(defparameter *default-window-title* "CLX Interface Window")


(defun set-window-space-requirement (window space-requirement)
  (multiple-value-bind
        (width min-width max-width height max-height min-height)
      (space-requirement-components space-requirement)
    (setf (xlib:wm-normal-hints window)
          (xlib:make-wm-size-hints
           :width  width  :min-width  min-width  :max-width  max-width
           :height height :min-height min-height :max-height max-height))))

(defun init-display (&key display-name space-requirement window-title)
  (setf *display* (xlib:open-default-display display-name))
  (let* ((screen (xlib:display-default-screen *display*))
	 (root (xlib:screen-root screen))
	 (black-pixel (xlib:screen-black-pixel screen))
         (white-pixel (xlib:screen-white-pixel screen))
	 (width (or (and space-requirement
			 (space-requirement-width space-requirement))
		    *default-window-width*))
	 (height (or (and space-requirement
			  (space-requirement-height space-requirement))
		     *default-window-height*))
	 (window (xlib:create-window :parent root
				     :x 0 :y 0 :width width :height height
				     :background white-pixel
				     :border black-pixel
				     :event-mask '(:exposure))))
    (setf *window* window)

    (setf (xlib:wm-name window)
	  (or window-title *default-window-title*))

    (when space-requirement
      (set-window-space-requirement window space-requirement))
    (xlib:map-window window)))

(defun close-display ()
  ;; It is sufficient to drop the reference to *window*, as closing
  ;; the display automatically releases all server resources.
  (setf *window* nil)
  (when *display*
    (xlib:close-display *display*))
  (setf *display* nil))

(defun call-with-x11-display (fun &key display-name space-requirement
			      window-title)
  (unwind-protect
       (progn
	 (init-display :display-name display-name
		       :space-requirement space-requirement
		       :window-title window-title)
	 (funcall fun))
    (close-display)))

(defmacro with-x11-display ((&key display-name space-requirement window-title)
			    &body body)
  `(call-with-x11-display (lambda () ,@body)
			  :display-name ,display-name
			  :space-requirement ,space-requirement
			  :window-title ,window-title))

;;; EOF
