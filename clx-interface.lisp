;;;
;;; clx-interface.lisp
;;;
;;; Interface to CLX for gaming stuff.
;;;

(cl:defpackage :game-stuff/clx-interface
  (:use :cl
        :nq-clim/layout/space-requirement
        :nq-clim/port/port-discovery
        :nq-clim/port/port-protocol
        :nq-clim/backend/clx/port)
  (:export
   "*PORT*"
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

(defvar *port* nil "The CLIM PORT.")
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
  (setf *port*
        (find-port :server-path `(:clx ,@(when display-name
                                               `(:display ,display-name)))))
  (setf *display* (clx-port-display *port*))
  (let* ((screen (xlib:display-default-screen *display*))
	 (root (xlib:screen-root screen))
	 (width (or (and space-requirement
			 (space-requirement-width space-requirement))
		    *default-window-width*))
	 (height (or (and space-requirement
			  (space-requirement-height space-requirement))
		     *default-window-height*))
	 (window (xlib:create-window :parent root
				     :x 0 :y 0 :width width :height height)))
    (setf *window* window)

    (setf (xlib:window-background *window*)
          (xlib:screen-white-pixel
           (xlib:display-default-screen
            (clx-port-display *port*))))

    (setf (xlib:window-event-mask *window*)
          (xlib:make-event-mask :exposure))

    (setf (xlib:wm-name window)
	  (or window-title *default-window-title*))

    (when space-requirement
      (set-window-space-requirement window space-requirement))
    (xlib:map-window window)))

(defun close-display ()
  ;; It is sufficient to drop the reference to *window*, as closing
  ;; the display automatically releases all server resources.
  (setf *window* nil)
  (setf *display* nil)
  (when *port*
    (destroy-port *port*))
  (setf *port* nil))

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
