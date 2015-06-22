;;;
;;; clx-interface.lisp
;;;
;;; Interface to CLX for gaming stuff.
;;;

(cl:defpackage :nq-clim/clx-interface
  (:use :cl
        :nq-clim/frame/application-frame-functions
        :nq-clim/frame/manageable-frame-functions
        :nq-clim/layout/space-requirement
        :nq-clim/sheet/mirror-functions
        :nq-clim/sheet/sheet-geometry-protocol
        :nq-clim/sheet/sheet-hierarchy-protocol
        :nq-clim/geometry/identity-transformation
        :nq-clim/port/port-discovery
        :nq-clim/port/port-protocol
        :nq-clim/backend/clx/port
        :nq-clim/backend/clx/graft
        :nq-clim/backend/clx/frame-sheet)
  (:import-from :xlib)
  (:export
   "*PORT*"
   "*GRAFT*"
   "*DISPLAY*"
   "*SHEET*"
   "*WINDOW*"
   "WITH-X11-DISPLAY"

   ;; For some reason, these don't appear to be defind in CLX.
   "+XK-UP+"
   "+XK-DOWN+"
   "+XK-LEFT+"
   "+XK-RIGHT+"
   "+XK-ESC+"
   "+XK-ENTER+"))
(cl:in-package :nq-clim/clx-interface)

;;; Important external variables.

(defvar *port* nil "The CLIM PORT.")
(defvar *graft* nil "The CLIM GRAFT.")
(defvar *sheet* nil "The CLIM SHEET.")
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

(defun init-display (&key display-name space-requirement frame)
  (setf *port*
        (find-port :server-path `(:clx ,@(when display-name
                                               `(:display ,display-name)))))
  (setf *display* (clx-port-display *port*))
  ;; KLUDGE: NOT the defined right way to obtain a graft, but it's
  ;; what we have available at the moment.
  (setf *graft* (make-clx-graft *port*))

  (setf *sheet* (make-instance 'clx-frame-sheet
                               :frame frame))
  (setf (sheet-transformation *sheet*) +identity-transformation+)
  (let ((width (or (and space-requirement
                        (space-requirement-width space-requirement))
                   *default-window-width*))
        (height (or (and space-requirement
                         (space-requirement-height space-requirement))
                    *default-window-height*)))
    (resize-sheet *sheet* width height))
  (sheet-adopt-child *graft* *sheet*)

  (setf (frame-top-level-sheet frame) *sheet*)

  (setf *window* (sheet-mirror *sheet*))

  (setf (xlib:window-background *window*)
        (xlib:screen-white-pixel
         (xlib:display-default-screen
          (clx-port-display *port*))))

  (setf (xlib:window-event-mask *window*)
        (xlib:make-event-mask :exposure))

  (setf (xlib:wm-name *window*)
        (or (and frame (frame-pretty-name frame)) *default-window-title*))

  (when space-requirement
    (set-window-space-requirement *window* space-requirement))
  (xlib:map-window *window*))

(defun close-display ()
  ;; It is sufficient to drop the reference to *window*, as closing
  ;; the display automatically releases all server resources.
  (setf *window* nil)
  (setf *display* nil)
  (setf *sheet* nil)
  (setf *graft* nil)
  (when *port*
    (destroy-port *port*))
  (setf *port* nil))

(defun call-with-x11-display (fun &key display-name space-requirement
			      frame)
  (unwind-protect
       (progn
	 (init-display :display-name display-name
		       :space-requirement space-requirement
		       :frame frame)
	 (funcall fun))
    (close-display)))

(defmacro with-x11-display ((&key display-name space-requirement frame)
			    &body body)
  `(call-with-x11-display (lambda () ,@body)
			  :display-name ,display-name
			  :space-requirement ,space-requirement
			  :frame ,frame))

;;; EOF
