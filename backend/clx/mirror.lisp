;;;
;;; nq-clim/backend/clx/mirror
;;;
;;; CLX mirrored sheet support
;;;

(cl:defpackage :nq-clim/backend/clx/mirror
  (:use :cl
        :nq-clim/backend/clx/port
        :nq-clim/sheet/mirror-functions
        :nq-clim/geometry/bounding-rectangle-protocol
        :nq-clim/sheet/sheet-hierarchy-protocol))
(cl:in-package :nq-clim/backend/clx/mirror)


(defmethod realize-mirror ((port clx-port) mirrored-sheet)
  (let ((parent-mirror (realize-mirror port (sheet-parent mirrored-sheet))))
    (multiple-value-bind (min-x min-y max-x max-y)
        (bounding-rectangle* mirrored-sheet)
      (xlib:create-window :parent parent-mirror
                          :x min-x
                          :y min-y
                          :width (- max-x min-x)
                          :height (- max-y min-y)))))

;;; EOF