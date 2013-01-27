;;;
;;; nq-clim/sheet/mirror-functions
;;;
;;; CLIM II 9.4.1.
;;;

(cl:defpackage :nq-clim/sheet/mirror-functions
  (:use :cl
        :nq-clim/sheet/sheet
        :nq-clim/sheet/sheet-hierarchy-protocol)
  (:export
   "SHEET-DIRECT-MIRROR"
   "SHEET-MIRRORED-ANCESTOR"
   "SHEET-MIRROR"
   "REALIZE-MIRROR"
   "DESTROY-MIRROR"
   ;; "RAISE-MIRROR"
   ;; "BURY-MIRROR"
   ))
(cl:in-package :nq-clim/sheet/mirror-functions)

(defgeneric sheet-direct-mirror (sheet)
  (:method ((sheet sheet))
    nil))

(defgeneric sheet-mirrored-ancestor (sheet)
  (:method ((sheet sheet))
    (if (sheet-direct-mirror sheet)
        sheet
        (sheet-mirrored-ancestor (sheet-parent sheet)))))

(defgeneric sheet-mirror (sheet)
  (:method ((sheet sheet))
    (or (sheet-direct-mirror sheet)
        (sheet-mirror (sheet-parent sheet)))))

(defgeneric realize-mirror (port mirrored-sheet))

(defgeneric destroy-mirror (port mirrored-sheet))

;; These two functions are oddly-specified, with no useful behavior
;; associated with the oddity.  Ignoring them for now.
;;
;; (defgeneric raise-mirror (port sheet))
;; (defgeneric bury-mirror (port sheet))

#+(or) ;; PORT generic function yet to be defined.
(defmethod port ((sheet sheet))
  nil)

;;; EOF
