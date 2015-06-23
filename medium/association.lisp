;;;
;;; nq-clim/medium/association
;;;
;;; Much of CLIM II 8.3.4 and 8.3.4.1
;;;

(cl:defpackage :nq-clim/medium/association
  (:use :cl)
  (:export
   "SHEET-MEDIUM"
   "MEDIUM-SHEET"
   "MEDIUM-DRAWABLE"
   "ALLOCATE-MEDIUM"
   "DEALLOCATE-MEDIUM"
   "ENGRAFT-MEDIUM"
   "DEGRAFT-MEDIUM"))
(cl:in-package :nq-clim/medium/association)


(defgeneric sheet-medium (sheet)
  (:documentation
   "The medium associated with SHEET, or NIL if SHEET does not have a medium.  Only applies to SHEET-WITH-MEDIUM-MIXIN."))

(defgeneric medium-sheet (medium)
  (:documentation
   "The sheet associated with MEDIUM, or NIL if MEDIUM is not grafted."))

(defgeneric medium-drawable (medium)
  (:documentation
   "The mirror (or pixmap, for pixmap mediums) that MEDIUM draws to, or NIL if MEDIUM is not grafted or MEDIUM's sheet is not mirrored."))

(defgeneric allocate-medium (port sheet)
  (:documentation
   "Obtain a medium suitable for use on PORT, with default characteristics determined by SHEET."))

(defgeneric deallocate-medium (port medium)
  (:documentation
   "Return MEDIUM to PORT for deallocation or re-use."))

(defgeneric engraft-medium (medium port sheet)
  (:documentation
   "Associate MEDIUM to SHEET on PORT."))

(defgeneric degraft-medium (medium port sheet)
  (:documentation
   "Disassociate MEDIUM from SHEET on PORT."))

;;; EOF
