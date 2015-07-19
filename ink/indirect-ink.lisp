;;;
;;; nq-clim/ink/indirect-ink
;;;
;;; Indirect inks (foreground and background inks, CLIM II 13.6).
;;;

(cl:defpackage :nq-clim/ink/indirect-ink
  (:use :cl)
  (:export
   "+FOREGROUND-INK+"
   "+BACKGROUND-INK+"))
(cl:in-package :nq-clim/ink/indirect-ink)


;; CLIM II 13.6 is spectacularly unspecific about the nature of the
;; constants +FOREGROUND-INK+ and +BACKGROUND-INK+ beyond that they
;; have certain semantics.  CLIM II 13.1 requires them to be a
;; subclass of DESIGN, but we're not doing the "design-based drawing
;; model", so that's a non-starter.  The simplest option is to just
;; have them have themselves as values, and if we need to change this
;; at some point in the future then we can.

(defconstant +foreground-ink+ '+foreground-ink+)
(defconstant +background-ink+ '+background-ink+)

;;; EOF
