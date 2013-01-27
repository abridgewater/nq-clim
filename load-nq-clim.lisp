;;;
;;; load-nq-clim.lisp
;;;
;;; A (temporary) way to build and load nq-clim.
;;;

(require :clx)

;; KLUDGE: While I'd much prefer to use something like the quick-build
;; system that I put together for the (SYMBOL-VALUE '*DAYJOB*), for
;; now I'm just going to use a hand-maintained linearization of the
;; file dependencies and a DOLIST.  Aside from the maintenance
;; problems, this also ignores warnings (including style-warnings)
;; from building various files, which is not an idea that I'm in favor
;; of.  I'd far rather have hard build failure in such cases.

(dolist (file '("layout/space-requirement"
                "clx-interface"
                "medium/medium"
                "medium/drawing"
                "medium/basic-medium"
                "geometry/bounding-rectangle-protocol"
                "geometry/coordinate"
                "geometry/region"
                "geometry/rectangle-protocol"
                "geometry/rectangle-api"
                "geometry/standard-bounding-rectangle"
                "geometry/standard-rectangle"
                "geometry/transformation"
                "geometry/transformation-protocol"
                "geometry/bounding-rectangle-api"
                "geometry/identity-transformation"
                "geometry/scaling-transformation"
                "geometry/transformation-composition"
                "sheet/sheet"
                "sheet/sheet-notification-protocol"
                "sheet/sheet-hierarchy-protocol"
                "sheet/sheet-multiple-child-mixin"
                "sheet/sheet-parent-mixin"
                "sheet/sheet-geometry-protocol"
                "sheet/sheet-geometry-mixin"
                "port/port"
                "sheet/mirror-functions"
                "sheet/mirrored-sheet-mixin"
                "port/port-protocol"
                "port/basic-port"
                "port/port-discovery"
                "backend/clx/medium"))
  (load (compile-file file)))

;;; EOF
