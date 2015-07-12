(asdf:defsystem :nq-clim
                :name "NQ-CLIM"
                :version "0.0.1"
                :class :package-inferred-system
                :defsystem-depends-on (:asdf-package-system)
                :depends-on (:nq-clim/clx-interface
                             :nq-clim/backend/clx/medium
                             :nq-clim/backend/clx/mirror
                             :nq-clim/medium/drawing
                             :nq-clim/medium/graphics-method
                             :nq-clim/sheet/sheet-with-medium-mixin
                             :nq-clim/frame/standard-application-frame))

(register-system-packages :clx '(:xlib))
