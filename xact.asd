(defsystem "xact"
  :description "Small interactors for XCB"
  :author "Arthur Miller <arthur.miller@live.com>"
  :license "GPLv3+"
  :version "0.0.1"
  :depends-on ("clxcb")
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "events")
                             (:file "state")
                             (:file "interactor")
                             (:file "widget")
                             (:file "keymap")
                             (:file "layout")
                             (:module "backend"
                              :serial t
                              :components ((:file "xcb")
                                           (:file "toplevel")
                                           (:file "screen-picker")))
                             (:module "mixins"
                              :serial t
                              :components ((:file "continuous")
                                           (:file "screen-picker")))
                             (:module "interactors"
                              :serial t
                              :components ((:file "resize")
                                           (:file "quit")
                                           (:file "drag")
                                           (:file "window")
                                           (:file "scale")))
                             (:module "widgets"
                              :serial t
                              :components ((:file "slider")))))))

(defsystem "xact/color-picker"
  :description "A colour picker widget for xact, with TrueType text"
  :author "Arthur Miller <arthur.miller@live.com>"
  :license "GPLv3+"
  :depends-on ("xact" "clxcb-truetype" "trivial-garbage")
  :components ((:module "src"
                :components ((:module "widgets"
                              :components ((:file "color-picker")))))))
