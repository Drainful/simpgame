(asdf:defsystem #:simpgame
  :serial t
  :description "A simple roguelike"
  :version "0.0.1"
  :author "Adrian Fullmer"
  :depends-on (#:croatoan
               #:iterate
               #:trivial-gamekit
               #:qtools
               #:qtcore
               #:qtgui)
  :pathname "src"
  :components ((:file "packages")
               (:file "utils/macros")
               (:file "utils/utils")
               (:file "utils/set")
               (:file "utils/vector2")
               (:file "model/game-object")
               (:file "model/region")
               (:file "model/model")
               (:file "model/update")
               (:file "gui-view/view")))
