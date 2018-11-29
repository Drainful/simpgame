(asdf:defsystem #:simpgame
  :serial t
  :description "A simple roguelike"
  :version "0.0.1"
  :author "Adrian Fullmer"
  :depends-on (#:croatoan
               #:iterate)
  :pathname "src"
  :components ((:file "packages")
               (:file "macros")
               (:file "utils")
               (:file "set")
               (:file "vector2")
               (:file "game-object")
               (:file "region")
               (:file "model")
               (:file "view")
               (:file "update")
               (:file "main")))
