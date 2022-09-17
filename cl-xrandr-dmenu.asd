(asdf:defsystem :cl-xrandr-dmenu
  :author "Lou Woell <lou.woell@posteo.de>"
  :license "GPLv3"
  :serial t
  :version "1.0.0"
  :description "A dmenu interface for xrandr written in Common Lisp"
  :build-operation "program-op"
  :build-pathname "./bin/xrandr-dmenu"
  :entry-point "cl-xrandr-dmenu:main"
  :depends-on (:adopt
               :uiop
               :str)
  :components ((:file "packages")
               (:file "xrandr")))
