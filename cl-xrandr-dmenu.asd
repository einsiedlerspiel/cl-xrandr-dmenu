;; Copyright (C) 2023  Lou Woell

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
