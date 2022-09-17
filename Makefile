LISP ?= /usr/bin/sbcl

build:
	$(LISP) --load cl-xrandr-dmenu.asd \
		--eval '(ql:quickload :cl-xrandr-dmenu)' \
		--eval '(asdf:make :cl-xrandr-dmenu)' \
		--eval '(quit)'
