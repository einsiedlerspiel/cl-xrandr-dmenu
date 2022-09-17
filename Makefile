LISP ?= /usr/bin/sbcl

build:
	$(LISP) --load cl-xrandr.asd \
		--eval '(ql:quickload :cl-xrandr)' \
		--eval '(asdf:make :cl-xrandr)' \
		--eval '(quit)'
