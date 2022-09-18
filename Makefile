LISP ?= /usr/bin/sbcl

build:
	$(LISP) --load cl-xrandr-dmenu.asd \
		--eval '(ql:quickload :cl-xrandr-dmenu)' \
		--eval '(asdf:make :cl-xrandr-dmenu)' \
		--eval '(quit)'

install:
	install "./bin/xrandr-dmenu" "$(HOME)/.local/bin"

uninstall:
	rm "$(HOME)/.local/bin/xrandr-dmenu"
