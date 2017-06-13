lisp:
	./ga --byte-compile

install:
	cp -f ga /usr/bin/ga
	cp -f ga-load /usr/bin/ga-load
	mkdir -p /usr/share/emacs/site-lisp/ga144
	cp -r src /usr/share/emacs/site-lisp/ga144

uninstall:
	rm /usr/bin/ga
	rm /usr/bin/ga-load
	rm -rf /usr/share/emacs/site-lisp/ga144

.PHONY: clean check init install lisp

clean:
	rm -rf compiled/
	rm -rf test-out
	find . -type f -name "*.elc" -exec rm {} \;

check:
	racket tests/test-compiler.rkt
	racket tests/tests.rkt
