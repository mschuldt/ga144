lisp: ga
	rm -f src/*.elc
	./ga --byte-compile

ga:
	./make-ga-script.sh

install:
	cp -f ga /usr/bin/ga
	cp -f ga-load /usr/bin/ga-load
	cp -f ga-sim /usr/bin/ga-sim

uninstall:
	rm /usr/bin/ga
	rm /usr/bin/ga-load
	rm /usr/bin/ga-sim

.PHONY: clean check init install lisp ga

clean:
	rm -rf compiled/
	rm -rf test-out
	find . -type f -name "*.elc" -exec rm {} \;

check:
	racket tests/test-compiler.rkt
	racket tests/tests.rkt
