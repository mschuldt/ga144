all: compiler interpreter tests
	raco make common.rkt rom.rkt dump el.rkt

compiler: assemble.rkt bootstream.rkt compile.rkt disassemble.rkt read.rkt
	raco make assemble.rkt bootstream.rkt compile.rkt disassemble.rkt read.rkt rom-dump-bootstream.rkt

interpreter: interpreter.rkt stack.rkt ga144.rkt f18a.rkt
	raco make interpreter.rkt stack.rkt ga144.rkt f18a.rkt

tests: tests/tests.rkt
	raco make tests/tests.rkt

.PHONY: clean check

clean:
	rm -rf compiled/
	rm -rf test-out
	find . -type f -name "*.elc" -exec rm {} \;

check:
	racket tests/test-compiler.rkt
	racket tests/tests.rkt
