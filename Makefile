all: compiler interpreter tests
	raco make common.rkt rom.rkt dump el.rkt

compiler: compiler/assemble.rkt compiler/bootstream.rkt compiler/compile.rkt compiler/disassemble.rkt compiler/read.rkt
	raco make compiler/assemble.rkt compiler/bootstream.rkt compiler/compile.rkt compiler/disassemble.rkt compiler/read.rkt rom-dump-bootstream.rkt

interpreter: interpreter/interpreter.rkt interpreter/stack.rkt interpreter/ga144.rkt interpreter/f18a.rkt
	raco make interpreter/interpreter.rkt interpreter/stack.rkt interpreter/ga144.rkt interpreter/f18a.rkt

tests: tests/tests.rkt
	raco make tests/tests.rkt

.PHONY: clean check

clean:
	rm -rf compiled/
	rm -rf interpreter/compiled/
	rm -rf compiler/compiled/
	rm -rf interpreter/test-out
	find . -type f -name "*rkt.elc" -exec rm {} \;

check:
	racket tests/tests.rkt
