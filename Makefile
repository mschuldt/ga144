all: compiler interpreter tests
	raco make src/common.rkt src/rom.rkt dump src/el.rkt

compiler: src/assemble.rkt src/bootstream.rkt src/compile.rkt src/disassemble.rkt src/read.rkt
	raco make src/assemble.rkt src/bootstream.rkt src/compile.rkt src/disassemble.rkt src/read.rkt src/rom-dump-bootstream.rkt

interpreter: src/interpreter.rkt src/stack.rkt src/ga144.rkt src/f18a.rkt
	raco make src/interpreter.rkt src/stack.rkt src/ga144.rkt src/f18a.rkt

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
