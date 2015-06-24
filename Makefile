all: compiler interpreter tests
	raco make common.rkt

compiler: compiler/assemble.rkt compiler/bootstream.rkt compiler/compile.rkt compiler/disassemble.rkt compiler/read.rkt
	raco make compiler/assemble.rkt compiler/bootstream.rkt compiler/compile.rkt compiler/disassemble.rkt compiler/read.rkt

interpreter: interpreter/interpreter.rkt interpreter/stack.rkt interpreter/ga144.rkt interpreter/f18a.rkt
	raco make interpreter/interpreter.rkt interpreter/stack.rkt interpreter/ga144.rkt interpreter/f18a.rkt

tests: interpreter/test.rkt interpreter/tests.rkt
	raco make interpreter/test.rkt interpreter/tests.rkt

.PHONY: clean check

clean:
	rm -rf compiled/
	rm -rf interpreter/compiled/
	rm -rf compiler/compiled/
	rm -rf interpreter/test-out

check:
	racket interpreter/tests.rkt
