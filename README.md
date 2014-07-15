This project contains

1. ANS Forth interpreter in ANSForth directory
2. ArrayForth interpreter in ArrayForth directory
3. GreenSyn, GreenArrays F18A synthesizer in machine directory 

GreenSyn
========

GreenSyn is a synthesizer for GreenArrays F18A program. GreenSyn optimizes a given F18A program using inductive synthesis technique. GreenSyn is written in Racket and uses z3 SMT solver. GreenSyn optimizes for the fastest running program. The runtime is estimated by summing runtime of all instructions in the given program without considering instruction fetching time. Programs that we can synthesize do not contain instructions that change the control flow of the program, which are ``; ex jump call next if -if``.
It also cannot synthesize ``!p`` instruction yet. Also, GreenSyn can only synthesize code of one node at a time.

Setting Up
----------
1. Dowanload [z3] and add it to environment path.
2. Download and install [DrRacket].


Running GreenSyn
----------------

* Create a racket file in machine directory.
* Declare the language and import the necessary files.

```
#lang racket
(require "cegis.rkt" "state.rkt")
```

* Simply call function "optimize" to optimize your program.

```
(optimize "1 . . +" #:bin-search `time)
```

* Click "Run" button in DrRacket or run ``racket your_file.rkt`` in the command line. You will see 

```
output program  : "1 . +"
length          : 3
approx. runtime : 8.0

Constants for neighbor ports:
UP = 325, DOWN = 277, LEFT = 373, RIGHT = 469

Time to synthesize: 1 seconds.
```

Approximated runtime is in nanosecond. The synthesizer removes one nop between ``@p`` and ``+``, which reduces the runtime of the program. 

Caution!
--------
### 1. Reads and Writes to Ports
``up`` ``down`` ``left`` and ``right`` are supported. However, multiport read and write and ``io`` are not supported.

The output program will refer to the ports using constants specified by the end of the output message.

### 2. Memory for Data Storage
In the default usage, you can only use entry 1 of the memory for storing data. If you need to use more, please read #:mem subsection in Optional Arguments section.

Database
--------
The superoptimizer stores all the results it found in its database. The default database directory is directory named ".db" under the current directory that you run racket/drracket in. You can change the database directory to a fixed path by editing ``data-dir`` initialization in cache.rkt from 

```
(define data-dir ".db")
```

to

```
(define data-dir "/home/username/path/to/database")
```

Optional Arguments
------------------
You can modify the search criteria by giving some of these arguments.

```
#!racket
(optimize "1 . . +" 
  #:f18a #f
  #:name "increment-by-one" 
  #:mem 1 
  #:init "" 
  #:slots 3 
  #:repeat 1 
  #:constraint (constraint t) 
  #:length-limit 8 
  #:time-limit 1000 
  #:num-bits 4 
  #:inst-pool `no-mem 
  #:bin-search #f)
```
### #:f18a - F18A syntax
When f18a is set to #t, make sure the input program is written in F18A machine code with proper alignment. If slot 4 contains an instruction that cannot be there, the output is undefined.

DEFAULT = #f (ArrayForth syntax)

### #:name - name of the program
This argument does not change the search behavior. It is just the name of the program which is useful for debugging.

### #:mem - number of entries of memory
The bigger mem is the longer the synthesizer takes. Therefore, provide just enough for the program. Note that we only support storing data from memory 0th entry until mem-1'th entry and the program itself is stored starting at mem'th entry. 

DEFAULT = 1

### #:slots - maximum length of the synthesized program
slots can be given as a string when user want to provide a sketch. For example, ``#:slots "_ . + _"`` means the synthesized program contains 4 instructions. The 1st and 4st instructions can be anything. The 2nd instruction is nop, and the 3rd instruction is plus. When given, slots has to be specified in F18A syntax with proper alignment. 

DEFAULT = original program's length

### #:repeat - number of time slots is repeated
When slots is a sketch in form of string. repeat can be used to indicate how many time the sketch is unrolled. For example, ``#:slots "dup _ _ ." #:repeat 3`` means that the actual sketch is ``"dup _ _ . dup _ _ . dup _ _ ."``. When given, repeat has to be specified in F18A syntax with proper alignment. 

DEFAULT = 1

### #:init - additional header sketch 
Init is the additional header sketch that comes before slots. For example, ``#:init "over push - 2*" #:slots "dup _ _ ." #:repeat 3`` means that the actual sketch is ``"over push - 2* dup _ _ . dup _ _ . dup _ _ ."``. 

DEFAULT = ""

### #:constraint - constraint on the output state
Constraint should be set on the registers and/or stacks that contain the output you are looking for. For example, if you want to synthesize x y --> x+y, you might only care that you want register t (the top of th stack) to be equal to x+y and don't care that if other registers and stacks are changed or not. The synthesizer always constraints reads and writes to the neighbor ports.

* Use ``#:constraint constraint-all`` to constrain everything (a b r s t data return memory).
* Use ``#:constraint constraint-none`` to constrain nothing except reads and writes to the neighbor ports.
* Use ``#:constraint (constraint (data n) (return m) <item> ...)`` to constrain data stack, return stack, and <item>. For example, to constraint a and t, use ``#:constraint (constraint a t)``. To constraint memory, use ``#:constraint (constraint memory)``. To constraint on the top 2 elements on data stack, the top element, r, s, and t, use ``#:constraint (constraint (data 2) r s t)``. Note that if you want to specify both ``(data n)`` and ``(return m)``, they have to be put next to each other.

DEFAULT = ``constraint-all``

### #:length-limit - maximum program length
The maximum length (number of slots) of the synthesized program. 

DEFAULT = the original runtime

### #:time-limit - maximum runtime
The maximum runtime in nanosecond of the synthesized program. 

DEFAULT = the original runtime

### #:num-bits - number of bits of a word
DEFAULT = 18

### #:inst-pool - instructions available to compose the synthesized program
* ``#:inst-pool `no-fake`` provides ``@p @+ @b @ !+ !b ! +* 2* 2/ - + and or drop dup pop over a nop push b! a!``
* ``#:inst-pool `no-fake-no-p`` provides `no-fake without ``@p``
* ``#:inst-pool `no-mem`` provides `no-fake without ``@+ @b @ !+ !b !``
* ``#:inst-pool `no-mem-no-p`` provides `no-mem without ``@p``

DEFAULT = ```no-fake``

### #:bin-search - turn on or off binary search on the program length
When slots is a number. In the default setting, We perform binary search the length of the synthesized program. For example, if slots is 8, we will start searching for a program whose length is 4. If we find an equivalent program, we will search on length 2. If not, we will search on length 6. The process keeps going like normal binary search. If bin-search is set to ```time``, it will perform binary search on approximate running time instead of program length. If bin-search is set to false, it will always search program whose length is equal to slots, but it is still optimizing for the shortest program in the sense that nop at the end does not count toward the length.

DEFAULT = ```length``

## Examples
Many examples can be found in examples.rkt.

## Bug Reports and Questions
Please contact mangpo [at] eecs.berkeley.edu

[DrRacket]:http://racket-lang.org/download/
[z3]:http://z3.codeplex.com/releases

