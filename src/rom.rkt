#lang racket ;; -*- lexical-binding: t -*-

(require "el.rkt")

(provide (all-defined-out))

;; block 1418  math rom anywhere
(defconst basic-rom '(("relay" . #xa1) ;; 1388
                      ("warm" . #xa9)  ;; warm
                      ("*.17" . #xb0) ;; 1390  multiply
                      ("*." . #xb7) ;; 1396  fractional multiply
                      ("taps" . #xbc) ;; 1386
                      ("interp" . #xc4) ;; 1384  interpolate
                      ("triangle" . #xce) ;; 1394
                      ("clc" . #xd3) ;; 1398
                      ("--u/mod" . #x2d5)  ;; 1398
                      ("-u/mod" . #x2d6) ;; 1398
                      ("poly" . #xaa) ;; 1382  polynomial approximation
                      ))

;; block 1432  analog
(defconst analog-rom '(("relay" . #xa1) ;; 1388
                       ("warm" . #xa9)  ;; warm
                       ("*.17" . #xb0) ;; 1390  multiply
                       ("*." . #xb7) ;; 1396  fractional multiply
                       ("-dac" . #xbc) ;; 1434
                       ("interp" . #xc4) ;; 1384  interpolate
                       ("triangle" . #xce) ;; 1394
                       ("clc" . #xd3) ;; 1398
                       ("--u/mod" . #x2d5)  ;; 1398
                       ("-u/mod" . #x2d6) ;; 1398
                       ("poly" . #xaa) ;; 1382  polynomial approximation
                       ))

;; block 1420  serdes top/bot
(defconst serdes-boot-rom '(("relay" . #xa1) ;; 1388
                            ("warm" . #xa9)
                            ("cold" . #xaa)
                            ("*.17" . #xb0) ;; 1390  multiply
                            ("*." . #xb7) ;; 1396  fractional multiply
                            ("taps" . #xbc) ;; 1386
                            ("interp" . #xc4) ;; 1384  interpolate
                            ("triangle" . #xce) ;; 1394
                            ("clc" . #xd3) ;; 1398
                            ("--u/mod" . #x2d5)  ;; 1398
                            ("-u/mod" . #x2d6) ;; 1398
                            ("poly" . #xaa) ;; 1382  polynomial approximation
                            ))

;; block 1422  sync serial boot side
(defconst sync-boot-rom '(("relay" . #xa1) ;; 1388
                          ("warm" . #xa9)
                          ("cold" . #xaa)
                          ("ser-exec" . #xb6)
                          ("ser-copy" . #xb9)
                          ("sget" . #xbe)
                          ("6in" . #xc0)
                          ("2in" . #xc2)
                          ("*.17" . #xcc) ;; 1390  multiply
                          ("taps" . #xd3) ;; 1386
                          ("triangle" . #xdb) ;; 1394
                          ))

;; block 1424  async serial boot top/bot
(defconst async-boot-rom '(("relay" . #xa1) ;; 1388
                           ("warm" . #xa9)
                           ("cold" . #xaa)
                           ("ser-exec" . #xae)
                           ("ser-copy" . #xb3)
                           ("wait" . #xbb)
                           ("sync" . #xbe)
                           ("start" . #xc5)
                           ("delay" . #xc8)
                           ("18ibits" . #xcb) ;; 1426
                           ("byte" . #xd0) ;; 1426
                           ("4bits" . #xd2) ;; 1426
                           ("2bits" . #xd3) ;; 1426
                           ("1bit" . #xd4) ;; 1426
                           ("lsh" . #xd9) ;; 1392
                           ("rsh" . #xdb)))
;; 1392 ;???????

;; block 1428  spi boot top/bot
(defconst spi-boot-rom '(("relay" . #xa1) ;; 1388
                         ("warm" . #xa9)
                         ("8obits" . #xc2)
                         ("ibit" . #xc7)
                         ("half" . #xca)
                         ("select" . #xcc)
                         ("obit" . #xd0)
                         ("rbit" . #xd5)
                         ("18ibits" . #xd9)
                         ;;?? ibits, u2/
                         ;; block 1430
                         ("cold" . #xaa)
                         ("spi-boot" . #xb0)
                         ("spi-exec" . #xb6)
                         ("spi-copy" . #xbc)))

;; block 1436  1-wire
(defconst 1-wire-rom '(("rcv" . #x9e)
                       ("bit" . #xa1)
                       ("warm" . #xa9)
                       ("cold" . #xaa)
                       ("triangle" . #xbe) ;; 1394
                       ("*.17" . #xc3) ;; 1390
                       ("*." . #xca) ;; 1396
                       ("interp" . #xcf) ;; 1384
                       ("clc" . #xcf) ;; 1398
                       ("--u/mod" . #x2d1)  ;; 1398
                       ("-u/mod" . #x2d2) ;; 1398 ;;TODO: check
                       ))

(defconst SDRAM-addr-rom ;; node 9 block 1320
  '(("warm".  #xa9)
    ("cmd" . #xaa)))

(defconst SDRAM-control-rom ;; node 8 block 1322
  '(("warm".  #xa9)))

(defconst SDRAM-data-rom ;; node 7 block 1324
  '(("warm".  #xa9)
    ("db@" . #xaa)
    ("db!" . #xb)
    ("inpt" . #xad)))

(defconst eForth-bitsy-rom ;; node 105 block 1306
  '(("warm".  #xa9)
    ("rp--" . #xaa)
    ("bs@" . #xac)
    ("'else" . #xac)
    ("rp@" . #xb0)
    ("pshbs" . #xb1)
    ("'r@" . #xb3)
    ("@w" . #xb4)
    ("rfrom" . #xb6)
    ("popbs" . #xb9)
    ("pshr" . #xbb)
    ("rp++" . #xbf)
    ("ip++" . #xbf)
    ("tor" . #xc1)
    ("rp!" . #xc4)
    ("'con" . #xc7)
    ("'var" . #xc8)
    ("'exit" . #xc9)
    ("bitsy" . #xce)
    ("xxt" . #xd0)
    ("'ex" . #xd3)
    ("'lit" . #xd5)
    ("'if" . #xd8)))

(defconst eForth-stack-rom  ;;node 106 block 1310
  '(("warm".  #xa9)
    ("'c@" . #xaa)
    ("'@" . #xaa)
    ("x@" . #xaa)
    ("sp++" . #xac)
    ("char+" . #xac)
    ("cell+" . #xac)
    ("1+" . #xac)
    ("popt" . #xae)
    ("sp--" . #xb0)
    ("char-" . #xb0)
    ("cell-" . #xb0)
    ("1-" . #xb0)
    ("psht" . #xb2)
    ("x!" . #xb4)
    ("'c!" . #xb6)
    ("'!" . #xb6)
    ("popts" . #xb7)
    ("pops" . #xb8)
    ("pshs" . #xba)
    ("page@" . #xbc)
    ("pshw" . #xbe)
    ("page!" . #xc0)
    ("sp@" . #xc3)
    ("sp!" . #xc6)
    ("'drop" . #xc8)
    ("'over" . #xc9)
    ("'dup" . #xca)
    ("'swap" . #xcb)
    ("'2/" . #xcd)
    ("um+" . #xcf)
    ("'nc" . #xd2)
    ("'cy" . #xd3)
    ("zless" . #xd8)
    ("'or" . #xdb)
    ("'xor" . #xdc)
    ("'and" . #xdd)
    ("negate" . #xde)
    ("invert" . #xdf)
    ("zeq" . #xe0)
    ("'+" . #xe2)
    ("swap-" . #xe3)))

(defconst SDRAM-mux-rom ;; node 107 block 1328
  '(("warm".  #xa9)
    ("a2rc" . #xaa)
    ("row!" . #xaf)
    ("sd@" . #xbb)
    ("sd!" . #xc5)  ;;TODO: sd! and poll are not in dumped rom
    ("poll" . #xcf)))

(defconst SDRAM-idle-rom '(("warm".  #xa9)
                           ("noop" . #xaa)
                           ("cmd" . #xac)
                           ("idle" . #xae)
                           ("init" . #xc0)))

(define (rom-doc name block doc (code false))
  (void))

(rom-doc
 "relay" 1388
 "relay moves a port executable packet down a sequence of nodes linked by their
b registers. the packet consists of a 1-cell index, a 1-cell count less one
of body size, and the body cells.

a packet may be started from memory within  a node, or it may simply be fed
to a port.

relay assumes that b points to the next node in the chain. uses one return
stack location and four data stack locations. it must be at the same location
in every node.")

(rom-doc
 "warm" false
 "")

(rom-doc
 "*.17" 1390
 "*.17 multiplies a fraction by a fraction,giving a fraction, or an integer by a
fraction, giving an integer. note that f1 is left in s to be ignored, dropped,
or reused. note that the definition of *. contains a call to this word.

17 bit fractions --- s.i ffff ffff ffff ffff ")

(rom-doc
 "*." 1396
 "*. multiplies a fraction by a fraction, giving a fraction, or an integer by a
fraction, giving an integer. note that f1 is left in s to be ignored, dropped,
or reused.

16 bit fractions --- si. ffff ffff ffff ffff")

(rom-doc
 "taps" 1386
 ": taps yxc-y'x'
for example...

fir yx-y'x' 15 taps -53 , 0 , 2276 , 0 , 382 ,
0 , -1706 , 0 , -1158 , 0 , 2014 , 0 , 2406 ,
0 , -1977 , 0 , -4206 , 0 , 1289 , 0 , 6801 ,
0 , 678 , 0 , -11109 , 0 , -6250 , 0 , 23531 ,
0 , 54145 , 0 ,

16 taps, 16 coefficients with intermediate cr
storage interleaved.")

(rom-doc
 "interp" 1384
 "interp ims-v
to determine values for m and s ...
let l be number of meaningful input bits. let n be power of 2 where 2**n + 1
is the number of table entries.

s equals l-n-1 cr
m equals 2** l-n - 1 br

so for example if you have an 8 bit adc, l equals 8. let n equal 2 for a 5
entry table. the table is expected to be at address 0, so to represent 0 to
1800 millivolts...

0 org 0 , 450 , 900 , 1350 , 1800 ,
mv i-n 3f 5 interp ;

0 mv gives 0
128 mv gives 900
256 mv gives 1800
and intermediate values are interpolated.")

(rom-doc
 "triangle" 1394
 "triangle assuming an angle expressed as a 16 bit fraction of a
revolution, 2* 2* triangle produces a triangle wave approximation
to the cosine of that angle.")

(rom-doc
 "clc" 1398
 "clears the carry bit for addition in extended arithmetic mode")

(rom-doc
 "--u/mod" 1398
 "(TODO)
the following defines u/mod in ram ... cr
u/mod hld-rq - 1 . + --u/mod ; br

if the divisor is a constant, just negate cr
it at edit or compile time.")
(rom-doc
 "-u/mod" 1398
 "(TODO)")
(rom-doc
 "poly" 1382
 "poly xn-xy cr
evaluation of chebyshev polynomials using the horner scheme.

x is the input value. n is the length of the coefficient table minus 2.
coefficient table follows inline, and execution continues after the final
table entry. x is left on the stack under the result, y.

for example...
cos f-f' cr
hart 3300 cr
-0.0043 0.0794 -0.6459 0.5708 indent
   2* 2* . triangle dup *. 2 poly indent
   -281 , 5203 , -42329 , 37407 , indent
   push drop pop *. + ;")

(rom-doc
 "cold" 3141
 ""
 ": cold 3141 a! 0x3fffe dup ! rdlu cold ;")
(rom-doc
 "ser-exec" 1422
 "ser-exec reads and processes a boot frame.")
(rom-doc
 "ser-copy" 1422
 "ser-copy receives n words at a, nop if n zero.")
(rom-doc
 "sget" 1422
 "sget receives 18 bits, first bit on falling clock edge, second bit on rising,
 and so on. ends with clock line high. data line must be stable by the time
clock edge is seen. spins the whole time, no suspension.")

(rom-doc "spi-copy" 1430
         "spi-copy reads one word per loop and the loop counter is the actual number of
words being sent ie a count of zero means no words sent")
