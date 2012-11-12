#lang racket

(require "programs.rkt" "state.rkt" "stack.rkt")

;(define TYPE `BV4)
;(define LOG_SIZE 2)

(define SIZE 18) ; number of bits per word

(define TYPE (string->symbol (format "BV~e" SIZE)))
(define MAX (sub1 (arithmetic-shift 1 SIZE)))
(define EXP_MASK (string->symbol (format "bv~e" MAX)))

(define STACK_SIZE (* 8 SIZE))
(define STACK_TYPE (string->symbol (format "BV~e" STACK_SIZE)))
  
(define MEM_ENTRIES 3)
(define MEM_SIZE (* MEM_ENTRIES SIZE))
(define MEM_TYPE (string->symbol (format "BV~e" MEM_SIZE)))

(define COMM_ENTRIES 4)
(define COMM_SIZE (* COMM_ENTRIES SIZE))
(define COMM_TYPE (string->symbol (format "BV~e" COMM_SIZE)))
(define COMM_BIT 3)

(define TIME_SIZE 18)

(define U-ID 0)
(define D-ID 1)
(define L-ID 2)
(define R-ID 3)
(define U-SEND `sendp0)
(define D-SEND `sendp1)
(define L-SEND `sendp2)
(define R-SEND `sendp3)
(define U-RECV `recvp0)
(define D-RECV `recvp1)
(define L-RECV `recvp2)
(define R-RECV `recvp3)

;;; this is consistent with arrayForth
(define UP #x145)
(define DOWN #x115)
(define LEFT #x175)
(define RIGHT #x1d5)
(define IO #x15d)

(define CHOICES (vector-length choice-id))
(define N_OF_SLOW (vector-length memory-op))
(define HOLE_BIT (inexact->exact (ceiling (+ (/ (log CHOICES) (log 2))))))

;;; counterexample
(struct inout (input output comm))

(define inout-list '())

(define spec (make-vector 100))
(define spec-lit (make-vector 100))
(define spec-count 0) ; number of instructions in spec
(define cand (make-vector 100))
(define cand-lit (make-vector 100))
(define cand-count 0) ; number of instructions in candidate program
(define output-constraint constraint-all)

(define (to-number word)
  (cond
   [(equal? word `up) UP]
   [(equal? word `down) DOWN]
   [(equal? word `left) LEFT]
   [(equal? word `right) RIGHT]
   [else word]))

(define (to-choice name)
  (if (vector-member name choice-id)
      (cons (vector-member name choice-id) 0)
      (cons (vector-member `@p choice-id) (to-number name))))

;;; Compile code (string of instructions) into output vector
;;; and return number of instructions in the given code
(define (compile code output output-lit)
  (define count 0)
  (define in (open-input-string code))
  (define (go)
    (define (append-vector inst)
      (vector-set! output count (car inst))
      (vector-set! output-lit count (cdr inst))
      (set! count (add1 count))
      (go))
    (let ([next (read in)])
	  (if (eof-object? next)
	      (void)
	      (append-vector (to-choice next)))))
  (go)
  count)

(define var 
  (case-lambda [(base i) (string->symbol (format "~a_v~a" base i))]
               [(base i j) (string->symbol (format "~a_~a_v~a" base i j))]))

(define (var-no-v base i)
  (string->symbol (format "~a_~a" base i)))

(define (makeBV bit)
  (string->symbol (format "BV~e" bit)))

(define (makebv bit)
  (string->symbol (format "bv~e" bit)))

(define (declare-bitvector )
  (pretty-display `(define-sort BV3 () (_ BitVec 3)))                            ; for index
  (pretty-display `(define-sort ,(makeBV HOLE_BIT) () (_ BitVec ,HOLE_BIT)))     ; for hole
  (pretty-display `(define-sort ,(makeBV SIZE) () (_ BitVec ,SIZE)))             ; for normal variables
  (pretty-display `(define-sort ,(makeBV STACK_SIZE) () (_ BitVec ,STACK_SIZE))) ; for stack
  (pretty-display `(define-sort ,(makeBV MEM_SIZE) () (_ BitVec ,MEM_SIZE)))     ; for mem
  (pretty-display `(define-sort ,(makeBV COMM_SIZE) () (_ BitVec ,COMM_SIZE)))   ; for comm
  (pretty-display `(define-sort ,(makeBV COMM_BIT) () (_ BitVec ,COMM_BIT)))     ; for comm index
  )

;;;;;;;;;;;;;;;;;;;;;;;; Generate necessary Z3 functions ;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-fun id form ...)
  (define id `(define-fun id form ...)))

; for power-of-2-bit
;(define-fun bitidx ((i BV3)) (,STACK_TYPE)
;  (concat (_ bv0 ,(- (- STACK_SIZE 3) LOG_SIZE)) (concat i (_ bv0 ,LOG_SIZE))))

;;; Create index into "array" bitvector
(define (declare-bitidx func-name array-size index-size)
  (define array-type (string->symbol (format "BV~e" array-size)))
  (define index-type (string->symbol (format "BV~e" index-size)))
  (newline)
  (pretty-display
   `(define-fun ,func-name ((i ,index-type)) (,array-type)
      (bvmul (concat (_ bv0 ,(- array-size index-size)) i) (_ ,(makebv SIZE) ,array-size)))))

;;; Get an entry in "array" bitvector at the given index
(define (declare-get func-name array-size index-size index-func)
  (define array-type (string->symbol (format "BV~e" array-size)))
  (define index-type (string->symbol (format "BV~e" index-size)))
  (newline)
  (pretty-display 
   `(define-fun ,func-name ((array ,array-type) (index ,index-type)) (,TYPE)
      ((_ extract ,(sub1 SIZE) 0) (bvlshr array (,index-func index))))))

;;; Return the "array" bitvector that is replaced at the given index with the given value
(define (declare-modify func-name array-size index-size index-func)
  (define array-type (string->symbol (format "BV~e" array-size)))
  (define index-type (string->symbol (format "BV~e" index-size)))
  (newline)
  (pretty-display
   `(define-fun ,func-name ((array ,array-type) (index ,index-type) (ele ,TYPE)) (,array-type)
      (bvor
       (bvand array (bvnot (bvshl (_ ,EXP_MASK ,array-size) (,index-func index))))
       (bvshl (concat (_ bv0 ,(- array-size SIZE)) ele) (,index-func index))))))
      
(define (declare-functions)
  (declare-bitidx `bitidx-stack STACK_SIZE 3)
  (declare-bitidx `bitidx-mem   MEM_SIZE   SIZE)
  (declare-bitidx `bitidx-comm  COMM_SIZE  COMM_BIT)
  (declare-get `get-stack       STACK_SIZE 3    `bitidx-stack)
  (declare-get `get-mem         MEM_SIZE   SIZE `bitidx-mem)
  (declare-get `get-comm        COMM_SIZE  COMM_BIT `bitidx-comm)
  (declare-modify `modify-stack STACK_SIZE 3    `bitidx-stack)
  (declare-modify `modify-mem   MEM_SIZE   SIZE `bitidx-mem))

;;;;;;;;;;;;;;;;;;;;;;;; Generate Z3 variables ;;;;;;;;;;;;;;;;;;;;;;;;

(define (declare-init v n i bit)
  (for* ([step (in-range 0 n)])
    (pretty-display `(declare-const ,(var v step i) ,(makeBV bit)))))

(define (declare-init-zero v n i bit)
  (for* ([step (in-range 0 n)])
    (pretty-display `(declare-const ,(var v step i) ,(makeBV bit))))
  (pretty-display `(assert (= ,(var v 0 i) (_ ,(makebv 0) ,bit)))))

(define (declare-vector-one name i vec-size)
  (pretty-display `(declare-const ,(var name i) ,(makeBV (* vec-size SIZE)))))

(define (declare-holes n)
  (for* ([step (in-range 1 n)])
    (pretty-display `(declare-const ,(var-no-v `h step) ,(makeBV HOLE_BIT)))
    (pretty-display `(declare-const ,(var-no-v `hlit step) ,TYPE))))

(define (encode-program prog literal n name)
  (for* ([step (in-range 1 n)])
    (pretty-display `(declare-const ,(var-no-v name step) ,(makeBV HOLE_BIT)))
    (pretty-display `(assert (= ,(var-no-v name step) (_ ,(makebv (vector-ref prog (sub1 step))) ,HOLE_BIT))))
    (pretty-display `(declare-const ,(var-no-v (format "~alit" name) step) ,TYPE))
    (pretty-display `(assert (= ,(var-no-v (format "~alit" name) step) (_ ,(makebv (vector-ref literal (sub1 step))) ,SIZE))))
))

(define (declare-vars n from to)
  (for* ([i (in-range from to)])
       (declare-init `dst n i STACK_SIZE)
       (declare-init `rst n i STACK_SIZE)
       (declare-init `sp n i 3)
       (declare-init `rp n i 3)
       (declare-init `mem n i MEM_SIZE)

       (for* ([var `(t s r a b)])
	    (declare-init var n i SIZE))

       (for* ([var `(sendp0 sendp1 sendp2 sendp3 recvp0 recvp1 recvp2 recvp3)])
	    (declare-init-zero var n i COMM_BIT))

       (for* ([var `(send0 send1 send2 send3 recv0 recv1 recv2 recv3)])
	    (declare-vector-one var i COMM_ENTRIES))))

;;;;;;;;;;;;;;;;;;;;;;;; Generate Z3 formulas ;;;;;;;;;;;;;;;;;;;;;;;;

;;; syn is true for synthesizer, false for verifier
;;; when syn is true, memory out of bound check and communication check are included.
(define (generate-choice choice step n i name syn)
  (define prev (sub1 step))

  ;;; pop from data stack to s
  (define (shrink)
    (set! check_s (format "(= s_~e_v~e (get-stack dst_~e_v~e sp_~e_v~e))" step i prev i prev i))
    (set! check_sp (format "(= sp_~e_v~e (bvsub sp_~e_v~e (_ bv1 3))) " step i prev i)))

  ;;; pop from return stack to r
  (define (shrink-return)
    (set! check_r (format "(= r_~e_v~e (get-stack rst_~e_v~e rp_~e_v~e))" step i prev i prev i))
    (set! check_rp (format "(= rp_~e_v~e (bvsub rp_~e_v~e (_ bv1 3))) " step i prev i)))

  ;;; push from s to data stack
  (define (grow)
    (set! check_s (format "(= s_~e_v~e t_~a_v~e)" step i prev i))
    (set! check_sp (format "(= sp_~e_v~e (bvadd sp_~a_v~e (_ bv1 ~a))) " step i prev i 3))
    (set! check_dst (format "(= dst_~e_v~e (modify-stack dst_~a_v~e sp_~e_v~e s_~a_v~e))" step i prev i step i prev i)))

  ;;; push from r to return stack
  (define (grow-return)
    (set! check_r (format "(= r_~e_v~e t_~a_v~e)" step i prev i))
    (set! check_rp (format "(= rp_~e_v~e (bvadd rp_~a_v~e (_ bv1 ~a))) " step i prev i 3))
    (set! check_rst (format "(= rst_~e_v~e (modify-stack rst_~a_v~e rp_~e_v~e r_~a_v~e))" step i prev i step i prev i)))
  
  ;;; for pushing c to data stack
  (define (push-const c)
    (set! check_t (format "(= t_~e_v~e (_ bv~e ~e))" step i c SIZE))
    (grow))

  (define (multiply-step)
    (define ssize (sub1 SIZE))
    (define t-even (format "(concat ((_ extract ~a ~a) t_~e_v~e) ((_ extract ~a 1) t_~e_v~e))" ssize ssize prev i ssize prev i)) ; bvashr
    (define a-even (format "(concat ((_ extract 0 0) t_~e_v~e) ((_ extract ~a 1) a_~e_v~e))" prev i ssize prev i))

    (define sum (format "(bvadd (concat #b0 t_~e_v~e) (concat #b0 s_~e_v~e))" prev i prev i))
    (define sum17 (format "(concat ((_ extract ~a ~a) ~a) (_ bv0 ~a))" ssize ssize sum ssize))
    (define t-odd (format "(bvor ((_ extract ~a 1) ~a) ~a)" SIZE sum sum17))
    (define a-odd (format "(concat ((_ extract 0 0) ~a) ((_ extract ~a 1) a_~e_v~e))" sum ssize prev i))
    
    (set! check_t (format "(= t_~e_v~e (ite (= ((_ extract 0 0) a_~e_v~e) #b0) ~a ~a))" 
			  step i prev i t-even t-odd))
    (set! check_a (format "(= a_~e_v~e (ite (= ((_ extract 0 0) a_~e_v~e) #b0) ~a ~a))" 
			  step i prev i a-even a-odd)))

  (define (write-port reg port val)
    (string-append (string-append
      (format  "(ite (= ~a_~a_v~e (_ bv~e ~e))" reg prev i val SIZE)
      (if syn
	  (format  "(and (and (= (get-comm send~a_v~e sendp~a_~a_v~e) t_~a_v~e) (= sendp~a_~e_v~e (bvadd sendp~a_~a_v~e (_ bv1 ~e)))) (bvule sendp~e_~e_v~e sendp~e_~e_v~e))"
		   port i port prev i prev i     port step i port prev i COMM_BIT     port step i port (sub1 n) i)
	  (format  "(= sendp~a_~e_v~e (bvadd sendp~a_~a_v~e (_ bv1 ~e)))"
		   port step i port prev i COMM_BIT)))
      (format  "(= sendp~e_~e_v~e sendp~e_~e_v~e))" port step i port prev i)))
    
  (define (read-port reg port val)
    (string-append (string-append
      (format "(ite (= ~a_~a_v~e (_ bv~e ~e))" reg prev i val SIZE)
      (if syn
	  (format "(and (and (= (get-comm recv~a_v~e recvp~a_~a_v~e) t_~a_v~e) (= recvp~a_~e_v~e (bvadd recvp~a_~a_v~e (_ bv1 ~e)))) (bvule sendp~e_~e_v~e sendp~e_~e_v~e))"
		  port i port prev i step i     port step i port prev i COMM_BIT     port step i port (sub1 n) i)
	  (format "(= recvp~a_~e_v~e (bvadd recvp~a_~a_v~e (_ bv1 ~e)))"
		  port step i port prev i COMM_BIT)))
      (format "(= recvp~e_~e_v~e recvp~e_~e_v~e))" port step i port prev i)))
  
  ;;; check that value in register is valid for read or write from a port
  (define (mem-range reg)
    (if syn
	(string-append (string-append (string-append (string-append (string-append
          (format "(and (= ~a_~e_v~e ~a_~a_v~e) " reg step i reg prev i)
	  (format "(or (or (or (or (bvult ~a_~a_v~e (_ bv~e ~e)) " reg prev i MEM_ENTRIES SIZE))
	  (format "(= ~a_~a_v~e (_ bv~e ~e))) " reg prev i UP SIZE))
	  (format "(= ~a_~a_v~e (_ bv~e ~e))) " reg prev i DOWN SIZE))
          (format "(= ~a_~a_v~e (_ bv~e ~e))) " reg prev i LEFT SIZE))
          (format "(= ~a_~a_v~e (_ bv~e ~e))))" reg prev i RIGHT SIZE))
	(format "(= ~a_~e_v~e ~a_~a_v~e)" reg step i reg prev i)))

  ;;; default formula for an instruction: equal to the value in the previous step
  (define check_hole (format "(and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (= ~a_~e (_ bv~e ~e)) " name step (vector-member choice choice-id) HOLE_BIT))
  (define check_sp (format "(= sp_~e_v~e sp_~e_v~e)" step i prev i))
  (define check_rp (format "(= rp_~e_v~e rp_~e_v~e)" step i prev i))
  (define check_t (format "(= t_~e_v~e t_~e_v~e)" step i prev i))
  (define check_s (format "(= s_~e_v~e s_~e_v~e)" step i prev i))
  (define check_r (format "(= r_~e_v~e r_~e_v~e)" step i prev i))
  (define check_a (format "(= a_~e_v~e a_~e_v~e)" step i prev i))
  (define check_b (format "(= b_~e_v~e b_~e_v~e)" step i prev i))
  (define check_dst (format "(= dst_~e_v~e dst_~e_v~e)" step i prev i))
  (define check_rst (format "(= rst_~e_v~e rst_~e_v~e)" step i prev i))
  (define check_mem (format "(= mem_~e_v~e mem_~e_v~e)" step i prev i))
  (define check_sendp_u (format "(= sendp~e_~e_v~e sendp~e_~e_v~e)" U-ID step i U-ID prev i))
  (define check_sendp_d (format "(= sendp~e_~e_v~e sendp~e_~e_v~e)" D-ID step i D-ID prev i))
  (define check_sendp_l (format "(= sendp~e_~e_v~e sendp~e_~e_v~e)" L-ID step i L-ID prev i))
  (define check_sendp_r (format "(= sendp~e_~e_v~e sendp~e_~e_v~e)" R-ID step i R-ID prev i))
  (define check_recvp_u (format "(= recvp~e_~e_v~e recvp~e_~e_v~e)" U-ID step i U-ID prev i))
  (define check_recvp_d (format "(= recvp~e_~e_v~e recvp~e_~e_v~e)" D-ID step i D-ID prev i))
  (define check_recvp_l (format "(= recvp~e_~e_v~e recvp~e_~e_v~e)" L-ID step i L-ID prev i))
  (define check_recvp_r (format "(= recvp~e_~e_v~e recvp~e_~e_v~e)" R-ID step i R-ID prev i))

  (cond 
    ; 2*
    [(equal? choice `2*) 
               (set! check_t (format "(= t_~e_v~e (bvshl t_~e_v~e (_ bv1 ~e)))" step i prev i SIZE))]
    ; 2/
    [(equal? choice `2/)
               (set! check_t (format "(= t_~e_v~e (bvlshr t_~e_v~e (_ bv1 ~e)))" step i prev i SIZE))]
    ; -
    [(equal? choice `-)
               (set! check_t (format "(= t_~e_v~e (bvnot t_~e_v~e))" step i prev i))]
    ; +
    [(equal? choice `+)
               (set! check_t 
		     (if (= step 1)
			 (format "(= t_~e_v~e (bvadd t_~e_v~e s_~e_v~e))" step i prev i prev i)
			 (format "(and (= t_~e_v~e (bvadd t_~e_v~e s_~e_v~e)) (and (= t_~e_v~e t_~e_v~e) (= s_~e_v~e s_~e_v~e)))" 
					 step i prev i prev i     (- step 1) i (- step 2) i     (- step 1) i (- step 2) i)))
               (shrink)]
    ; and
    [(equal? choice `and)
               (set! check_t (format "(= t_~e_v~e (bvand t_~e_v~e s_~e_v~e))" step i prev i prev i))
               (shrink)]
    ; or
    [(equal? choice `or)
               (set! check_t (format "(= t_~e_v~e (bvxor t_~e_v~e s_~e_v~e))" step i prev i prev i))
               (shrink)]
    ; drop
    [(equal? choice `drop) 
               (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
               (shrink)]
    ; dup
    [(equal? choice `dup)
               (set! check_t (format "(= t_~e_v~e t_~a_v~e)" step i prev i))
	       (grow)]
    ; @+ (can't read port)
    [(equal? choice `@+)
               (set! check_a 
		     (if syn
			 (format "(and (bvult a_~a_v~e (_ bv~e ~e)) (= a_~e_v~e (bvadd a_~a_v~e (_ bv1 ~a))))" 
				 prev i MEM_ENTRIES SIZE step i prev i SIZE)
			 (format "(= a_~e_v~e (bvadd a_~a_v~e (_ bv1 ~a)))" step i prev i SIZE)))
	       (set! check_t (format "(= t_~e_v~e (get-mem mem_~e_v~e a_~e_v~e))" step i prev i prev i))
	       (grow)]
    ; @
    [(equal? choice `@)
               (set! check_a (mem-range `a))
	       (set! check_t (format "(or (bvugt a_~a_v~e (_ bv~e ~e)) (= t_~e_v~e (get-mem mem_~e_v~e a_~e_v~e)))" 
				     prev i MEM_ENTRIES SIZE     step i prev i prev i))
	       (grow)
	       (set! check_recvp_u (read-port `a U-ID UP))
	       (set! check_recvp_d (read-port `a D-ID DOWN))
	       (set! check_recvp_l (read-port `a L-ID LEFT))
	       (set! check_recvp_r (read-port `a R-ID RIGHT))]
    ; @b
    [(equal? choice `@b)
               (set! check_b (mem-range `b))
	       (set! check_t (format "(or (bvugt b_~a_v~e (_ bv~e ~e)) (= t_~e_v~e (get-mem mem_~e_v~e b_~e_v~e)))" 
				     prev i MEM_ENTRIES SIZE     step i prev i prev i))
	       (set! check_recvp_u (read-port `b U-ID UP))
	       (set! check_recvp_d (read-port `b D-ID DOWN))
	       (set! check_recvp_l (read-port `b L-ID LEFT))
	       (set! check_recvp_r (read-port `b R-ID RIGHT))
	       (grow)]
    ; @p
    [(equal? choice `@p)
               (set! check_t (format "(= t_~e_v~e ~alit_~e)" step i name step))
	       (grow)]
    ; !+ (can't store to port)
    [(equal? choice `!+)
               (set! check_a 
    		     (if syn
    			 (format "(and (bvult a_~a_v~e (_ bv~e ~e)) (= a_~e_v~e (bvadd a_~a_v~e (_ bv1 ~a))))" 
    				 prev i MEM_ENTRIES SIZE step i prev i SIZE)
    			 (format "(= a_~e_v~e (bvadd a_~a_v~e (_ bv1 ~a)))" step i prev i SIZE)))
    	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
    	       (set! check_mem (format "(= mem_~e_v~e (modify-mem mem_~a_v~e a_~a_v~e t_~a_v~e))" 
    				       step i prev i prev i prev i))
    	       (shrink)]
    ; !
    [(equal? choice `!)
               (set! check_a (mem-range `a))
    	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
    	       (set! check_mem (format "(ite (bvult a_~a_v~e (_ bv~e ~e)) (= mem_~e_v~e (modify-mem mem_~a_v~e a_~a_v~e t_~a_v~e)) (= mem_~e_v~e mem_~a_v~e))" 
    				       step i MEM_ENTRIES SIZE step i prev i prev i prev i step i prev i))
    	       (set! check_sendp_u (write-port `a U-ID UP))
    	       (set! check_sendp_d (write-port `a D-ID DOWN))
    	       (set! check_sendp_l (write-port `a L-ID LEFT))
    	       (set! check_sendp_r (write-port `a R-ID RIGHT))
    	       (shrink)]
    ; !b
    [(equal? choice `!b)
               (set! check_b (mem-range `b))
    	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
    	       (set! check_mem (format "(ite (bvult a_~a_v~e (_ bv~e ~e)) (= mem_~e_v~e (modify-mem mem_~a_v~e b_~a_v~e t_~a_v~e)) (= mem_~e_v~e mem_~a_v~e))" 
    				       step i MEM_ENTRIES SIZE step i prev i prev i prev i step i prev i))
    	       (set! check_sendp_u (write-port `b U-ID UP))
    	       (set! check_sendp_d (write-port `b D-ID DOWN))
    	       (set! check_sendp_l (write-port `b L-ID LEFT))
    	       (set! check_sendp_r (write-port `b R-ID RIGHT))
    	       (shrink)]
    ; a!
    [(equal? choice `a!)
               (set! check_a (format "(= a_~e_v~e t_~a_v~e)" step i prev i))
	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
	       (shrink)]
    ; b!
    [(equal? choice `b!)
               (set! check_b (format "(= b_~e_v~e t_~a_v~e)" step i prev i))
	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
	       (shrink)]
    ; a
    [(equal? choice `a)
               (set! check_t (format "(= t_~e_v~e a_~e_v~e)" step i prev i))
	       (grow)]
    ; +*
    [(equal? choice `+*)
               (multiply-step)]
    ; pop
    [(equal? choice `pop)
	       (set! check_t (format "(= t_~e_v~e r_~e_v~e)" step i prev i))
               (shrink-return)
	       (grow)]
    ; push
    [(equal? choice `push)
	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
               (grow-return)
	       (shrink)]
    ; over
    [(equal? choice `over)
               (set! check_t (format "(= t_~e_v~e s_~a_v~e)" step i prev i))
	       (grow)]
    ; nop
    [(equal? choice `nop) (void)]
        )
  (set! check_sp (string-append check_sp ") "))
  (set! check_rp (string-append check_rp ") "))
  (set! check_t (string-append check_t ") "))
  (set! check_s (string-append check_s ") "))
  (set! check_r (string-append check_r ") "))
  (set! check_a (string-append check_a ") "))
  (set! check_b (string-append check_b ") "))
  (set! check_dst (string-append check_dst ") "))
  (set! check_rst (string-append check_rst ") "))
  (set! check_mem (string-append check_mem ") "))
  (set! check_sendp_u (string-append check_sendp_u ") "))
  (set! check_sendp_d (string-append check_sendp_d ") "))
  (set! check_sendp_l (string-append check_sendp_l ") "))
  (set! check_sendp_r (string-append check_sendp_r ") "))
  (set! check_recvp_u (string-append check_recvp_u ") "))
  (set! check_recvp_d (string-append check_recvp_d ") "))
  (set! check_recvp_l (string-append check_recvp_l ") "))
  (set! check_recvp_r (string-append check_recvp_r ")"))
  (string-append (string-append (string-append (string-append (string-append (string-append (string-append (string-append 
    (string-append (string-append (string-append (string-append (string-append (string-append (string-append (string-append 
      (string-append (string-append
    check_hole 
    check_sp)
    check_rp)
    check_t)
    check_s) 
    check_r) 
    check_a)
    check_b)
    check_dst)
    check_rst)
    check_mem)
    check_sendp_u)
    check_sendp_d)
    check_sendp_l)
    check_sendp_r)
    check_recvp_u)
    check_recvp_d)
    check_recvp_l)
    check_recvp_r))

;;; Handle assumption differently from assertion on input-outpout pair.
;;; Memory out of bound check and communication check happen here.
;;; This function is only called by verifier.
(define (generate-choice-assumption choice step i name)
  (define prev (sub1 step))
    
  (define (write-port reg port val)
    (string-append
      (format "(and (= ~a_~a_v~e (_ bv~e ~e)) " reg prev i val SIZE)
      (format "(not (= (get-comm send~a_v~e sendp~a_~a_v~e) t_~a_v~e)))" port i port prev i step i)))
    
  (define (read-port reg port val)
    (string-append
      (format "(and (= ~a_~a_v~e (_ bv~e ~e)) " reg prev i val SIZE)
      (format "(not (= (get-comm recv~a_v~e recvp~a_~a_v~e) t_~a_v~e)))" port i port prev i step i)))
  
  (define (mem-range reg)
    (string-append (string-append (string-append (string-append (string-append
      (format "(and (and (and (and (bvuge ~a_~a_v~e (_ bv~e ~e)) " reg prev i MEM_ENTRIES SIZE))
      (format "(not (= ~a_~a_v~e (_ bv~e ~e)))) " reg prev i UP SIZE))
      (format "(not (= ~a_~a_v~e (_ bv~e ~e)))) " reg prev i DOWN SIZE))
      (format "(not (= ~a_~a_v~e (_ bv~e ~e)))) " reg prev i LEFT SIZE))
      (format "(not (= ~a_~a_v~e (_ bv~e ~e))))" reg prev i RIGHT SIZE)))

  (define check_assump 
  (cond 
    ; @+ (can't read port)
    [(equal? choice `@+)
	 (format "(bvuge a_~a_v~e (_ bv~e ~e))" prev i MEM_ENTRIES SIZE)]
    ; @
    [(equal? choice `@) 
	 (format "(or (or (or (or ~a ~a) ~a) ~a) ~a)" 
		 (mem-range `a) 
		 (read-port `a U-ID UP) 
		 (read-port `a D-ID DOWN) 
		 (read-port `a L-ID LEFT)
		 (read-port `a R-ID RIGHT))]
    ; @b
    [(equal? choice `@b)
	 (format "(or (or (or (or ~a ~a) ~a) ~a) ~a)" 
		 (mem-range `b) 
		 (read-port `b U-ID UP) 
		 (read-port `b D-ID DOWN) 
		 (read-port `b L-ID LEFT)
		 (read-port `b R-ID RIGHT))]
    ; !+ (can't store to port)
    [(equal? choice `!+)
	 (format "(bvuge a_~a_v~e (_ bv~e ~e))" prev i MEM_ENTRIES SIZE)]
    ; !
    [(equal? choice `!)
	 (format "(or (or (or (or ~a ~a) ~a) ~a) ~a)" 
		 (mem-range `a) 
		 (write-port `a U-ID UP) 
		 (write-port `a D-ID DOWN) 
		 (write-port `a L-ID LEFT)
		 (write-port `a R-ID RIGHT))]
    ; !b
    [(equal? choice `!b)
	 (format "(or (or (or (or ~a ~a) ~a) ~a) ~a)" 
		 (mem-range `b) 
		 (write-port `b U-ID UP) 
		 (write-port `b D-ID DOWN) 
		 (write-port `b L-ID LEFT)
		 (write-port `b R-ID RIGHT))]))
    (format "(and (= ~a_~e (_ bv~e ~e)) ~a)" name step (vector-member choice choice-id) HOLE_BIT check_assump))

;;; Returns the bv constant representing the given time (n).
(define (bv-time n)
  (format "(_ bv~a ~a)" n TIME_SIZE))

(define (generate-time-constraint step)
  (pretty-display (format "(assert (= time_~a (bvadd time_~a (ite (bvult h_~a (_ bv~a ~a)) ~a ~a))))"
                          step (sub1 step) step N_OF_SLOW HOLE_BIT (bv-time 10) (bv-time 3))))

;;; Returns an expression counting the number of trailing nops given
;;; the total number of slots. This can then be subtracted from the
;;; total time.
(define (nop-offset slots)
  (define bv-nop (format "(_ bv~a ~a)" (vector-member 'nop choice-id) HOLE_BIT))
  (define (go hole-number expr)
    (format "(ite (= h_~a ~a) (bvadd ~a ~a) ~a)"
            hole-number bv-nop (bv-time 3) expr (bv-time 0)))
  (define start (format "(ite (= h_1 ~a) ~a ~a)" bv-nop (bv-time 3) (bv-time 0)))
  (foldr go start (reverse (stream->list (in-range 2 (add1 slots))))))

(define (generate-time-constraints n time-limit)
  (for* ([step (in-range 0 (add1 n))])
        (pretty-display `(declare-const ,(var-no-v `time step) ,(makeBV TIME_SIZE))))
  (pretty-display `(assert (= ,(var-no-v `time 0) (_ bv0 ,TIME_SIZE))))
  (for* ([step (in-range 1 (add1 n))])
	(generate-time-constraint step))
  (pretty-display (format "(assert (bvult (bvsub time_~a ~a) (_ bv~a ~a)))"
                          n (nop-offset n) time-limit TIME_SIZE)))

(define support '#(@p @+ @b @ !+ !b ! +* 2* 2/ - + and or drop dup pop over a nop push b! a!))
(define support-len (vector-length support))
 
(define (generate-formula step n version name syn)
  (define clauses (make-vector support-len))
  (when (= (modulo step 4) 0)
	(pretty-display (format "(assert (= (_ bv0 2) ((_ extract 1 0) ~a_~a)))" name step)))
  (for* ([i (in-range 0 (vector-length support))])
	(vector-set! clauses i (generate-choice (vector-ref support i) step n version name syn)))
  (pretty-display `(assert ,(conjunct clauses support-len `or))))

(define (generate-formulas n from to name syn)
  (for* ([version (in-range from to)]
	 [step (in-range 1 n)])
    (generate-formula step n version name syn)))

;;;;;;;;;;;;;;;;;;;;;;;; Synthesizer helper functions ;;;;;;;;;;;;;;;;;;;;;;;;

(define (assert-state-input state n i)
  (pretty-display (format "(assert (= dst_~e_v~e (_ bv~e ~e)))" n i (stack-body (progstate-data state)) STACK_SIZE))
  (pretty-display (format "(assert (= sp_~e_v~e (_ bv~e ~e)))" n i (stack-sp (progstate-data state)) 3))
  (pretty-display (format "(assert (= rst_~e_v~e (_ bv~e ~e)))" n i (stack-body (progstate-return state)) STACK_SIZE))
  (pretty-display (format "(assert (= rp_~e_v~e (_ bv~e ~e)))" n i (stack-sp (progstate-return state)) 3))
  (pretty-display (format "(assert (= mem_~e_v~e (_ bv~e ~e)))" n i (progstate-memory state) MEM_SIZE))
  (pretty-display (format "(assert (= t_~e_v~e (_ bv~e ~e)))" n i (progstate-t state) SIZE))
  (pretty-display (format "(assert (= s_~e_v~e (_ bv~e ~e)))" n i (progstate-s state) SIZE))
  (pretty-display (format "(assert (= r_~e_v~e (_ bv~e ~e)))" n i (progstate-r state) SIZE))
  (pretty-display (format "(assert (= a_~e_v~e (_ bv~e ~e)))" n i (progstate-a state) SIZE))
  (pretty-display (format "(assert (= b_~e_v~e (_ bv~e ~e)))" n i (progstate-b state) SIZE)))

(define (assert-state-output state n i)
  (when (progstate-data output-constraint)
	(pretty-display (format "(assert (= dst_~e_v~e (_ bv~e ~e)))" n i (stack-body (progstate-data state)) STACK_SIZE))
	(pretty-display (format "(assert (= sp_~e_v~e (_ bv~e ~e)))" n i (stack-sp (progstate-data state)) 3)))
  (when (progstate-return output-constraint)
	(pretty-display (format "(assert (= rst_~e_v~e (_ bv~e ~e)))" n i (stack-body (progstate-return state)) STACK_SIZE))
	(pretty-display (format "(assert (= rp_~e_v~e (_ bv~e ~e)))" n i (stack-sp (progstate-return state)) 3)))
  (when (progstate-memory output-constraint)
	(pretty-display (format "(assert (= mem_~e_v~e (_ bv~e ~e)))" n i (progstate-memory state) MEM_SIZE)))
  (when (progstate-t state)
	(pretty-display (format "(assert (= t_~e_v~e (_ bv~e ~e)))" n i (progstate-t state) SIZE)))
  (when (progstate-s output-constraint)
	(pretty-display (format "(assert (= s_~e_v~e (_ bv~e ~e)))" n i (progstate-s state) SIZE)))
  (when (progstate-r output-constraint)
	(pretty-display (format "(assert (= r_~e_v~e (_ bv~e ~e)))" n i (progstate-r state) SIZE)))
  (when (progstate-a output-constraint)
	(pretty-display (format "(assert (= a_~e_v~e (_ bv~e ~e)))" n i (progstate-a state) SIZE)))
  (when (progstate-b output-constraint)
	(pretty-display (format "(assert (= b_~e_v~e (_ bv~e ~e)))" n i (progstate-b state) SIZE))))

(define (assert-comm comm n i)
  (pretty-display (format "(assert (= send~e_v~e (_ bv~e ~e)))" U-ID i (commstate-send-u comm) COMM_SIZE))
  (pretty-display (format "(assert (= send~e_v~e (_ bv~e ~e)))" D-ID i (commstate-send-d comm) COMM_SIZE))
  (pretty-display (format "(assert (= send~e_v~e (_ bv~e ~e)))" L-ID i (commstate-send-l comm) COMM_SIZE))
  (pretty-display (format "(assert (= send~e_v~e (_ bv~e ~e)))" R-ID i (commstate-send-r comm) COMM_SIZE))
  (pretty-display (format "(assert (= recv~e_v~e (_ bv~e ~e)))" U-ID i (commstate-recv-u comm) COMM_SIZE))
  (pretty-display (format "(assert (= recv~e_v~e (_ bv~e ~e)))" D-ID i (commstate-recv-d comm) COMM_SIZE))
  (pretty-display (format "(assert (= recv~e_v~e (_ bv~e ~e)))" L-ID i (commstate-recv-l comm) COMM_SIZE))
  (pretty-display (format "(assert (= recv~e_v~e (_ bv~e ~e)))" R-ID i (commstate-recv-r comm) COMM_SIZE))
  (pretty-display (format "(assert (= sendp~e_~e_v~e (_ bv~e ~e)))" U-ID n i (commstate-sendp-u comm) COMM_BIT))
  (pretty-display (format "(assert (= sendp~e_~e_v~e (_ bv~e ~e)))" D-ID n i (commstate-sendp-d comm) COMM_BIT))
  (pretty-display (format "(assert (= sendp~e_~e_v~e (_ bv~e ~e)))" L-ID n i (commstate-sendp-l comm) COMM_BIT))
  (pretty-display (format "(assert (= sendp~e_~e_v~e (_ bv~e ~e)))" R-ID n i (commstate-sendp-r comm) COMM_BIT))
  (pretty-display (format "(assert (= recvp~e_~e_v~e (_ bv~e ~e)))" U-ID n i (commstate-recvp-u comm) COMM_BIT))
  (pretty-display (format "(assert (= recvp~e_~e_v~e (_ bv~e ~e)))" D-ID n i (commstate-recvp-d comm) COMM_BIT))
  (pretty-display (format "(assert (= recvp~e_~e_v~e (_ bv~e ~e)))" L-ID n i (commstate-recvp-l comm) COMM_BIT))
  (pretty-display (format "(assert (= recvp~e_~e_v~e (_ bv~e ~e)))" R-ID n i (commstate-recvp-r comm) COMM_BIT)))

(define (assert-pair input output comm n i has-out)
  (assert-state-input input 0 i)
  (when has-out
      (assert-state-output output n i)
      (assert-comm comm n i)))

(define (assert-input-output n has-out)
  (foldl (lambda (p i)
           (assert-pair (inout-input p) (inout-output p) (inout-comm p) n i has-out)
           (add1 i))
         0
         inout-list))

;;;;;;;;;;;;;;;;;;;;;;;; Verifier helper functions ;;;;;;;;;;;;;;;;;;;;;;;;

;;; Turn vector of cluases into a string of conjunction or disjunction of the cluases 
(define (conjunct clauses size conj)
  (define s "")
  (for ([i (in-range 1 size)])
       (set! s (string-append s (format "(~a " conj))))
  (set! s (string-append s (vector-ref clauses 0)))
  (for ([i (in-range 1 size)])
       (set! s (string-append s (format " ~a)" (vector-ref clauses i)))))
  s)

(define (generate-assumption step version name)
  (define clauses (make-vector 6))
  (define index 0)
  (for* ([choice `(@+ @ @b !+ ! !b)])
	(vector-set! clauses index 
		     (generate-choice-assumption choice step version name))
	(set! index (add1 index)))
  (conjunct clauses 6 `or))

(define (generate-assumptions n from to name)
  (define clauses (make-vector (* (- to from) (add1 n))))
  (define index 0)
  (for* ([version (in-range from to)]
	 [step (in-range 1 n)])
	(vector-set! clauses index (generate-assumption step version name))
	(set! index (add1 index)))
  (conjunct clauses  index `or))

(define (assert-input-eq)
  (for ([var `(dst rst mem t s r a b sp rp)])
       (pretty-display (format "(assert (= ~a_0_v0 ~a_0_v1))" var var))))

(define (assert-output-neq)
  ;;; Add this assertion to set the irrelevant communcation entries to 0
  ;;; so that it's easy for comparision
  (define (assert-ir-comm v n)
    (for* ([port `(send recv)]
	   [p (in-range 0 4)]
	   [i (in-range 0 COMM_ENTRIES)])
	  (define index (format "(_ bv~a ~a)" i COMM_BIT))
	  (pretty-display (format "(assert (or (bvult ~a ~ap~a_~a_v~a) (= (get-comm ~a~a_v~a ~a) (_ bv0 ~a))))" 
				  index port p n v   port p v index SIZE))))
  (assert-ir-comm 0 spec-count)
  (assert-ir-comm 1 cand-count)

  ;;; Assert outputs are not equal
  (define clauses (make-vector 26))
  (define index 0)

  (define (vector-add var)
    (vector-set! clauses index (format "(not (= ~a_~e_v0 ~a_~e_v1))" var spec-count var cand-count))
    (set! index (add1 index)))

  ;;; TODO: relax constraint here too
  (for ([var `(sendp0 sendp1 sendp2 sendp3 recvp0 recvp1 recvp2 recvp3)])
       (vector-add var))
  (for* ([var `(send recv)]
	 [channel (in-range 0 4)])
       (vector-set! clauses index (format "(not (= ~a~a_v0 ~a~a_v1))" var channel var channel))
       (set! index (add1 index)))
  (when (progstate-data output-constraint)
	(vector-add `dst)
	(vector-add `sp))
  (when (progstate-return output-constraint)
	(vector-add `rst)
	(vector-add `rp))
  (when (progstate-memory output-constraint)
	(vector-add `mem))
  (when (progstate-t output-constraint)
	(vector-add `t))
  (when (progstate-s output-constraint)
	(vector-add `s))
  (when (progstate-r output-constraint)
	(vector-add `r))
  (when (progstate-a output-constraint)
	(vector-add `a))
  (when (progstate-b output-constraint)
	(vector-add `b))

  ;;; Include assumption throughtout the program
  (pretty-display (format "(assert (or ~a ~a))" (conjunct clauses index `or) (generate-assumptions (add1 cand-count) 1 2 `cand))))

;;;;;;;;;;;;;;;;;;;;;;;; Synthesizer (and general formula generator) ;;;;;;;;;;;;;;;;;;;;;;;;

(define (synthesize-prog n has-prog has-out [time-limit 0])
  (declare-bitvector)
  (declare-functions)
  (newline)
  (if has-prog
      (encode-program spec spec-lit (add1 n) `h)
      (declare-holes (add1 n)))

  (declare-vars (add1 n) 0 (length inout-list))
  (newline)
  (generate-formulas (add1 n) 0 (length inout-list) `h #t)
  (newline)
  (assert-input-output n has-out)
  (newline)
  (if (> time-limit 0)
      (generate-time-constraints n time-limit)
      (void))
  (newline)
  (pretty-display `(check-sat))
  (pretty-display `(get-model))
)

;;;;;;;;;;;;;;;;;;;;;;;; Verifier  ;;;;;;;;;;;;;;;;;;;;;;;;

(define (verify-prog)
  (declare-bitvector)
  (declare-functions)
  (newline)

  ; formular for spec
  (encode-program spec spec-lit (add1 spec-count) `spec)
  (newline)
  (declare-vars (add1 spec-count) 0 1)
  (newline)
  (generate-formulas (add1 spec-count) 0 1 `spec #t)
  (newline)

  ; formular for candidate
  (encode-program cand cand-lit (add1 cand-count) `cand)
  (newline)
  (declare-vars (add1 cand-count) 1 2)
  (newline)
  (generate-formulas (add1 cand-count) 1 2 `cand #f)
  (newline)
  
  (assert-input-eq)
  (newline)
  (assert-output-neq)
  (newline)
  (pretty-display `(check-sat))
  (pretty-display `(get-model))
)

;;;;;;;;;;;;;;;;;;;;;;;; Input-Output-Rend-Recv storage, converter, and generator ;;;;;;;;;;;;;;;;;;;;;;;;

(define (vec-to-bits-inner vec vec-size)
  (define bits 0)
  (for* ([i (in-range 0 vec-size)])
    (set! bits (+ (arithmetic-shift bits SIZE) (vector-ref vec (- (- vec-size i) 1)))))
  bits)
(define (vec-to-bits vec vec-size)
  (if (vector? vec)
      (vec-to-bits-inner vec vec-size)
      vec))

(define (convert-progstate state)
  (define data (if (progstate-data state)
		   (stack (stack-sp (progstate-data state)) (vec-to-bits (stack-body (progstate-data state)) 8))
		   #f))
  (define return (if (progstate-return state)
		     (stack (stack-sp (progstate-return state)) (vec-to-bits (stack-body (progstate-return state)) 8))
		     #f))
  (define mem (if (progstate-memory state)
		  (vec-to-bits (progstate-memory state) MEM_ENTRIES)
		  #f))
  (progstate (progstate-a state) (progstate-b state) 
	     (progstate-p state) (progstate-i state) 
	     (progstate-r state) (progstate-s state) (progstate-t state)
	     data return mem))
             
(define (convert-commstate state)
  (commstate (vec-to-bits (commstate-send-u state) COMM_ENTRIES)
             (vec-to-bits (commstate-send-d state) COMM_ENTRIES)
             (vec-to-bits (commstate-send-l state) COMM_ENTRIES)
             (vec-to-bits (commstate-send-r state) COMM_ENTRIES)
             (vec-to-bits (commstate-recv-u state) COMM_ENTRIES)
             (vec-to-bits (commstate-recv-d state) COMM_ENTRIES)
             (vec-to-bits (commstate-recv-l state) COMM_ENTRIES)
             (vec-to-bits (commstate-recv-r state) COMM_ENTRIES)
             (commstate-sendp-u state) (commstate-sendp-d state) (commstate-sendp-l state) (commstate-sendp-r state)
             (commstate-recvp-u state) (commstate-recvp-d state) (commstate-recvp-l state) (commstate-recvp-r state)))


;;;;;;;;;;;;;;;;;;;;;;;; GreenSyn API ;;;;;;;;;;;;;;;;;;;;;;;;

(define current-input 0)
(define current-output 0)
(define current-comm 0)

(provide (all-defined-out))

;;; Set
;;; 1) number of entries of memory
;;; 2) number of entries of send/recv storage of each 4 neighbors
(define (greensyn-reset mem-entries comm-entries [constraint constraint-all])
  (set! output-constraint constraint)

  (set! MEM_ENTRIES mem-entries)
  (set! MEM_SIZE (* MEM_ENTRIES SIZE))
  (set! MEM_TYPE (string->symbol (format "BV~e" MEM_SIZE)))

  (set! COMM_ENTRIES comm-entries)
  (set! COMM_SIZE (* COMM_ENTRIES SIZE))
  (set! COMM_TYPE (string->symbol (format "BV~e" COMM_SIZE)))
  (set! COMM_BIT (inexact->exact (floor (+ (/ (log COMM_ENTRIES) (log 2)) 1))))

  (set! inout-list '()))

;;; Set input for counterexmaple.
(define (greensyn-input state)
  (set! current-input (convert-progstate state)))

;;; Set output for counterexmaple.
(define (greensyn-output state)
  (set! current-output (convert-progstate state)))

;;; Set send/recv storage for counterexmaple.
(define (greensyn-send-recv state)
  (set! current-comm (convert-commstate state)))

;;; Add previously set counterexample (input, output, and send/recv)
;;; to the list that will be generated as formula.
;;; If input, output, send/recv are set but not commit, 
;;; the counterexample won't be included in the generated formula
(define (greensyn-commit)
  (set! inout-list (cons (inout (convert-progstate current-input)
                                (convert-progstate current-output)
                                (convert-commstate current-comm))
                         inout-list)))

;;; Set spec for verification
(define (greensyn-spec string)
  (set! spec-count (compile string spec spec-lit)))

;;; Generate Z3 file for synthesis from the list of counterexamples
;;; Usage:
;;; (greensyn-reset)
;;; { (greensyn-input input) (greensyn-output output) (greensyn-send-recv send-recv) (greensyn-commit) }+
;;; (greensyn-check-sat file number_of_slots)
(define (greensyn-check-sat #:file [file "prog.smt2"] prog-size #:time-limit [time-limit 0])
  (define out (open-output-file file #:exists 'replace))
  (parameterize ([current-output-port out])
    (synthesize-prog prog-size #f #t time-limit))
  (close-output-port out))

;;; Generate Z3 file for verification from spec and a given candidate
;;; Usage:
;;; (greensyn-reset)
;;; (greensyn-spec string_of_spec)
;;; (greensyn-verify file string_of_candidate)
;;; CAUTION: note that if intruction in the last slot is invalid
;;; it will return unsat!!!!!
(define (greensyn-verify file string)
  (set! cand-count (compile string cand cand-lit))
  (define out (open-output-file file #:exists 'replace))
  (parameterize ([current-output-port out])
    (verify-prog))
  (close-output-port out))

;;; Generate Z3 file according to the earlier set spec (greensyn-spec)
;;; if has-out is true, then the formula will assert on earlier set output (greensyn-output)
;;; otherwise, the formula won't include output
;;; Usage:
;;; (greensyn-reset)
;;; { (greensyn-input input) (greensyn-output output)? (greensyn-send-recv send-recv)? (greensyn-commit) }+
;;; (greensyn-gen-formula string_of_candidate assert_output)
(define (greensyn-gen-formula file has-out)
  (define out (open-output-file file #:exists 'replace))
  (parameterize ([current-output-port out])
    (synthesize-prog spec-count #t has-out))
  (close-output-port out))
  
  
