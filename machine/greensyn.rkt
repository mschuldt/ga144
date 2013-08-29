 #lang racket

(require "programs.rkt" "state.rkt" "stack.rkt")

;(define TYPE `BV4)
;(define LOG_SIZE 2)

(define SIZE 4) ; number of bits per word

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
(define COMMTYPE_UNIT 4)
(define COMMTYPE_SIZE (* COMM_ENTRIES 4))
(define COMM_TYPE (string->symbol (format "BV~e" COMM_SIZE)))
(define COMM_BIT 4)

(define TIME_SIZE 16)
(define LENGTH_SIZE 16)

(define U-ID 0)
(define D-ID 1)
(define L-ID 2)
(define R-ID 3)
(define IO-ID 4)

(define CHOICES (vector-length choice-id))
(define N_OF_SLOW (vector-length memory-op))
(define HOLE_BIT (inexact->exact (ceiling (+ (/ (log CHOICES) (log 2))))))

;;; counterexample
(struct inout (input output comm))

(define inout-list '())

;; TODO: Use lists instead of vectors, or something.
(define spec (make-vector MEM-SIZE))
(define spec-lit (make-vector MEM-SIZE))
(define spec-count 0) ; number of instructions in spec
(define cand (make-vector MEM-SIZE))
(define cand-lit (make-vector MEM-SIZE))
(define cand-count 0) ; number of instructions in candidate program
(define output-constraint constraint-all)
(define SUPPORT `all)

(define support-all '#(@p @+ @b @ !+ !b ! +* 2* 2/ - + and or drop dup pop over a nop push b! a! lshift rshift /))
(define support-no-fake '#(@p @+ @b @ !+ !b ! +* 2* 2/ - + and or drop dup pop over a nop push b! a!))
(define support-no-fake-no-p '#(@+ @b @ !+ !b ! +* 2* 2/ - + and or drop dup pop over a nop push b! a!))
(define support-no-mem '#(@p +* 2* 2/ - + and or drop dup pop over a nop push a!))
(define support-no-mem-no-p '#(+* 2* 2/ - + and or drop dup pop over a nop push a!))
(define (support)
  (cond
   [(equal? SUPPORT `all) support-all]
   [(equal? SUPPORT `no-fake) support-no-fake]
   [(equal? SUPPORT `no-fake-no-p) support-no-fake-no-p]
   [(equal? SUPPORT `no-mem) support-no-mem]
   [(equal? SUPPORT `no-mem-no-p) support-no-mem-no-p]))
(define (support-len) (vector-length (support)))

(define (to-number word)
  (cond
   [(equal? word `up) UP]
   [(equal? word `down) DOWN]
   [(equal? word `left) LEFT]
   [(equal? word `right) RIGHT]
   [(equal? word `io) RIGHT]
   [else word]))

;;; Converts a name of an instruction (as a symbol) to its numeric
;;; representation. Turns numbers into the code for @p.
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
  (pretty-display `(define-sort BV1 () (_ BitVec 1)))  
  (pretty-display `(define-sort BV3 () (_ BitVec 3)))   ; for index

  (pretty-display `(define-sort ,(makeBV HOLE_BIT) () (_ BitVec ,HOLE_BIT)))     ; for hole
  (pretty-display `(define-sort ,(makeBV SIZE) () (_ BitVec ,SIZE)))             ; for normal variables
  (pretty-display `(define-sort ,(makeBV STACK_SIZE) () (_ BitVec ,STACK_SIZE))) ; for stack

  (pretty-display `(define-sort ,(makeBV MEM_SIZE) () (_ BitVec ,MEM_SIZE)))     ; for mem
  (pretty-display `(define-sort ,(makeBV COMM_SIZE) () (_ BitVec ,COMM_SIZE)))   ; for comm array
  (pretty-display `(define-sort ,(makeBV COMMTYPE_SIZE) () (_ BitVec ,COMMTYPE_SIZE)))   ; for commtype array
  (pretty-display `(define-sort ,(makeBV COMM_BIT) () (_ BitVec ,COMM_BIT)))     ; for comm index
  (pretty-display `(define-sort BV4 () (_ BitVec ,COMMTYPE_UNIT)))   ; for commtype unit

  (pretty-display `(define-sort ,(makeBV TIME_SIZE) () (_ BitVec ,TIME_SIZE)))     ; for time
  )

;;;;;;;;;;;;;;;;;;;;;;;; Generate necessary Z3 functions ;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-fun id form ...)
  (define id `(define-fun id form ...)))

;;; Create index into "array" bitvector
(define (declare-bitidx func-name array-size index-size [val-size SIZE])
  (define array-type (string->symbol (format "BV~e" array-size)))
  (define index-type (string->symbol (format "BV~e" index-size)))
  (define i
    (if (> array-size index-size)
	(format "(concat (_ bv0 ~a) i)" (- array-size index-size))
	"i"))
  (newline)
  (pretty-display `(define-fun ,func-name ((i ,index-type)) (,array-type)
		     (bvmul ,i (_ ,(makebv val-size) ,array-size)))))

;;; Get an entry in "array" bitvector at the given index
(define (declare-get func-name array-size index-size index-func [val-size SIZE])
  (define array-type (string->symbol (format "BV~e" array-size)))
  (define index-type (string->symbol (format "BV~e" index-size)))
  (newline)
  (pretty-display 
   `(define-fun ,func-name ((array ,array-type) (index ,index-type)) (,(format "BV~a" val-size))
      ((_ extract ,(sub1 val-size) 0) (bvlshr array (,index-func index))))))

;;; Return the "array" bitvector that is replaced at the given index with the given value
(define (declare-modify func-name array-size index-size index-func)
  (define array-type (string->symbol (format "BV~e" array-size)))
  (define index-type (string->symbol (format "BV~e" index-size)))
  (define ele
    (if (> array-size SIZE)
	(format "(concat (_ bv0 ~a) ele)" (- array-size SIZE))
	"ele"))
  (newline)
  (pretty-display
   `(define-fun ,func-name ((array ,array-type) (index ,index-type) (ele ,TYPE)) (,array-type)
      (bvor
       (bvand array (bvnot (bvshl (_ ,EXP_MASK ,array-size) (,index-func index))))
       (bvshl ,ele (,index-func index))))))
      
(define (declare-functions)
  (declare-bitidx `bitidx-stack STACK_SIZE   3)
  (declare-bitidx `bitidx-mem   MEM_SIZE     SIZE)
  (declare-bitidx `bitidx-comm  COMM_SIZE    COMM_BIT)
  (declare-bitidx `bitidx-commtype COMMTYPE_SIZE   COMM_BIT COMMTYPE_UNIT)

  (declare-get `get-stack       STACK_SIZE   3        `bitidx-stack)
  (declare-get `get-mem         MEM_SIZE     SIZE     `bitidx-mem)
  (declare-get `get-comm        COMM_SIZE    COMM_BIT `bitidx-comm)
  (declare-get `get-commtype    COMMTYPE_SIZE   COMM_BIT `bitidx-commtype COMMTYPE_UNIT)

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

(define (declare-vector-one name i bit)
  (pretty-display `(declare-const ,(var name i) ,(makeBV bit))))

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
       (declare-init-zero `commp n i COMM_BIT)
       (declare-init-zero `recvp n i COMM_BIT)

       (for* ([var `(t s r a b)])
	     (declare-init var n i SIZE))

       (declare-vector-one `commdata i COMM_SIZE)
       (declare-vector-one `recvdata i COMM_SIZE)
       (declare-vector-one `commtype i COMMTYPE_SIZE)))

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
    
  (define (write-port reg)
    (define (test val)
      (format "(= ~a_~a_v~e (_ bv~e ~e))" reg prev i val SIZE))

    (define (typecheck port val)
      (format "(and ~a (= (get-commtype commtype_v~e commp_~a_v~e) (_ bv~a ~a)))"
              (test val)
              i prev i (+ 5 port) COMMTYPE_UNIT))

    (define main (format "(and (= (get-comm commdata_v~e commp_~a_v~e) t_~a_v~e) (= commp_~e_v~e (bvadd commp_~a_v~e (_ bv1 ~e))) (or ~a ~a ~a ~a ~a))"
                                        i prev i prev i   
                                        step i prev i COMM_BIT
                                        (typecheck U-ID UP) (typecheck D-ID DOWN) 
                                        (typecheck L-ID LEFT) (typecheck R-ID RIGHT)
                                        (typecheck IO-ID IO)))

    (when syn
          (set! main (format "(and ~a (bvule commp_~e_v~e commp_~e_v~e))"
                             main step i (sub1 n) i)))
                                        
    (format "(ite (or ~a ~a ~a ~a ~a) ~a (= commp_~e_v~e commp_~e_v~e))"
            (test UP) (test DOWN) (test LEFT) (test RIGHT) (test IO)
            main
            step i prev i))
    
  (define (read-port reg)
    (define (test val)
      (format "(= ~a_~a_v~e (_ bv~e ~e))" reg prev i val SIZE))

    (define (typecheck port val)
      (format "(and ~a (= (get-commtype commtype_v~e commp_~a_v~e) (_ bv~a ~a)))"
              (test val)
              i prev i port COMMTYPE_UNIT))

    (define main (format "(and (= (get-comm commdata_v~e commp_~a_v~e) t_~a_v~e) (= commp_~e_v~e (bvadd commp_~a_v~e (_ bv1 ~e))) (or ~a ~a ~a ~a ~a))"
                                        i prev i step i   
                                        step i prev i COMM_BIT
                                        (typecheck U-ID UP) (typecheck D-ID DOWN) 
                                        (typecheck L-ID LEFT) (typecheck R-ID RIGHT)
                                        (typecheck IO-ID IO)))

    (when syn
          (set! main (format "(and ~a (bvule commp_~e_v~e commp_~e_v~e))"
                             main step i (sub1 n) i)))
                                        
    (format "(ite (or ~a ~a ~a ~a ~a) ~a (= commp_~e_v~e commp_~e_v~e))"
            (test UP) (test DOWN) (test LEFT) (test RIGHT) (test IO)
            main
            step i prev i))
    
  (define (recv-port reg)
    (define (test val)
      (format "(= ~a_~a_v~e (_ bv~e ~e))" reg prev i val SIZE))

    (define main (format "(and (= (get-comm recvdata_v~e recvp_~a_v~e) t_~a_v~e) (= recvp_~e_v~e (bvadd recvp_~a_v~e (_ bv1 ~e))))"
                         i prev i step i
                         step i prev i COMM_BIT
                         ))

    ;; (when syn
    ;;       (set! main (format "(and ~a (and ~a (bvule recvp_~e_v~e recvp_~e_v~e)))"
    ;;                          main step i (sub1 n) i)))
                                        
    (format "(ite (or ~a ~a ~a ~a ~a) ~a (= recvp_~e_v~e recvp_~e_v~e))"
            (test UP) (test DOWN) (test LEFT) (test RIGHT) (test IO)
            main
            step i prev i))
  
  ;;; check that value in register is valid for read or write from a port
  (define (mem-range reg)
    (if syn
	(string-append 
          (format "(and (= ~a_~e_v~e ~a_~a_v~e) " reg step i reg prev i)
	  (format "(or (bvult ~a_~a_v~e (_ bv~e ~e)) " reg prev i MEM_ENTRIES SIZE)
	  (format "(= ~a_~a_v~e (_ bv~e ~e)) " reg prev i UP SIZE)
	  (format "(= ~a_~a_v~e (_ bv~e ~e)) " reg prev i DOWN SIZE)
          (format "(= ~a_~a_v~e (_ bv~e ~e)) " reg prev i LEFT SIZE)
          (format "(= ~a_~a_v~e (_ bv~e ~e))" reg prev i RIGHT SIZE)
          (format "(= ~a_~a_v~e (_ bv~e ~e))))" reg prev i IO SIZE)
          )
	(format "(= ~a_~e_v~e ~a_~a_v~e)" reg step i reg prev i)))

  ;;; default formula for an instruction: equal to the value in the previous step
  (define check_hole (format "(= ~a_~e (_ bv~e ~e))" name step (vector-member choice choice-id) HOLE_BIT))
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
  (define check_comm (format "(= commp_~e_v~e commp_~e_v~e)" step i prev i))
  (define check_recv (format "(= recvp_~e_v~e recvp_~e_v~e)" step i prev i))

  (cond 
    ;; 2*
    [(equal? choice `2*) 
     (set! check_t (format "(= t_~e_v~e (bvshl t_~e_v~e (_ bv1 ~e)))" step i prev i SIZE))]

    ;; shiftl
    [(equal? choice `lshift)
     (set! check_t (format "(= t_~e_v~e (bvshl s_~e_v~e t_~e_v~e))" step i prev i prev i))
     (shrink)]

    ;; 2/
    [(equal? choice `2/)
     (set! check_t (format "(= t_~e_v~e (bvashr t_~e_v~e (_ bv1 ~e)))" step i prev i SIZE))]
    
    ;; shiftr
    [(equal? choice `rshift)
     (set! check_t (format "(= t_~e_v~e (bvashr s_~e_v~e t_~e_v~e))" step i prev i prev i))
     (shrink)]

    ;; /
    [(equal? choice `/)
     (set! check_t (format "(= t_~e_v~e (bvsdiv s_~e_v~e t_~e_v~e))" step i prev i prev i))
     (shrink)]

    ;; -
    [(equal? choice `-)
     (set! check_t (format "(= t_~e_v~e (bvnot t_~e_v~e))" step i prev i))]

    ;; +
    [(equal? choice `+)
     (set! check_t 
           (if (= step 1)
               (format "(= t_~e_v~e (bvadd t_~e_v~e s_~e_v~e))" step i prev i prev i)
               (format "(and (= t_~e_v~e (bvadd t_~e_v~e s_~e_v~e)) (= ~a_~a (_ bv~a ~a)))" 
                       step i prev i prev i     
                       name prev (vector-member 'nop choice-id) HOLE_BIT)))
     (shrink)]

    ;; and
    [(equal? choice `and)
     (set! check_t (format "(= t_~e_v~e (bvand t_~e_v~e s_~e_v~e))" step i prev i prev i))
     (shrink)]

    ;; or
    [(equal? choice `or)
     (set! check_t (format "(= t_~e_v~e (bvxor t_~e_v~e s_~e_v~e))" step i prev i prev i))
     (shrink)]

    ;; drop
    [(equal? choice `drop) 
     (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
     (shrink)]

    ;; dup
    [(equal? choice `dup)
     (set! check_t (format "(= t_~e_v~e t_~a_v~e)" step i prev i))
     (grow)]

    ;; @+ (can't read port)
    [(equal? choice `@+)
     (set! check_a 
           (if syn
               (format "(and (bvult a_~a_v~e (_ bv~e ~e)) (= a_~e_v~e (bvadd a_~a_v~e (_ bv1 ~a))))" 
                       prev i MEM_ENTRIES SIZE step i prev i SIZE)
               (format "(= a_~e_v~e (bvadd a_~a_v~e (_ bv1 ~a)))" step i prev i SIZE)))
     (set! check_t (format "(= t_~e_v~e (get-mem mem_~e_v~e a_~e_v~e))" step i prev i prev i))
     (grow)]

    ;; @
    [(equal? choice `@)
     (set! check_a (mem-range `a))
     (set! check_t (format "(or (bvugt a_~a_v~e (_ bv~e ~e)) (= t_~e_v~e (get-mem mem_~e_v~e a_~e_v~e)))" 
                           prev i MEM_ENTRIES SIZE     step i prev i prev i))
     (set! check_comm (read-port `a))
     (set! check_recv (recv-port `a))
     (grow)]

    ;; @b
    [(equal? choice `@b)
     (set! check_b (mem-range `b))
     (set! check_t (format "(or (bvugt b_~a_v~e (_ bv~e ~e)) (= t_~e_v~e (get-mem mem_~e_v~e b_~e_v~e)))" 
                           prev i MEM_ENTRIES SIZE     step i prev i prev i))
     (set! check_comm (read-port `b))
     (set! check_recv (recv-port `b))
     (grow)]

    ;; @p
    [(equal? choice `@p)
     (set! check_t (format "(= t_~e_v~e ~alit_~e)" step i name step))
     (grow)]

    ;; !+ (can't store to port)
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

    ;; !
    [(equal? choice `!)
     (set! check_a (mem-range `a))
     (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
     (set! check_mem (format "(ite (bvult a_~a_v~e (_ bv~e ~e)) (= mem_~e_v~e (modify-mem mem_~a_v~e a_~a_v~e t_~a_v~e)) (= mem_~e_v~e mem_~a_v~e))" 
                             step i MEM_ENTRIES SIZE step i prev i prev i prev i step i prev i))
     (set! check_comm (write-port `a))
     (shrink)]

    ;; !b
    [(equal? choice `!b)
     (set! check_b (mem-range `b))
     (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
     (set! check_mem (format "(ite (bvult b_~a_v~e (_ bv~e ~e)) (= mem_~e_v~e (modify-mem mem_~a_v~e b_~a_v~e t_~a_v~e)) (= mem_~e_v~e mem_~a_v~e))" 
                             step i MEM_ENTRIES SIZE step i prev i prev i prev i step i prev i))
     (set! check_comm (write-port `b))
     (shrink)]

    ;; a!
    [(equal? choice `a!)
     (set! check_a (format "(= a_~e_v~e t_~a_v~e)" step i prev i))
     (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
     (shrink)]

    ;; b!
    [(equal? choice `b!)
     (set! check_b (format "(= b_~e_v~e t_~a_v~e)" step i prev i))
     (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
     (shrink)]

    ;; a
    [(equal? choice `a)
     (set! check_t (format "(= t_~e_v~e a_~e_v~e)" step i prev i))
     (grow)]

    ;; +*
    [(equal? choice `+*)
     (multiply-step)]

    ;; pop
    [(equal? choice `pop)
     (set! check_t (format "(= t_~e_v~e r_~e_v~e)" step i prev i))
     (shrink-return)
     (grow)]

    ;; push
    [(equal? choice `push)
     (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
     (grow-return)
     (shrink)]

    ;; over
    [(equal? choice `over)
     (set! check_t (format "(= t_~e_v~e s_~a_v~e)" step i prev i))
     (grow)]

    ;; nop
    [(equal? choice `nop) (void)]
    )
  (string-join (list
                "(and"
                check_hole 
                check_sp
                check_rp
                check_t
                check_s
                check_r
                check_a
                check_b
                check_dst
                check_rst
                check_mem
                check_comm
                check_recv
                ")")))

;;; Handle assumption differently from assertion on input-outpout pair.
;;; Memory out of bound check and communication check happen here.
;;; This function is only called by verifier.
(define (generate-choice-assumption choice step i name)
  (define prev (sub1 step))
  
  (define (mem-range reg)
    (string-append 
      (format "(and (bvuge ~a_~a_v~e (_ bv~e ~e)) " reg prev i MEM_ENTRIES SIZE)
      (format "(not (= ~a_~a_v~e (_ bv~e ~e))) " reg prev i UP SIZE)
      (format "(not (= ~a_~a_v~e (_ bv~e ~e))) " reg prev i DOWN SIZE)
      (format "(not (= ~a_~a_v~e (_ bv~e ~e))) " reg prev i LEFT SIZE)
      (format "(not (= ~a_~a_v~e (_ bv~e ~e)))" reg prev i RIGHT SIZE)
      (format "(not (= ~a_~a_v~e (_ bv~e ~e))))" reg prev i IO SIZE)))

  (define check_assump 
  (cond 
    ;; @+ (can't read port)
    [(equal? choice `@+)
     (format "(bvuge a_~a_v~e (_ bv~e ~e))" prev i MEM_ENTRIES SIZE)]
    ;; @
    [(equal? choice `@) (mem-range `a)]
    ;; @b
    [(equal? choice `@b) (mem-range `b)]
    ;; !+ (can't store to port)
    [(equal? choice `!+)
     (format "(bvuge a_~a_v~e (_ bv~e ~e))" prev i MEM_ENTRIES SIZE)]
    ;; !
    [(equal? choice `!) (mem-range `a)]
    ;; !b
    [(equal? choice `!b) (mem-range `b)]))

    (format "(and (= ~a_~e (_ bv~e ~e)) ~a)" name step (vector-member choice choice-id) HOLE_BIT check_assump))

;;; Returns the bv constant representing the given time (n).
(define (bv-time n)
  (format "(_ bv~a ~a)" n TIME_SIZE))

(define (bv n size)
  (format "(_ bv~a ~a)" n size))

(define (generate-time-constraint step)
  (pretty-display (format "(assert (= time_~a (bvadd time_~a (ite (bvult h_~a (_ bv~a ~a)) ~a ~a))))"
                          step (sub1 step) step N_OF_SLOW HOLE_BIT (bv-time 10) (bv-time 3))))

(define (generate-length-constraint step)
  (pretty-display (format "(assert (= length_~a (bvadd length_~a (ite (= h_~a (_ bv~a ~a)) (_ bv5 ~a) (_ bv1 ~a)))))"
                          step (sub1 step) 
                          step (vector-member `@p choice-id) HOLE_BIT LENGTH_SIZE LENGTH_SIZE)))

;;; Generates assertions for holes which are already know, given a
;;; spec. The spec should have normal instructions as well as
;;; underscores for holes.
(define (generate-known-holes sketch begin n)
  (let* ([instrs (list->vector (program->instructions sketch))]
         [len    (vector-length instrs)]) ;; TODO: get rid of len?
    (for ([i (in-range 0 n)])
         (let* ([instr (vector-ref instrs i)]
                [num   (string->number instr)])
           (unless (equal? instr "_")
             (pretty-display (format "(assert (= h_~a (_ bv~a ~a)))"
                                     (add1 (+ i begin)) (car (to-choice (string->symbol instr))) HOLE_BIT))
             (when num
               (pretty-display (format "(assert (= hlit_~a (_ bv~a ~a)))"
                                       (add1 (+ i begin)) num SIZE))))))))

;;; Returns an expression counting the number of trailing nops given
;;; the total number of slots. This can then be subtracted from the
;;; total time.
(define (nop-offset begin end cost size)
  (define bv-nop (format "(_ bv~a ~a)" (vector-member 'nop choice-id) HOLE_BIT))
  (define (go hole-number expr)
    (format "(ite (= h_~a ~a) (bvadd ~a ~a) ~a)"
            hole-number bv-nop (bv cost size) expr (bv 0 size)))
  (define start (format "(ite (= h_~a ~a) ~a ~a)" begin bv-nop (bv cost size) (bv 0 size)))
  (foldr go start (reverse (stream->list (in-range (add1 begin) (add1 end))))))

(define (generate-time-constraints begin end repeat time-limit)
  (for* ([step (in-range begin (add1 end))])
        (pretty-display `(declare-const ,(var-no-v `time step) ,(makeBV TIME_SIZE))))
  (pretty-display `(assert (= ,(var-no-v `time begin) (_ bv0 ,TIME_SIZE))))
  (for* ([step (in-range (add1 begin) (add1 end))])
	(generate-time-constraint step))
  (pretty-display `(declare-const total_time ,(makeBV TIME_SIZE)))
  (pretty-display (format "(assert (= total_time (bvmul (bvsub time_~a ~a) (_ bv~a ~a))))" 
			  end (nop-offset (add1 begin) end 3 TIME_SIZE) repeat TIME_SIZE))
  (pretty-display (format "(assert (bvult total_time (_ bv~a ~a)))" time-limit TIME_SIZE)))

(define (generate-length-constraints begin end length-limit)
  (for* ([step (in-range begin (add1 end))])
        (pretty-display `(declare-const ,(var-no-v `length step) ,(makeBV LENGTH_SIZE))))
  (pretty-display `(assert (= ,(var-no-v `length begin) (_ bv0 ,LENGTH_SIZE))))
  (for* ([step (in-range (add1 begin) (add1 end))])
	(generate-length-constraint step))
  (pretty-display `(declare-const total_length ,(makeBV TIME_SIZE)))
  (pretty-display (format "(assert (= total_length (bvsub length_~a ~a)))" 
                          end (nop-offset (add1 begin) end 1 LENGTH_SIZE)))
  (pretty-display (format "(assert (bvult total_length (_ bv~a ~a)))" length-limit LENGTH_SIZE)))

;;; Generates assertions for repeated body such that 
;;; the i-th hole in the body is the same for all copies.
(define (generate-repeat-constraints init body repeat)
  (for* ([i (in-range 0 body)]
	 [t (in-range 1 repeat)])
       (pretty-display (format "(assert (= h_~a h_~a))" (add1 (+ init i)) (add1 (+ init (+ i (* t body))))))
       (pretty-display (format "(assert (= hlit_~a hlit_~a))" (add1 (+ init i)) (add1 (+ init (+ i (* t body))))))))

 
(define (generate-formula step n version name syn [sup (support)])
  (define clauses (make-vector (vector-length sup)))
  (when (= (modulo step 4) 0)
	(pretty-display (format "(assert (= (_ bv0 2) ((_ extract 1 0) ~a_~a)))" name step)))
  (for* ([i (in-range 0 (vector-length sup))])
	(vector-set! clauses i (generate-choice (vector-ref sup i) step n version name syn)))
  (pretty-display `(assert ,(conjunct clauses (vector-length sup) `or))))

(define (generate-formulas n from to name syn [sup (support)])
  (for* ([version (in-range from to)]
	 [step (in-range 1 n)])
    (generate-formula step n version name syn sup)))

(define (generate-known-formulas program n version name syn)
  (for ([step (in-range 1 n)])
       (define choice (vector-ref choice-id (vector-ref program (sub1 step))))
       (pretty-display `(assert ,(generate-choice choice step n version name syn)))))

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
        ;; (pretty-display (format "(assert (= dst_~e_v~e (_ bv~e ~e)))" n i (stack-body (progstate-data state)) STACK_SIZE))
        (define stack (progstate-data state))
        (for ([nth (in-range (progstate-data output-constraint))])
             (pretty-display (format "(assert (= ~a (_ bv~a ~a)))"
                                     (nth-stack n i nth) 
                                     (data-at (stack-body stack) 
                                              (modulo (- (stack-sp stack) nth) 8)) 
                                     SIZE)))
        ;; (pretty-display (format "(assert (= sp_~e_v~e (_ bv~e ~e)))" 
        ;;                         n i (stack-sp stack) 3))
        )
  (when (progstate-return output-constraint)
	(pretty-display (format "(assert (= rst_~e_v~e (_ bv~e ~e)))" n i (stack-body (progstate-return state)) STACK_SIZE))
	(pretty-display (format "(assert (= rp_~e_v~e (_ bv~e ~e)))" n i (stack-sp (progstate-return state)) 3)))
  (when (progstate-memory output-constraint)
	(pretty-display (format "(assert (= mem_~e_v~e (_ bv~e ~e)))" n i (progstate-memory state) MEM_SIZE)))
  (when (progstate-t output-constraint)
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
  (pretty-display (format "(assert (= commdata_v~e (_ bv~e ~e)))" 
                          i (commstate-data comm) COMM_SIZE))
  (pretty-display (format "(assert (= recvdata_v~e (_ bv~e ~e)))" 
                          i (commstate-recv comm) COMM_SIZE))
  (pretty-display (format "(assert (= commtype_v~e (_ bv~e ~e)))" 
                          i (commstate-type comm) COMMTYPE_SIZE))

  (pretty-display (format "(assert (= commp_~e_v~e (_ bv~e ~e)))" 
                          n i (commstate-p comm) COMM_BIT))
  ;; (pretty-display (format "(assert (= recvp_~e_v~e (_ bv~e ~e)))" 
  ;;                         n i (vector-length (commstate-recv comm)) COMM_BIT)))
)

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

(define (top-stack step i)
  (format "(get-stack dst_~e_v~e sp_~e_v~e)" step i step i))

(define (nth-stack step i n)
  (format "(get-stack dst_~e_v~e (bvsub sp_~e_v~e (_ bv~e 3)))"
          step i step i n))

(define (data-at stack index)
  (modulo (arithmetic-shift stack (- 0 (* index SIZE)))
          (arithmetic-shift 1 SIZE)))

(define (assert-input-eq)
  (for ([var `(dst rst mem t s r a b sp rp)])
       (pretty-display (format "(assert (= ~a_0_v0 ~a_0_v1))" var var)))
  (pretty-display "(assert (= recvdata_v0 recvdata_v1))"))

(define (assert-output-neq)
  ;;; Add this assertion to set the irrelevant communcation entries to 0
  ;;; so that it's easy for comparision
  (define (assert-ir-comm v n)
    (for ([i (in-range 0 COMM_ENTRIES)])
         (define index (format "(_ bv~a ~a)" i COMM_BIT))
         (pretty-display (format "(assert (or (bvult ~a commp_~a_v~a) (= (get-comm commdata_v~a ~a) (_ bv0 ~a))))" 
        			  index n v v index SIZE))
         (pretty-display (format "(assert (or (bvult ~a commp_~a_v~a) (= (get-commtype commtype_v~a ~a) (_ bv0 ~a))))" 
        			  index n v v index COMMTYPE_UNIT))
    ))

  (assert-ir-comm 0 spec-count)
  (assert-ir-comm 1 cand-count)

  ;;; Assert outputs are not equal
  ;(define clauses (make-vector 26))
  ;(define index 0)
  (define clauses (list))

  (define (list-add-var var)
    (set! clauses (cons (format "(not (= ~a_~e_v0 ~a_~e_v1))" var spec-count var cand-count) clauses)))

  (define (list-add x)
    (set! clauses (cons x clauses)))

  (list-add "(not (= commdata_v0 commdata_v1))")
  (list-add "(not (= commtype_v0 commtype_v1))")
  (list-add-var `commp)
  (list-add-var `recvp)

  (when (progstate-data output-constraint)
        (define data (progstate-data output-constraint))
        ;; (set! clauses (cons (format "(not (= ~a ~a))" 
        ;;                             (top-stack spec-count 0)
        ;;                             (top-stack cand-count 1))
        ;;                     clauses))
        (for ([i (in-range (progstate-data output-constraint))])
             (list-add (format "(not (= ~a ~a))"
                                         (nth-stack spec-count 0 i)
                                         (nth-stack cand-count 1 i))))
	;(list-add `dst)
	;(list-add `sp)
        )
  (when (progstate-return output-constraint)
	(list-add-var `rst)
	(list-add-var `rp))
  (when (progstate-memory output-constraint)
	(list-add-var `mem))
  (when (progstate-t output-constraint)
	(list-add-var `t))
  (when (progstate-s output-constraint)
	(list-add-var `s))
  (when (progstate-r output-constraint)
	(list-add-var `r))
  (when (progstate-a output-constraint)
	(list-add-var `a))
  (when (progstate-b output-constraint)
	(list-add-var `b))

  ;;; Include assumption throughtout the program
  (pretty-display (format "(assert (or ~a ~a))" (string-join clauses) (generate-assumptions (add1 cand-count) 1 2 `cand))))

;;;;;;;;;;;;;;;;;;;;;;;; Synthesizer (and general formula generator) ;;;;;;;;;;;;;;;;;;;;;;;;

(define (synthesize-prog sketch init repeat has-prog has-out time-limit length-limit)
  (define n (if (number? sketch) sketch (program-length sketch)))
  (define init-n (if (number? init) init (program-length init)))
  (define slots (+ init-n (* n repeat)))
  (define slots+1 (add1 slots))

  (declare-bitvector)
  (declare-functions)
  (newline)

  ;; holes
  (if has-prog
      (encode-program spec spec-lit slots+1 `h)
      (declare-holes slots+1))

  (declare-vars slots+1 0 (length inout-list))

  (unless (number? init) (generate-known-holes init 0 init-n))
  (unless (number? sketch)
	  (for ([i (in-range 0 repeat)])
	       (generate-known-holes sketch (+ init-n (* i n)) n)))
  (generate-repeat-constraints init-n n repeat)

  ;; formula
  (newline)
  (generate-formulas slots+1 0 (length inout-list) `h #t)

  ;; input-output
  (newline)
  (assert-input-output slots has-out)
  (newline)
  (when time-limit (generate-time-constraints init-n (+ init-n n) repeat time-limit))
  (when length-limit (generate-length-constraints init-n (+ init-n n) length-limit))
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
  (generate-known-formulas spec (add1 spec-count) 0 `spec #t)
  (newline)

  ; formular for candidate
  (encode-program cand cand-lit (add1 cand-count) `cand)
  (newline)
  (declare-vars (add1 cand-count) 1 2)
  (newline)
  (generate-known-formulas cand (add1 cand-count) 1 `cand #f)
  (newline)
  
  (assert-input-eq)
  (newline)
  (assert-output-neq)
  (newline)
  (pretty-display `(check-sat))
  (pretty-display `(get-model))
)

;;;;;;;;;;;;;;;;;;;;;;;; Input-Output-Rend-Recv storage, converter, and generator ;;;;;;;;;;;;;;;;;;;;;;;;


(define (vec-to-bits vec vec-size [nbit SIZE])
  (define (vec-to-bits-inner)
    (define bits 0)
    (for* ([i (in-range 0 vec-size)])
	  (define ele (if (< (- (- vec-size i) 1) (vector-length vec))
			  (vector-ref vec (- (- vec-size i) 1))
			  0))
	  (set! bits (+ (arithmetic-shift bits nbit) (modulo ele (arithmetic-shift 1 nbit)))))
    bits)

  (if (vector? vec)
      (vec-to-bits-inner)
      vec))

(define (convert-progstate state)
  ;(pretty-display `(convert-progstate ,(progstate-memory state)))
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
  (commstate (vec-to-bits (commstate-data state) COMM_ENTRIES)
             (vec-to-bits (commstate-type state) COMM_ENTRIES COMMTYPE_UNIT)
             (vec-to-bits (commstate-recv state) COMM_ENTRIES)
             (commstate-p state)))

;;;;;;;;;;;;;;;;;;;;;;;; GreenSyn API ;;;;;;;;;;;;;;;;;;;;;;;;

(define current-input 0)
(define current-output 0)
(define current-comm 0)

(provide (all-defined-out))

;;; Set
;;; 1) number of entries of memory
;;; 2) number of entries of send/recv storage of each 4 neighbors
(define (greensyn-reset mem-entries comm-entries constraint #:num-bits [num-bits 18] #:inst-pool [support `no-fake] )
  (set! output-constraint constraint)
  (set! SUPPORT support)

  (set! SIZE num-bits) ; number of bits per word
  (set! TYPE (string->symbol (format "BV~e" SIZE)))
  (set! MAX (sub1 (arithmetic-shift 1 SIZE)))
  (set! EXP_MASK (string->symbol (format "bv~e" MAX)))

  (set! STACK_SIZE (* 8 SIZE))
  (set! STACK_TYPE (string->symbol (format "BV~e" STACK_SIZE)))

  (set! MEM_ENTRIES mem-entries)
  (set! MEM_SIZE (* MEM_ENTRIES SIZE))
  (set! MEM_TYPE (string->symbol (format "BV~e" MEM_SIZE)))

  (set! COMM_ENTRIES comm-entries)
  (set! COMM_SIZE (* COMM_ENTRIES SIZE))
  (set! COMMTYPE_SIZE (* COMM_ENTRIES 4))
  (set! COMM_TYPE (string->symbol (format "BV~e" COMM_SIZE)))
  (set! COMM_BIT (inexact->exact (floor (+ (/ (log (* 2 COMM_ENTRIES)) (log 2)) 1))))

  (set! inout-list '()))

;;; Set input for counterexmaple.
(define (greensyn-input state)
  (set! current-input (convert-progstate state)))

;;; Set output for counterexmaple.
(define (greensyn-output state)
  (set! current-output (convert-progstate state)))

;;; Set send/recv storage for counterexmaple.
(define (greensyn-send-recv state)
  (set! current-comm (convert-commstate state))
  )

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
(define (greensyn-check-sat #:file [file "prog.smt2"] sketch [init 0] [repeat 1] 
                            #:time-limit [time-limit #f] #:length-limit [length-limit #f])
  (define out (open-output-file file #:exists 'replace))
  (parameterize ([current-output-port out])
    (synthesize-prog sketch init repeat #f #t time-limit length-limit))
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
;;; (greensyn-spec)
;;; (greensyn-input input) 
;;; (greensyn-output output) 
;;; (greensyn-send-recv send-recv)? 
;;; (greensyn-commit) }
;;; (greensyn-gen-formula string_of_candidate assert_output)
(define (greensyn-gen-formula file has-out)
  (define out (open-output-file file #:exists 'replace))
  (parameterize ([current-output-port out])
    (synthesize-prog spec-count 0 1 #t has-out))
  (close-output-port out))
  
  
