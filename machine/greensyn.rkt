#lang racket

;(define TYPE `BV4)
;(define LOG_SIZE 2)

(define SIZE 18)

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

; this is consistent with arrayForth
(define UP #x145)
(define DOWN #x115)
(define LEFT #x175)
(define RIGHT #x1d5)
(define IO #x15d)

(define HOLE_SIZE 5)
(define CHOICES 22)

(struct progstate (dst rst mem t s r a b sp rp) #:mutable) ;rp
(struct commstate (send-u send-d send-l send-r recv-u recv-d recv-l recv-r sendp-u sendp-d sendp-l sendp-r recvp-u recvp-d recvp-l recvp-r))
(struct inout (input output comm) #:mutable)

(define inout-list (make-vector 10))
(define num-inout 0)

(define spec (make-vector 100))
(define spec-count 0)
(define cand (make-vector 100))
(define cand-count 0)

(define choice-id '#(2* 2/ - + and or drop dup @+ @ @b !+ ! !b a! b! up down left right nop 1))

(define (to-choice name)
  ;; (display (format "to-choice ~e ~e\n" name (vector-member name choice-id)))
  (or (vector-member name choice-id) (raise (format "Cannot synthesize ~s!" name))))

(define (compile code output)
  (define count 0)
  (define in (open-input-string code))
  (define (go)
    (define (append-vector inst)
      (vector-set! output count inst)
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
  (pretty-display `(define-sort ,(makeBV HOLE_SIZE) () (_ BitVec ,HOLE_SIZE)))   ; for hole
  (pretty-display `(define-sort ,(makeBV SIZE) () (_ BitVec ,SIZE)))             ; for normal variables
  (pretty-display `(define-sort ,(makeBV STACK_SIZE) () (_ BitVec ,STACK_SIZE))) ; for stack
  (pretty-display `(define-sort ,(makeBV MEM_SIZE) () (_ BitVec ,MEM_SIZE)))     ; for mem
  (pretty-display `(define-sort ,(makeBV COMM_SIZE) () (_ BitVec ,COMM_SIZE)))   ; for comm
  (pretty-display `(define-sort ,(makeBV COMM_BIT) () (_ BitVec ,COMM_BIT)))     ; for comm index
  )

(define-syntax-rule (define-fun id form ...)
  (define id `(define-fun id form ...)))

; for power-of-2-bit
;(define-fun bitidx ((i BV3)) (,STACK_TYPE)
;  (concat (_ bv0 ,(- (- STACK_SIZE 3) LOG_SIZE)) (concat i (_ bv0 ,LOG_SIZE))))

(define (declare-bitidx func-name array-size index-size)
  (define array-type (string->symbol (format "BV~e" array-size)))
  (define index-type (string->symbol (format "BV~e" index-size)))
  (newline)
  (pretty-display
   `(define-fun ,func-name ((i ,index-type)) (,array-type)
      (bvmul (concat (_ bv0 ,(- array-size index-size)) i) (_ ,(makebv SIZE) ,array-size)))))

(define (declare-get func-name array-size index-size index-func)
  (define array-type (string->symbol (format "BV~e" array-size)))
  (define index-type (string->symbol (format "BV~e" index-size)))
  (newline)
  (pretty-display 
   `(define-fun ,func-name ((array ,array-type) (index ,index-type)) (,TYPE)
      ((_ extract ,(sub1 SIZE) 0) (bvlshr array (,index-func index))))))

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

(define (declare-init v n i bit)
  (for* ([step (in-range 0 n)])
    (pretty-display `(declare-const ,(var v step i) ,(makeBV bit)))))

(define (declare-init-zero v n i bit)
  (for* ([step (in-range 0 n)])
    (pretty-display `(declare-const ,(var v step i) ,(makeBV bit))))
  (pretty-display `(assert (= ,(var v 0 i) (_ ,(makebv 0) ,bit)))))

(define (declare-vector-one name i vec-size #:init [vec (make-vector vec-size 0)])
  (define all 0)
  (for* ([i (in-range 0 vec-size)])
    (set! all (+ (arithmetic-shift all SIZE) 
                 (vector-ref vec (- (- vec-size i) 1)))))
  (pretty-display `(declare-const ,(var name i) ,(makeBV (* vec-size SIZE)))))

(define (declare-holes n)
  (for* ([step (in-range 1 n)])
    (pretty-display `(declare-const ,(var-no-v `h step) ,(makeBV HOLE_SIZE)))))

(define (encode-program prog n name)
  (for* ([step (in-range 1 n)])
    (pretty-display `(declare-const ,(var-no-v name step) ,(makeBV HOLE_SIZE)))
    (pretty-display `(assert (= ,(var-no-v name step) (_ ,(makebv (vector-ref prog (sub1 step))) ,HOLE_SIZE))))))

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

(define (generate-choice choice step n i name)
  (define prev (sub1 step))

  (define (shrink)
    (set! check_s (format "(= s_~e_v~e (get-stack dst_~e_v~e sp_~e_v~e))" step i prev i prev i))
    (set! check_sp (format "(= sp_~e_v~e (bvsub sp_~e_v~e (_ bv1 3))) " step i prev i)))

  (define (grow)
    (set! check_s (format "(= s_~e_v~e t_~a_v~e)" step i prev i))
    (set! check_sp (format "(= sp_~e_v~e (bvadd sp_~a_v~e (_ bv1 ~a))) " step i prev i 3))
    (set! check_dst (format "(= dst_~e_v~e (modify-stack dst_~a_v~e sp_~e_v~e s_~a_v~e))" step i prev i step i prev i)))
  
  ; for pushing  c to stack
  (define (push-const c)
    (set! check_t (format "(= t_~e_v~e (_ bv~e ~e))" step i c SIZE))
    (grow))

  (define (write-port reg port val)
    (string-append (string-append
      (format  "(ite (= ~a_~a_v~e (_ bv~e ~e))" reg prev i val SIZE)
      (format  "(and (and (= (get-comm send~a_v~e sendp~a_~a_v~e) t_~a_v~e) (= sendp~a_~e_v~e (bvadd sendp~a_~a_v~e (_ bv1 ~e)))) (bvule sendp~e_~e_v~e sendp~e_~e_v~e))"
	       port i port prev i prev i     port step i port prev i COMM_BIT     port step i port (sub1 n) i))
      (format  "(= sendp~e_~e_v~e sendp~e_~e_v~e))" port step i port prev i)))
    
  (define (read-port reg port val)
    (string-append (string-append
      (format "(ite (= ~a_~a_v~e (_ bv~e ~e))" reg prev i val SIZE)
      (format "(and (and (= (get-comm recv~a_v~e recvp~a_~a_v~e) t_~a_v~e) (= recvp~a_~e_v~e (bvadd recvp~a_~a_v~e (_ bv1 ~e)))) (bvule sendp~e_~e_v~e sendp~e_~e_v~e))"
	      port i port prev i step i     port step i port prev i COMM_BIT     port step i port (sub1 n) i))
      (format "(= recvp~e_~e_v~e recvp~e_~e_v~e))" port step i port prev i)))
  
  (define (mem-range reg)
    (string-append (string-append (string-append (string-append (string-append
      (format "(and (= ~a_~e_v~e ~a_~a_v~e) " reg step i reg prev i)
      (format "(or (or (or (or (bvult ~a_~a_v~e (_ bv~e ~e)) " reg prev i MEM_ENTRIES SIZE))
      (format "(= ~a_~a_v~e (_ bv~e ~e))) " reg prev i UP SIZE))
      (format "(= ~a_~a_v~e (_ bv~e ~e))) " reg prev i DOWN SIZE))
      (format "(= ~a_~a_v~e (_ bv~e ~e))) " reg prev i LEFT SIZE))
      (format "(= ~a_~a_v~e (_ bv~e ~e))))" reg prev i RIGHT SIZE)))

  (define check_hole (format "(and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (= ~a_~e (_ bv~e ~e)) " name step choice HOLE_SIZE))
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
    [(= choice (vector-member `2* choice-id)) 
               (set! check_t (format "(= t_~e_v~e (bvshl t_~e_v~e (_ bv1 ~e)))" step i prev i SIZE))]
    ; 2/
    [(= choice (vector-member `2/ choice-id)) 
               (set! check_t (format "(= t_~e_v~e (bvlshr t_~e_v~e (_ bv1 ~e)))" step i prev i SIZE))]
    ; -
    [(= choice (vector-member `- choice-id)) 
               (set! check_t (format "(= t_~e_v~e (bvnot t_~e_v~e))" step i prev i))]
    ; +
    [(= choice (vector-member `+ choice-id)) 
               (set! check_t (format "(= t_~e_v~e (bvadd t_~e_v~e s_~e_v~e))" step i prev i prev i))
               (shrink)]
    ; and
    [(= choice (vector-member `and choice-id)) 
               (set! check_t (format "(= t_~e_v~e (bvand t_~e_v~e s_~e_v~e))" step i prev i prev i))
               (shrink)]
    ; or
    [(= choice (vector-member `or choice-id)) 
               (set! check_t (format "(= t_~e_v~e (bvxor t_~e_v~e s_~e_v~e))" step i prev i prev i))
               (shrink)]
    ; drop
    [(= choice (vector-member `drop choice-id)) 
               (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
               (shrink)]
    ; dup
    [(= choice (vector-member `dup choice-id)) 
               (set! check_t (format "(= t_~e_v~e t_~a_v~e)" step i prev i))
	       (grow)]
    ; @+ (can't read port)
    [(= choice (vector-member `@+ choice-id)) 
               (set! check_a (format "(and (bvult a_~a_v~e (_ bv~e ~e)) (= a_~e_v~e (bvadd a_~a_v~e (_ bv1 ~a))))" 
				     prev i MEM_ENTRIES SIZE step i prev i SIZE))
	       (set! check_t (format "(= t_~e_v~e (get-mem mem_~e_v~e a_~e_v~e))" step i prev i prev i))
	       (grow)]
    ; @
    [(= choice (vector-member `@ choice-id)) 
               (set! check_a (mem-range `a))
	       (set! check_t (format "(or (bvugt a_~a_v~e (_ bv~e ~e)) (= t_~e_v~e (get-mem mem_~e_v~e a_~e_v~e)))" 
				     prev i MEM_ENTRIES SIZE     step i prev i prev i))
	       (grow)
	       (set! check_recvp_u (read-port `a U-ID UP))
	       (set! check_recvp_d (read-port `a D-ID DOWN))
	       (set! check_recvp_l (read-port `a L-ID LEFT))
	       (set! check_recvp_r (read-port `a R-ID RIGHT))]
    ; @b
    [(= choice (vector-member `@b choice-id)) 
               (set! check_b (mem-range `b))
	       (set! check_t (format "(or (bvugt b_~a_v~e (_ bv~e ~e)) (= t_~e_v~e (get-mem mem_~e_v~e b_~e_v~e)))" 
				     prev i MEM_ENTRIES SIZE     step i prev i prev i))
	       (set! check_recvp_u (read-port `b U-ID UP))
	       (set! check_recvp_d (read-port `b D-ID DOWN))
	       (set! check_recvp_l (read-port `b L-ID LEFT))
	       (set! check_recvp_r (read-port `b R-ID RIGHT))
	       (grow)]
    ; !+ (can't store to port)
    [(= choice (vector-member `!+ choice-id)) 
               (set! check_a (format "(and (bvult a_~a_v~e (_ bv~e ~e)) (= a_~e_v~e (bvadd a_~a_v~e (_ bv1 ~a))))" 
				     prev i MEM_ENTRIES SIZE step i prev i SIZE))
	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
	       (set! check_mem (format "(= mem_~e_v~e (modify-mem mem_~a_v~e a_~a_v~e t_~a_v~e))" 
				       step i prev i prev i prev i))
	       (shrink)]
    ; !
    [(= choice (vector-member `! choice-id)) 
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
    [(= choice (vector-member `!b choice-id)) 
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
    [(= choice (vector-member `a! choice-id)) 
               (set! check_a (format "(= a_~e_v~e t_~a_v~e)" step i prev i))
	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
	       (shrink)]
    ; b!
    [(= choice (vector-member `b! choice-id)) 
               (set! check_b (format "(= b_~e_v~e t_~a_v~e)" step i prev i))
	       (set! check_t (format "(= t_~e_v~e s_~e_v~e)" step i prev i))
	       (shrink)]
    [(= choice (vector-member `up choice-id)) (push-const UP)]
    [(= choice (vector-member `down choice-id)) (push-const DOWN)]
    [(= choice (vector-member `left choice-id)) (push-const LEFT)]
    [(= choice (vector-member `right choice-id)) (push-const RIGHT)]
    ; nop
    [(= choice (vector-member `nop choice-id)) (void)]
    [(= choice (vector-member `1 choice-id)) (push-const 1)]
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

(define (generate-formula step n version name)
 (define s "(assert ")
 (for* ([i (in-range 1 CHOICES)])
   (set! s (string-append s "(or ")))
 (pretty-display s)
 (pretty-display (generate-choice 0 step n version name))
 (for* ([i (in-range 1 CHOICES)])
   (pretty-display (string-append (generate-choice i step n version name) ")")))
 (pretty-display ")"))
 
(define (generate-formulas n from to name)
  (for* ([version (in-range from to)]
	 [step (in-range 1 n)])
    (generate-formula step n version name)))

(define (assert-state-all state n i)
  (pretty-display (format "(assert (= dst_~e_v~e (_ bv~e ~e)))" n i (progstate-dst state) STACK_SIZE))
  (pretty-display (format "(assert (= rst_~e_v~e (_ bv~e ~e)))" n i (progstate-rst state) STACK_SIZE))
  (pretty-display (format "(assert (= mem_~e_v~e (_ bv~e ~e)))" n  i(progstate-mem state) MEM_SIZE))
  (pretty-display (format "(assert (= t_~e_v~e (_ bv~e ~e)))" n  i(progstate-t state) SIZE))
  (pretty-display (format "(assert (= s_~e_v~e (_ bv~e ~e)))" n i (progstate-s state) SIZE))
  (pretty-display (format "(assert (= r_~e_v~e (_ bv~e ~e)))" n i (progstate-r state) SIZE))
  (pretty-display (format "(assert (= a_~e_v~e (_ bv~e ~e)))" n i (progstate-a state) SIZE))
  (pretty-display (format "(assert (= b_~e_v~e (_ bv~e ~e)))" n i (progstate-b state) SIZE))
  (pretty-display (format "(assert (= sp_~e_v~e (_ bv~e ~e)))" n i (progstate-sp state) 3))
  (pretty-display (format "(assert (= rp_~e_v~e (_ bv~e ~e)))" n i (progstate-rp state) 3)))

(define (assert-state state n i)
  (pretty-display (format "(assert (= dst_~e_v~e (_ bv~e ~e)))" n i (progstate-dst state) STACK_SIZE))
  (pretty-display (format "(assert (= rst_~e_v~e (_ bv~e ~e)))" n i (progstate-rst state) STACK_SIZE))
  (pretty-display (format "(assert (= mem_~e_v~e (_ bv~e ~e)))" n  i(progstate-mem state) MEM_SIZE))
  (pretty-display (format "(assert (= t_~e_v~e (_ bv~e ~e)))" n  i(progstate-t state) SIZE))
  (pretty-display (format "(assert (= s_~e_v~e (_ bv~e ~e)))" n i (progstate-s state) SIZE))
  (pretty-display (format "(assert (= r_~e_v~e (_ bv~e ~e)))" n i (progstate-r state) SIZE))
  (pretty-display (format "(assert (= a_~e_v~e (_ bv~e ~e)))" n i (progstate-a state) SIZE))
  (pretty-display (format "(assert (= b_~e_v~e (_ bv~e ~e)))" n i (progstate-b state) SIZE))
  (pretty-display (format "(assert (= sp_~e_v~e (_ bv~e ~e)))" n i (progstate-sp state) 3))
  (pretty-display (format "(assert (= rp_~e_v~e (_ bv~e ~e)))" n i (progstate-rp state) 3))
  )

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

(define (assert-pair input output comm n i)
  (assert-state-all input 0 i)
  (assert-state output n i)
  (assert-comm comm n i)
  )

(define (assert-input-output n)
  (for* ([i (in-range 0 num-inout)])
    (define p (vector-ref inout-list i))
    (assert-pair (inout-input p) (inout-output p) (inout-comm p) n i)))

(define (assert-input-eq)
  (for ([var `(dst rst mem t s r a b sp rp)])
       (pretty-display (format "(assert (= ~a_0_v0 ~a_0_v1))" var var))))

(define (assert-output-neq)
  (define (assert-ir-comm v n)
    (for* ([port `(send recv)]
	   [p (in-range 0 4)]
	   [i (in-range 0 COMM_ENTRIES)])
	  (define index (format "(_ bv~a ~a)" i COMM_BIT))
	  (pretty-display (format "(assert (or (bvult ~a ~ap~a_~a_v~a) (= (get-comm ~a~a_v~a ~a) (_ bv0 ~a))))" 
				  index port p n v   port p v index SIZE))))
  (assert-ir-comm 0 spec-count)
  (assert-ir-comm 1 cand-count)
  (define s "(assert ")
  (for ([i (in-range 0 25)]) ; 10 stat var + 8 comm pointers + comm
       (set! s (string-append s "(or ")))
  (set! s (string-append s (format "(not (= dst_~e_v0 dst_~e_v1))" spec-count cand-count)))
  (for ([var `(rst mem t s r a b sp rp sendp0 sendp1 sendp2 sendp3 recvp0 recvp1 recvp2 recvp3)])
       (set! s (string-append s (format " (not (= ~a_~e_v0 ~a_~e_v1)))" var spec-count var cand-count))))
  (for* ([var `(send recv)]
	 [channel (in-range 0 4)])
       (set! s (string-append s (format " (not (= ~a~a_v0 ~a~a_v1)))" var channel var channel))))
  (set! s (string-append s ")"))
  (pretty-display s))
  
(define (synthesize-prog n)
  (declare-bitvector)
  (declare-vars (add1 n) 0 num-inout)
  (declare-holes (add1 n))
  (newline)
  (declare-functions)
  (newline)
  (generate-formulas (add1 n) 0 num-inout `h)
  (newline)
  (assert-input-output n)
  (newline)
  (pretty-display `(check-sat))
  (pretty-display `(get-model))
)

(define (verify-prog)
  (declare-bitvector)
  (declare-functions)
  (newline)

  ; formular for spec
  (encode-program spec (add1 spec-count) `spec)
  (newline)
  (declare-vars (add1 spec-count) 0 1)
  (newline)
  (generate-formulas (add1 spec-count) 0 1 `spec)
  (newline)

  ; formular for candidate
  (encode-program cand (add1 cand-count) `cand)
  (newline)
  (declare-vars (add1 cand-count) 1 2)
  (newline)
  (generate-formulas (add1 cand-count) 1 2 `cand)
  (newline)
  
  (assert-input-eq)
  (newline)
  (assert-output-neq)
  (newline)
  (pretty-display `(check-sat))
  (pretty-display `(get-model))
)

;;;;;;;;;;;;;;;;;;;;;;;; Input-Output-Rend-Recv storage, converter, and generator ;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-progstate
          #:dst [dst (make-vector 8 #x15555)]
          #:rst [rst (make-vector 8 #x15555)]
          #:mem [mem (make-vector MEM_ENTRIES 0)]
          #:t [t #x15555] #:s [s #x15555] #:r [r #x15555] #:a [a #x15555] #:b [b IO]
	  #:sp [sp 0] #:rp [rp 0])
  (progstate dst rst mem t s r a b sp rp))

(define (default-commstate
	 #:send-u [send-u (make-vector COMM_ENTRIES 0)]
	 #:send-d [send-d (make-vector COMM_ENTRIES 0)]
	 #:send-l [send-l (make-vector COMM_ENTRIES 0)]
	 #:send-r [send-r (make-vector COMM_ENTRIES 0)]
	 #:recv-u [recv-u (make-vector COMM_ENTRIES 0)]
	 #:recv-d [recv-d (make-vector COMM_ENTRIES 0)]
	 #:recv-l [recv-l (make-vector COMM_ENTRIES 0)]
	 #:recv-r [recv-r (make-vector COMM_ENTRIES 0)])
  (commstate send-u send-d send-l send-r recv-u recv-d recv-l recv-r 0 0 0 0 0 0 0 0))

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
  (progstate (vec-to-bits (progstate-dst state) 8) 
             (vec-to-bits (progstate-rst state) 8) 
             (vec-to-bits (progstate-mem state) MEM_ENTRIES) 
             (progstate-t state) (progstate-s state) (progstate-r state) 
             (progstate-a state) (progstate-b state) 
             (progstate-sp state) (progstate-rp state)))
  

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

(define (add-input-output input output comm)
  (vector-set! inout-list num-inout (inout (convert-progstate input) (convert-progstate output) (convert-commstate comm)))
  (set! num-inout (add1 num-inout)))

;;;;;;;;;;;;;;;;;;;;;;;; GreenSyn API ;;;;;;;;;;;;;;;;;;;;;;;;

(define current-input (default-progstate))
(define current-output (default-progstate))
(define current-comm (default-commstate))

(provide (all-defined-out))

;;; Set
;;; 1) number of entries of memory
;;; 2) number of entries of send/recv storage of each 4 neighbors
;;; 3) number of bits for indexing to send/recv storage.
(define (greensyn-reset mem-entries comm-entries comm_bit)
  (set! MEM_ENTRIES mem-entries)
  (set! MEM_SIZE (* MEM_ENTRIES SIZE))
  (set! MEM_TYPE (string->symbol (format "BV~e" MEM_SIZE)))

  (set! COMM_ENTRIES comm-entries)
  (set! COMM_SIZE (* COMM_ENTRIES SIZE))
  (set! COMM_TYPE (string->symbol (format "BV~e" COMM_SIZE)))
  ;(set! COMM_BIT (floor (+ (/ (log COMM_ENTRIES) (log COMM_ENTRIES)) 1)))
  (set! COMM_BIT comm_bit)

  (set! num-inout 0)
  (set! current-input (default-progstate))
  (set! current-output (default-progstate))
  (set! current-comm (default-commstate)))

;;; Set input for counterexmaple.
(define (greensyn-input dst rst mem t s r a b sp rp)
  (set! current-input (convert-progstate (progstate dst rst mem t s r a b sp rp))))

;;; Set output for counterexmaple.
(define (greensyn-output dst rst mem t s r a b sp rp)
  (set! current-output (convert-progstate (progstate dst rst mem t s r a b sp rp))))

;;; Set send/recv storage for counterexmaple.
(define (greensyn-send-recv send-u sendp-u 
		  send-d sendp-d
		  send-l sendp-l
		  send-r sendp-r
		  recv-u recvp-u
		  recv-d recvp-d
		  recv-l recvp-l
		  recv-r recvp-r)
  (set! current-comm (convert-commstate (commstate send-u send-d send-l send-r recv-u recv-d recv-l recv-r sendp-u sendp-d sendp-l sendp-r recvp-u recvp-d recvp-l recvp-r))))

;;; Add previously set counterexample (input, output, and send/recv)
;;; to the list that will be generated as formula.
;;; If input, output, send/recv are set but not commit, 
;;; the counterexample won't be included in the generated formula
(define (greensyn-commit)
  (add-input-output current-input current-output current-comm))

;;; Generate Z3 file for synthesis from the list of counterexamples
(define (greensyn-check-sat #:file [file "prog.smt2"] prog-size)
  (parameterize ([current-output-port (open-output-file file #:exists 'replace)])
    (synthesize-prog prog-size)))

;;; Set spec for verification
(define (greensyn-spec string)
  (set! spec-count (compile string spec)))

;;; Generate Z3 file for verification from spec and a given candidate
(define (greensyn-verify file string)
  (set! cand-count (compile string cand))
  (parameterize ([current-output-port (open-output-file file #:exists 'replace)])
    (verify-prog)))
  
