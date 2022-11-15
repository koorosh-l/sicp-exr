#!guile \
-s
!#
(define (fast-prime? a)
  (define (iter mem ul)
    (cond
     [(>= mem ul) #t]
     [(zero? (remainder a mem)) #f]
     [else ;; (display mem) (newline) (newline) 
      (iter (1+ mem)
	    (euclidean-quotient a mem))]))
  (iter 2 a))

(define (eliminate! i arr)
  (let loop ([m (+ i i)]
	     [ub (bitvector-length arr)])
    (cond
     [(>= m ub) #t]
     [else (bitvector-set! arr m #f)
	   (loop (+ m i) ub)])))

(define (sieve max)
  (define size max)
  (define arr (make-bitvector size #t))
  ;; (bitvector-set! arr 0 #f)
  ;; (bitvector-set! arr 1 #f)
  (let loop ([ub (1- size)]
	     [i 2])
    (cond
     [(>= i ub) arr]
     [else (eliminate! i arr)
	   (loop ub;;(1+ (quotient size (+ i 2)))
		 (1+ i))])))

(define (sieve-out->numlist arr)
  (let loop ([i 0] [size (bitvector-length arr)])
    (cond
     [(>= i size) '()]
     [(bitvector-ref arr i) (cons i (loop (1+ i) size))]
     [else (loop (1+ i) size)])))
(define (check arr)
  (let loop ([l (bitvector-length arr)]
	     [i 0])
    (cond
     [(>= i l) #t]
     [(and (bitvector-ref ) (not (fast-prime? )))]
     [else (loop l (1+ i))])))
