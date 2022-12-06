#!/home/koorosh/.guix-profile/bin/guile \
-s
!#
(define bitvector-ref bitvector-bit-set?)
(define (bitvector-set! vec idx val)
  (if (not val)
      (bitvector-clear-bit! vec idx)
      (bitvector-set-bit! vec idx)))
(define (fast-prime? a)
  (define (iter mem ul)
    (cond
     [(>= mem ul) #t]
     [(zero? (remainder a mem)) #f]
     [else (iter (1+ mem)
		 (euclidean-quotient a mem))]))
  (iter 2 a))
(define max (string->number (list-ref (command-line) 1)))
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
	   (loop (1+ (quotient size (+ i 2)))
		 (1+ i))])))
(define (sieve-out->numlist arr)
  (let loop ([i 0] [size (bitvector-length arr)])
    (cond
     [(>= i size) '()]
     [(bitvector-ref arr i) (cons i (loop (1+ i) size))]
     [else (loop (1+ i) size)])))
(define (bitvector-index-for-each f bv)
  (let loop ([i 0] [l (bitvector-length bv)])
    (when (not (>= i l))
      (f i (bitvector-ref bv i))
      (loop (1+ i) l))))
(bitvector-index-for-each (lambda (i v) (when v (display i) (display "\n")))
			  (sieve max))
