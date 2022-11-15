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

(define max (read))
(define-inlinable (index->num i) (+ i 2))
(define-inlinable (number->index n) (- n 2))

(define (eliminate! i arr)
  (let loop ([m (index->num i)] [l (bitvector-length arr)])
    ;;(display i) (newline);; (display m) (newline) 
    (cond
     [(>= m l) #t]
     [else (bitvector-set! arr m #f)
	   (loop (+ m i) l)])))
(define (sive max)
  (define size (1- max))
  (define arr (make-bitvector size #t))
  (let loop ([ub (1- size)]
	     [i 0])
    (cond
     [(>= i ub) arr]
     [else (eliminate! i arr)
	   (loop (1+ (quotient size (+ i 2)))
		 (1+ i))])))

(let loop ([i 0])
  (cond
   [(= i size) #t]
   [else (when (bitvector-ref arr i)
	   (display (+ i 2))
	   (newline))
	 (loop (1+ i))]))
(define (check arr)
  (let loop ([l (bitvector-length arr)]
	     [i 0])
    (cond
     [(>= i l) #t]
     [(and (bitvector-ref ) (not (fast-prime? )))]
     [else (loop l (1+ i))])))
