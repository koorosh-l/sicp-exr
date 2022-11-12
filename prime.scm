;;#!/usr/bin/guile \
;;-s
;;!#
(define fast-prime?
  (lambda (a)
    (letrec ([iter (lambda (mem ul)
		     (cond
		      [(>= mem ul) #t]
		      [(zero? (remainder a mem)) #f]
		      [else;; (display mem) (newline) (newline) 
		       (iter (1+ mem)
			     (euclidean-quotient a mem))]))])
      (iter 2 a))))
(define fillter
  (lambda (predicate? combiner null-val term a next b)
    (define iter
      (lambda (a res)
	(cond
	 [(> a b) res]
	 [(not (predicate? (term a))) (iter  (next a) res)]
	 [else (iter (next a) (combiner res (term a)))])))
    (iter a null-val)))
(define (neg a) (* -1 a))
(define (solve a b)
  (fillter fast-prime?
	   (lambda (a b) (cons b a))
	   '()
	   (lambda (a) (neg a))
	   (neg b) 1+ (neg a)))
(define count (read))
(let main ([count count])
  (when (> count 0)
    	(for-each (lambda (a) (display a) (newline))
		  (solve (read) (read)))
	(newline)
	(main (1- count))))
