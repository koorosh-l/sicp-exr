(define-module (sicp)
  #:use-module (ice-9 futures)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-111)
  #:export (square
	    avarage
	    square-root gcd
	    dummy-exp
	    dummy-exp-iter
	    fast-exp
	    fast-exp-iter
	    fast-mul
	    mul-iter
	    fib-iter
	    sum-ints
	    sum-ints-iter
	    sum
	    sum-iter
	    para-fai))
(define square
  (lambda (x) (* x x)))
(define exer1.3
  (let ([sum-sq (lambda (a b) (+ (square a) (square b)))])
    (lambda (a b c)
      (cond
       [(< a b) (exer1.3 b a c)]
       [(< b c) (exer1.3 a c b)]
       [else (sum-sq a b)]))))
;;exp
(define-public (fact n)
  (letrec ([iter (lambda (n a)
		   (cond
		    [(<= n 1) a]
		    [(iter (1- n) (* a n))]))])
    (iter n 1)))
(define dummy-exp
  (lambda (b n)
    (if (zero? n) 1
	(* b (dummy-exp b (- n 1))))))
(define dummy-exp-iter
  (lambda (b n)
    (define iter
      (lambda (b counter product)
	(cond
	 [(zero? counter) product]
	 [else (iter b (- counter 1) (* b product))])))
    (iter b n 1)))
(define fast-exp
  (lambda (b n)
    (cond
     [(zero? n) 1]
     [(even? n) (fast-exp (* b b) (/ n 2))]
     [(* b (fast-exp b (- n 1)))])))
(define fast-exp-iter
  (lambda (b n)
    (define iter
      (lambda (b n product)
	(cond
	 [(= n 1) (* b product)]
	 [(even? n) (iter (* b b) (/ n 2) product)]
	 [else (iter b (- n 1) (* product b))])))
    (if (zero? n) 1
	(iter b n 1))))
;;8eee0846
;;1.18
(define mul-iter
  (letrec ([iter
	    (lambda (b counter product)
	      (cond
	       [(zero? counter) product]
	       [else (iter b (- counter 1) (+ b product))]))])
    (lambda (b n)
      (iter b n 0))))
(define double (lambda (x) (* 2 x)))
(define halve (lambda (x) (/ x 2)))
(define fast-mul
  (lambda (b n)
    (cond
     [(zero? n) 0]
     [(even? n) (fast-mul (+ b b) (/ n 2))]
     [(+ b (fast-mul b (- n 1)))])))
;;gcd
(define (gcd a b)
  (if (zero? b) a
      (gcd b (remainder a b))))
(define-public sum-ints
  (lambda (a b)
    (if (> a b)
	0
	(+ a (sum-ints (+ 1 a) b)))))
(define-public sum-ints-iter
  (lambda (a b)
    (define iter
      (lambda (a b sum)
	(cond
	 [(> a b) (touch sum)]
	 [else (iter (1+ a) b (future (+ sum a)))])))
    (iter a b (future 0))))
(define sum
  (lambda (term a next b)
    (if (> a b)
	0
	(+ (term a)
	   (sum term (next a) next b)))))
(define-public integral
  (lambda (f a b dx)
    (define add-dx (lambda (x) (+ x dx)))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)))
(define sum-iter
  (lambda (term a next b)
    (define iter
      (lambda (a res)
	(if (> a b) res
	    (iter (next a) (+ res (term a))))))
    (iter a 0)))
(define-public product
  (lambda (term a next b)
    (if (> a b) 1
	(* (term a) (product term (next a) next b)))))
(define-public product-iter
  (lambda (term a next b)
    (define iter
      (lambda (a res)
	(if (> a b) res
	    (iter (next a) (* res (term a))))))
    (iter a 1)))
(define-public (pi-pro a)
  (* 4
     (product-iter (lambda (x) (/ (* x (+ x 2.0)) (* (+ x 1.0) (+ x 1.0))))
		   2
		   (lambda (x) (+ x 2))
		   a)))
(define-public accumulate
  (lambda (combiner null-val term a next b)
    (if (> a b) null-val
	(combiner (term a) (accumulate combiner null-val term (next a) next b)))))
(define-public accumulate-iter
  (lambda (combiner null-val term a next b)
    (define iter
      (lambda (a res)
	(if (> a b) null-val
	    (iter (next a) (combiner (term a) res)))))
    (iter a null-val)))
(define-public filltered-accumulate
  (lambda (predicate? combiner null-val term a next b)
    (cond
     [(> a b) null-val]
     [(not (predicate? (term a))) (filltered-accumulate predicate? combiner
						      null-val term
						      (next a) next b)]
     [else (combiner (term a)
		     (filltered-accumulate predicate? combiner null-val
					   term (next a) next b))])))
(define-public prime-iter?
  (lambda (x)
    (define iter
      (lambda (x mem)
	(cond
	 [(<= x mem) #t]
	 [(zero? (remainder x mem)) #f]
	 [else (iter x (+ mem 1))])))
    (iter x 2)))
(define-public fast-prime?
  (lambda (a)
    (letrec ([iter (lambda (mem ul)
		     (cond
		      [(>= mem ul) #t]
		      [(zero? (remainder a mem)) #f]
		      [else;; (display mem) (newline) (newline) 
		       (iter (1+ mem)
			     (euclidean-quotient a mem))]))])
      (iter 2 a))))
(define my-remainder
  (lambda (a b)
    (letrec* ([a a]
	      [b b]
	      [iter
	       (lambda ()
		 (cond
		  [(zero? (- a b)) 0]
		  [else (set! a (+ a a))
			(iter)]))])
      (iter))))
(define (simple-fib n)
  "baby be a simple fib"
  (cond
   [(or (= n 1) (= n 0)) 1]
   [else (+ (fib (- n 1)) (fib (- n 2)))]))
;; (define (iter-fib n)
;;   (define (iter n fm1 fm2)
;;     (cond
;;      [(or (= n 1) (= n 0)) fm2]
;;      [else (iter (1- n)
;; 		 fm2
;; 		 (+ fm1 fm2))]))
;;   (iter n 1 1))
;; (define (the-best-fib n)
;;   "it's iterative logn BUT it doesn't work right now and I don't know how to fix it"
;;   (define (iter n fm1 fm2)
;;     (cond
;;      [(or (= n 1) (= n 0)) fm2]
;;      [else (iter (1- n)
;; 		 fm2
;; 		 (+ fm1 fm2))])))
;; (define fib the-best-fib)

;; (define-public fast-boxed-prime?
;;   (lambda (a)
;;     (letrec ([mem 2]
;; 	     [ul (box a)]
;; 	     [remainder (lambda (i j))]
;; 	     [euclidean-quotient (lambda (a b))]
;; 	     [iter (lambda ()
;; 		     (cond
;; 		      [(>= mem ul) #t]
;; 		      [(zero? (remainder a mem)) #f]
;; 		      [else (set! mem (1+ mem))
;; 			    (set-box! ul (euclidean-quotient a mem))
;; 			    (iter)]))])
;;       (iter))))
(define-public lame-fast-prime?
  (lambda (a)
    (letrec ([iter (lambda (mem)
		     (cond
		      [(>= mem (1+ (sqrt a))) #t]
		      [(zero? (remainder a mem)) #f]
		      [else (iter (1+ mem))]))])
      (iter 2))))
(define filltered-accumulate-iter
  (lambda (predicate? combiner null-val term a next b)
    (define iter
      (lambda (a res)
	(cond
	 [(> a b) res]
	 [(not (predicate? (term a))) (iter  (next a) res)]
	 [else (iter (next a) (combiner res (term a)))])))
    (iter a null-val)))
(define-public sicp-filter filltered-accumulate-iter)
(define (dist a b nproc)
  (define int (abs (floor (/ (- a b) nproc))))
  (let loop ([a a] [b b] [int int] [ans '()])
    (cond
     [(>= (+ a int -1) b) (cons (list a b) ans)]
     [else (loop (+ a int) b int
		 (cons (list a (+ a int -1)) ans))])))
(define-public (parallel-sicp-filter predicate? combiner null-val
				     term a next b nproc)
  (letrec* ([l (dist a b nproc)]
	    [f (lambda (a b) (sicp-filter predicate? combiner null-val term
				     a next b))]
	    [res (par-map (lambda (a) (apply f a)) l)])
    (apply combiner res)))
;; (parallel-sicp-filter (lambda (a) #t)
;; 		      + 0 identity 0 1+ 100 2)
(define-public (pi a)
  (* (sicp-filter (lambda (a) #t) + 0.0
		  (lambda (a)
		    (/ (if (odd? a) -1 1)
		       (+ a a 1)))
		  0 1+ a) 4))
(define-public (para-pi a)
  (* (parallel-sicp-filter (lambda (a) #t) + 0.0
			   (lambda (a)
			     (/ (if (odd? a) -1 1)
				(+ a a 1)))
			   0 1+ a 20) 4))
;; (define (gen-make-list- econs)
;;   (lambda a
;;     (cond
;;      [(null? (cdr a)) (car a)]
;;      [else ()])))
(define (mycons a b)
  (lambda (f)
    (f a b)))
(define (mycar p)
  (p (lambda (a b) a)))
(define (mycdr p)
  (p (lambda (a b) b)))
(define (mylist . a)
  (cond
   [(null? a) '()]
   [else (mycons (car a) (apply mylist (cdr a)))]))
(define (mylist-ref l k)
  (cond
   [(zero? k) (mycar l)]
   [else (mylist-ref (mycdr l) (1- k))]))
(define (numcons a b)
  (* (fast-exp-iter 2 a) (fast-exp-iter 3 b)))
(define (numcar p)
  (cond
   [(not (zero? (remainder p 2))) 0]
   [else (1+ (numcar (/ p 2)))]))
(define (numcdr p)
  (cond
   [(not (zero? (remainder p 3))) 0]
   [else (1+ (numcdr (/ p 3)))]))
(define (numlist . a)
  (cond
   [(null? (cdr a)) (car a)]
   [else (numcons (car a) (apply numlist (cdr a)))]))
(define (numlist-ref l k)
  (cond
   [(zero? k) (numcar l)]
   [else (numlist-ref (numcdr l) (1- k))]))
(define zero (lambda (f)
	       (lambda (x) x)))
(define (add1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
;; (define one (add1 zero))
;; n = zero
;; zero:
;; (lambda (f)
;;   (lambda (x) x))
;; (zero 1+):
;; (lambda (x) x)
;; one (add1 zero):
;; (lambda (f)
;;   (lambda (x)
;;     (f x)))

;; (one 1+):
;; (lambda (x)
;;   (1+ x))
;; ((one 1+) 0)
;; (lambda (0)
;;   (1+ 0))

;; two (add1 one)
;; n: (lambda (f)
;;      (lambda (x)
;;        (f x)))
(define fgc
  (lambda (n)
    (let loop ()
      (cond
       [(zero? n) (gc) n]
       [else (gc) (set! n (1- n)) (loop)]))))

(define (my-append l1 l2)
  (cond
   [(null? l1) l2]
   [else (cons (car l1) (append (cdr l1) l2))]))
(define (my-last-pair l)
  (cond
   [(null? (cdr l)) l]
   [else (last-pair (cdr l))]))
(define (my-reverse l)
  (cond
   [(null? l) l]
   [(null? (cdr l)) (list (car l))]
   [else (my-append (my-reverse (cdr l))
		    (list  (car l)))]))
(define (count-leaves x)
  (cond
   [(null? x) 0]
   [(not (pair? x)) 1]
   [else (+ (count-leaves (car x)) (count-leaves (cdr x)))]))
(define (deep-reverse x)
  (cond
   [(null? x) x]
   [(not (pair? x)) x]
   [else (my-append (deep-reverse (cdr x))
		    (list (deep-reverse (car x))))]))
(define (fringe x)
  (cond
   [(null? x) x]
   [(not (pair? x)) (list x)]
   [else (my-append (fringe (car x))
		    (fringe (cdr x)))]))
;;should be done sicp:168
;; (define (make-mobile l r)
;;   (list l r))
;; (define (make-branch len struct)
;;   (list len struct))
;; (define (left-branch m)
;;   (car m))
;; (define (right-branch m)
;;   (cadr m))
;; (define (branch-len b)
;;   (car b))
;; (define (branch-struct b)
;;   (cadr b))
(define (subsets s)
  (cond
   [(null? s) (list '())]
   [else (let ([rest (subsets (cdr s))])
	   (my-append rest
		      (map (lambda (a) (cons (car s) a)) rest)))]))
(define (tree-map x f)
  (cond
   [(null? x)     '()]
   [(not (pair? x)) (f x)]
   [else (cons (tree-map (car x) f)
	       (tree-map (cdr x) f))]))

(define (my-filter pred? l)
  (cond
   [(null? l) '()]
   [(pred? (car l)) (cons (car l) (my-filter pred? (cdr l)))]
   [else (my-filter pred? (cdr l))]))
(define (acc op init seq)
  (cond
   [(null? seq) init]
   [else (op (car seq)
	     (acc op init (cdr seq)))]))
(define (enumarate-interval l h)
  (if (< h l)
       '()
       (cons l (enumarate-interval (1+ l) h))))
(define (enumarate-tree tree)
  (cond
   [(null? tree) '()]
   [(not (pair? tree)) (list tree)]
   [else
    (my-append (enumarate-tree (car tree))
	       (enumarate-tree (cdr tree)))]))

(define (s-map p seq)
  (acc))
(define (s-append))

(define (memoize f)
  (let ([prv (list)]
	[res (list)])
    (lambda x
      (define q (assoc x prv))
      (cond
       [q (display "HIT\n") (cdr q)]
       [else (set! res (apply f x))
	     (set! prv (cons (cons x res) prv))
	     res]))))
