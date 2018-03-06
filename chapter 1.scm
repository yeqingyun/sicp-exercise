;; 1.3
(define (lager a b c) (cond ((and (< a c) (< b c)) (if (< a b) (+ b c) (+ a c)))
					   ((and (< a b) (< c b)) (if (< a c) (+ b c) (+ b a)))
					   (else (if (< b c) (+ a c) (+ a b)))))

(define (bigger a b) (if (< a b) b a))
(define (smaller a b) (if (< a b) a b))

(define (lager a b c) (+ (bigger a b) (bigger (smaller a b) c)))

;; 1.4
;;该过程描述为 a + b 的绝对值

;; 1.5
;;如果是应用顺(test 0 (p))将进入无线的调用循环中，如果是正则序将正常调用，结果为0，因为正则序只在需要用时才求值(p)所在的参数

;; 1.6
;;结果解释器会报错，超过了递归的最大深度，因为new-if是普通函数，在应用序解释器中，if 和 else 都将执行，且因为new-if是普通函数，所以sqrt-iter不再是尾递归调用，解释器的优化也无法进行，所以导致无限的递归调用。

;; 1.7
(define (sqrt x) 
  (define (average a b) (/ (+ a b) 2))
  (define (good-enough? guess) (<= (abs (- (/ (* guess guess)  x) 1)) 0.01))
  (define (improve guess) (average (/ x guess) guess))
  (define (sqrt-iter guess) (if (good-enough? guess) guess (sqrt-iter (improve guess)))) 
  (sqrt-iter 1.0))

;; 1.8
(define (lqrt x)
  (define (lverage a b) (/ (+ a b) 3))
  (define (good-enough? guess) (<= (abs (- (/ (* guess guess guess) x) 1)) 0.01))
  (define (improve guess) (lverage (/ x (* guess guess)) (* 2 guess)))
  (define (lqrt-iter guess) (if (good-enough? guess) guess (lqrt-iter (improve guess))))
  (lqrt-iter 1.0))


;; 1.9

;; 1:
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc(+ 0 5)))))
;; (inc (inc (inc (inc(5)))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; 2:
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;; 1是线性递归过程 2是线性迭代过程


;; 1.10

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; ...
;; 2的10次方


;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; 2的16次方

;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; ...
;; 2的16次方

;; (define (f n) (A 0 n)) f(n) = 2n
;; (define (g n) (A 1 n)) f(n) = 2的n次方 
;; (define (h n) (A 2 n)) f(n) = 2的 (2的n次方)次方

;; 1.11

;;递归
(define (f n) 
  (if (< n 3)
	  n
	  (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))


;;迭代
(define (fd n) 
  (define (fd-iter a b c n) (if (= 0 n) a (fd-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))
(fd-iter 0 1 2 n))


;; 1.12
(define (psk x y) (if (< y x) 0 (if (or (= x 1) (= x y)) 1 (+ (psk (- x 1) (- y 1)) (psk x (- y 1))))))


;; 1.14


;; 1.16
(define (double x) (+ x x))
(define (valve x) (/ x 2))
(define (* a b)
(cond ((= a 1) b)
        ((= b 1) a)
        ((even? b) (* (double a) (valve b)))
        (else (+ a (* a (- b 1))))))

;; 1.17

(define (double x) (+ x x))
(define (valve x) (/ x 2))



(define (miter x y n)
    (cond ((or (= x 0) (= y 0)) n)
        ((even? y) (miter (double x) (valve y) n))
        (else (miter x (- y 1) (+ n x)))))

(define (* a b)
  (miter a b 0))




;;1.29

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

(define (simpson f a b n)
    
    (define h (/ (- b a) n))

    (define (y k)
        (f (+ a (* k h))))

    (define (factor k)
        (cond ((or (= k 0) (= k n))
                1)
              ((odd? k)
                4)
              (else
                2)))
    
    (define (term k)
        (* (factor k)
           (y k)))

    (define (next k)
        (+ k 1))

    (if (not (even? n))
        0
        (* (/ h 3)
           (sum term (exact->inexact 0) next n))))

;; 1.30
(define (sum term a next b)
  (define (iter a result) 
	(if (> a b) result
		(iter (next a) (+ (term a) result))))
  (iter a 0))


;; 1.31
(define (product term a next b)
  (if (> a b)
	  1
	  (* (term a) (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
	(if (> a b) result
		(iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial a b) 
  (define (even? x) ( = 0 (remainder x 2)))
  (define (term)
	(let ((n 1)) 
	  (lambda (a) (if (even? n)
					  (begin (set! n (+ 1 n)) (/ (+ a 1) a))
					  (begin (set! n (+ 1 n)) (/ (- a 1) a))))))
  (define (next k) (+ 2 k))
  (product (term) a next b))


;; 1.34
(define (f g) (g 2))


;; 1.35
(define tolerance 0.00001)

(define (fix-point f first-guess) 
  (define (close-enough? v1 v2) 
	(> tolerance (abs (- v1 v2))))
  (define (try guess) 
	(let ((next (f guess))) 
	(if (close-enough? guess next)
		next
		(try next))))
  (try first-guess))

;;(fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0) 1.618...

;; 1.36
(define (fix-point f first-guess)
  (define (close-enough? v1 v2)
	(> tolerance (abs (- v1 v2))))
  (define (try guess step)
	(display (format "step: ~a, guess: ~a" step guess))
	(newline)
	(let ((next (f guess))) 
	(if (close-enough? guess next)
		next
		(try next (+ 1 step)))))
  (try first-guess 1))

(define (average-damp f)
    (lambda (x)
	  (/ (+ x (f x)) 2)))
;;(fix-point (lambda (x) (/ (log 1000 10)(log x 10))) 2.0)
;;(fix-point (average-damp (lambda (x) (/ (log 1000 10)(log x 10)))) 2.0)


;; 1.37
(define (cont-frac n d k)
  (define (iter i)
	(if (= i k)
		(/ (n k) (d k))
		(/ (n i) (+ (d i) (iter (+ 1 i))))))
  (iter 1))

(define (cont-frac n d k) 
  (define (iter i result)
	(if (= i 0)
		result
		(iter (- i 1) 
			  (/ (n i) (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))


;; 1.38

(define (e k)
    (define (N i) 1)
    (define (D i)
        (if (= 0 (remainder (+ i 1) 3))
            (* 2 (/ (+ i 1) 3))
            1))
(+ 2.0 (cont-frac N D k)))

;; 1.39

(define (tan-cf x k)
    (define (N i)
        (if (= i 1)
            x
            (- (square x))))
    (define (D i)
        (- (* i 2) 1))
    (exact->inexact (cont-frac N D k)))





