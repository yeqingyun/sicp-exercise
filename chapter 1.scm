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
