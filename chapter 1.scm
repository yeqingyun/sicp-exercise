;; 1.3
(define (lager a b c) (cond ((and (< a c) (< b c)) (if (< a b) (+ b c) (+ a c)))
					   ((and (< a b) (< c b)) (if (< a c) (+ b c) (+ b a)))
					   (else (if (< b c) (+ a c) (+ a b)))))

(define (bigger a b) (if (< a b) b a))
(define (smaller a b) (if (< a b) a b))

(define (lager a b c) (+ (bigger a b) (bigger (smaller a b) c)))

;; 1.4
;;�ù�������Ϊ a + b �ľ���ֵ

;; 1.5
;;�����Ӧ��˳(test 0 (p))���������ߵĵ���ѭ���У�������������������ã����Ϊ0����Ϊ������ֻ����Ҫ��ʱ����ֵ(p)���ڵĲ���

;; 1.6
;;����������ᱨ�������˵ݹ�������ȣ���Ϊnew-if����ͨ��������Ӧ����������У�if �� else ����ִ�У�����Ϊnew-if����ͨ����������sqrt-iter������β�ݹ���ã����������Ż�Ҳ�޷����У����Ե������޵ĵݹ���á�

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

;; 1�����Եݹ���� 2�����Ե�������


;; 1.10

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; ...
;; 2��10�η�


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
;; 2��16�η�

;; (A 3 3)
;; (A 2 (A 3 2))
;; (A 2 (A 2 (A 3 1)))
;; (A 2 (A 2 2))
;; ...
;; 2��16�η�

;; (define (f n) (A 0 n)) f(n) = 2n
;; (define (g n) (A 1 n)) f(n) = 2��n�η� 
;; (define (h n) (A 2 n)) f(n) = 2�� (2��n�η�)�η�

;; 1.11

;;�ݹ�
(define (f n) 
  (if (< n 3)
	  n
	  (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))


;;����
(define (fd n) 
  (define (fd-iter a b c n) (if (= 0 n) a (fd-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))
(fd-iter 0 1 2 n))


;; 1.12
(define (psk x y) (if (< y x) 0 (if (or (= x 1) (= x y)) 1 (+ (psk (- x 1) (- y 1)) (psk x (- y 1))))))


;; 1.14
