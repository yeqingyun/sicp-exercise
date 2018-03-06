;; 2.1
(define (make-rat n d)
  (if (> d 0)
	  (cons n d)
	  (cons (- n) (- d))))

;; 2.2
(define (make-segment startPoint endPoint)
  (cons startPoint endPoint))
(define (start-segment segment)
  (car segment))
(define (end-segment segment) 
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point) 
  (car point))
(define (y-point point) 
  (cdr point))
(define (midpoint-segment segment) 
  (let ((startPoint (start-segment segment))
		(endPoint (end-segment segment))) 
	(make-point (/ (- (x-point startPoint) (x-point endPoint)) 2)
				(/ (- (y-point startPoint) (y-point endPoint)) 2))))

;;(midpoint-segment (make-segment (make-point 1 2) (make-point 7 8)))


;; 2.3

(define (make-rectangle height width)
  (cons height width))
(define (rectangle-height rectangle)
  (car rectangle))
(define (rectangle-width rectangle)
  (cdr rectangle))
(define (width-of-rectangle rectangle) 
  (let ((w (rectangle-width rectangle))) 
	(abs (- (x-point (end-point w)) (x-point (start-point w))))))
(define (height-of-rectangle rectangle) 
  (let ((h (rectangle-height rectangle))) 
	(abs (- (y-point (end-point h)) (y-point (start-point h))))))
(define (rectangle-perimeter rectangle)
  (* 2 (+ (height-of-rectangle rectangle) (width-of-rectangle rectangle))))
(define (rectangle-area rectangle)
  (* (height-of-rectangle rectangle) (width-of-rectangle rectangle)))



;; (define (cons a b)
;;   (define (dispath m)
;; 	(cond ((= 0 m) a)
;; 		  ((= 1 m) b)
;; 		  (else (error "arg not 0 or 1" m))))
;;   dispath)
;; (define (car z)
;;   (z 0))
;; (define (cdr z)
;;   (z 1))


;; 2.4

;; (define (cdr z)
;;   (z (lambda (p q) q)))

;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) (x y))

;; 2.5

;; (define (cons a b)
;;   (* (expt 2 a) (expt 3 b)))

;; (define (car z)
;;   (if (= (remainder z 2) 0)
;; 	  (+ 1 (car (/ z 2)))
;; 	  0))
;; (define (cdr z)
;;   (if (= (remainder z 3) 0)
;; 	  (+ 1 (cdr (/ z 3)))
;; 	  0))

;; 2.6

;; (add-1 zero)

;; (add-1 (lambda (f)
;; 		 (lambda (x)
;; 		   x)))

;; ((lambda (n)               ; add-1
;;    (lambda (f)
;; 	 (lambda (x)
;; 	   (f ((n f) x)))))
;;  (lambda (f)              ; zero
;;    (lambda (x)
;; 	 x)))

;; (lambda (f)
;;   (lambda (x)
;; 	(f (
;; 		((lambda (f)        ; zero
;; 		   (lambda (x)
;; 			 x))
;; 		 f)
;; 		x))))
  
;; (lambda (f)
;;   (lambda (x)
;; 	(f ((lambda (x) x)
;; 		x))))

;; (lambda (f)
;;   (lambda (x)
;; 	(f x)))

(define one
    (lambda (f)
        (lambda (x)
            (f x))))



(define (for-each proc tab) 
  (define (iter f b) 
	(if (not (null? b))
		(begin
		  (f (car b))
		  (iter f (cdr b)))))
  (iter proc tab))

(define (filter f tab)
  (define (iter p t r)
	(if (null? t)
		(reverse r)
		(if (p (car t))
			(iter p (cdr t) (cons (car t) r))
			(iter p (cdr t) r)
			)))
  (iter f tab '()))

(define (deep-reverse t)
  (define (iter t r)
	(cond ((null? t) r)
		  ((pair? (car t)) (iter (cdr t) (cons (deep-reverse (car t)) r)))
		  (else (iter (cdr t) (cons (car t) r)))))
  (iter t '()))

(define (square-tree tree)
  (define (iter t)
	(cond ((null? t) '())
		  ((not (pair? t)) (* t t))
		  (else (cons (iter (car t))
					  (iter (cdr t))))))
  (iter tree))

(define (square-tree tree)
  (map (lambda (x)
		 (if (pair? x)
			 (square-tree x)
			 (* x x)))
	   tree))


(define (square-tree tree)
  (define (square x)
	(* x x))
  (define (tree-map f t)
	  (cond ((null? t) '())
		  ((not (pair? (car t))) (cons (f (car t)) (tree-map f (cdr t))))
		  (else (cons (tree-map f (car t)) (tree-map f (cdr t))))))
  (tree-map square tree))
