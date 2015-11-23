#lang racket
; Aufgabe 20
(define (euler-n n)
  (define (fak x)
    (define (fak-iter produkt zaehler n)
      (if (> zaehler n) produkt
          (fak-iter (* zaehler produkt) (+ zaehler 1) n)))
    (fak-iter 1 1 x))
  (define (summe f a b sum)
    (if (> a b) sum
        (summe f (+ a 1) b (+ sum (f a)))))
  (summe (lambda (x) (/ 1 (fak x))) 0 n 0))

; Aufgabe 21
(define (ackermann n m)
  (cond ((= n 0) (+ m 1))
        ((= m 0) (ackermann (- n 1) 1))
        (else (ackermann (- n 1) (ackermann n (- m 1))))))

; Aufgabe 22
(define (osterformel j)
  (let* ((a (remainder j 19))
         (b (remainder j 4))
         (c (remainder j 7))
         (k (floor (/ j 100)))
         (p (floor (/ (+ (* 8 k) 13) 25)))
         (q (floor (/ k 4)))
         (M (remainder (+ 15 k (- p) (- q)) 30))
         (N (remainder (+ 4 k (- q)) 7))
         (d (remainder (+ (* 19 a) M) 30))
         (e (remainder (+ (* 2 b) (* 4 c) (* 6 d) N) 7))
         (o (+ 22 d e)))
    o))

; Aufgabe 23
(define (maxziffer_alt n)
  (define (make-list n)
    (if (= n 0) '()
        (list* (remainder n 10) (make-list (quotient n 10)))))        
  (apply max (make-list n)))

(define (maxziffer n)
  (define (make-list n)
    (if (= n 0) '()
        (list* (remainder n 10) (make-list (quotient n 10)))))        
  (apply max (make-list (abs n))))

; Aufgabe 24
(define (sum x y)
  (define (n x)
    (+ x 1))
  (if (= y 0) x
      (n (sum x (- y 1)))))

; Aufgabe 25
(define (mul x y)
  (if (= y 0) 0
      (sum x (mul x (- y 1)))))

; Aufgabe 26
(define (q n)
  (if (or (= n 1) (= n 2)) 1
      (+ (q (- n (q (- n 1)))) (q (- n (q (- n 2)))))))
         
