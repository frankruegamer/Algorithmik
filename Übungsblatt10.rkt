#lang racket
; Aufgabe 45
(define (removeFirstLast string)
  (list->string (drop-right (drop (string->list string) 1) 1)))

; Aufgabe 46
(define (sicheresPasswort passwort)
  (define (checkLength liste)
    (>= (length liste) 8))
  (define (checkSpecialchar liste)
    (define (isSpecialchar? char)
      (let ((asciicode (char->integer char)))
        (not (or (>= 65 asciicode 90) (>= 97 asciicode 122)))))
    (define (countSpecialchar liste)
      (if (null? liste) 0
          (+ (if (isSpecialchar? (car liste)) 1 0) (countSpecialchar (cdr liste)))))
    (>= (countSpecialchar liste) 2))
  (define (checkLowerUpper liste)
    (define (isLower? char)
      (<= 97 (char->integer char) 122))
    (define (isUpper? char)
      (<= 65 (char->integer char) 90))
    (define (countLowerUpper liste low up)
      (if (null? liste) (and (> low 0) (> up 0))
          (countLowerUpper (cdr liste) (+ low (if (isLower? (car liste)) 1 0)) (+ up (if (isUpper? (car liste)) 1 0)))))
    (countLowerUpper liste 0 0))
  (let ((passwortliste (string->list passwort)))
    (and (checkLength passwortliste) (checkSpecialchar passwortliste) (checkLowerUpper passwortliste))))

; Aufgabe 47
(define (isAnagramm anagram string)
  (define (normalize s)
    (define (trim liste)
      (if (or (null? liste) (not (equal? (car liste) #\space))) liste
          (trim (cdr liste))))
    (list->string (trim (sort (string->list (string-downcase s)) char<?))))
  (equal? (normalize anagram) (normalize string)))

; Aufgabe 48
(define (vektor-add . vektoren)
  (define (iteration n)
    (define (sum l)
      (if (null? l) 0
          (+ (list-ref (car l) n) (sum (cdr l)))))
    (if (> (+ n 1) (length (car vektoren))) empty        
        (cons (sum vektoren) (iteration (+ n 1)))))
  (iteration 0))
