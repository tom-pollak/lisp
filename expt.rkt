#lang racket

(define (square x)
  (* x x))

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (expt b n)
  (cond ((= n 0) 1)
       ((even? n)
        (square (expt b (/ n 2))))
       (else
        (* b (expt b (- n 1))))))

(define (expt-iter a b n)
  (cond ((= n 0) a)
       ((even? n)
        (expt-iter a (square b) (/ n 2)))
       (else
        (expt-iter (* a b) b (- n 1)))))

(define (multiply a b r)
  (cond ((= b 0) r)
       ((even? b)
        (multiply (double a) (halve b) r))
       (else
        (multiply a (- b 1) (+ r a)))))

;; (expt 4 3) ; Should be 16
;; (expt-iter 1 4 3) ; 48


;; 4 4 1 = 256
;; 16 2 2 = 64
;; 256
