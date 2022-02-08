#lang racket

(define (cube x)
  (* x x x))

;; (define (sum term a next b)
;;   (cond [(> a b)
;;          0]
;;        [else
;;         (+ (term a)
;;           (sum term (next a) next b))]))

(define (sum term a next b)
  (define (iter a result)
    (cond [(> a b)
           result]
         [else
          (iter (next a) (+ result (term a)))])
    )
  (iter a 0))

(define (product term a next b)
  (define (iter a result)
    ((cond [(> a b)
            result]
          [else
           (iter (next a) (* result (term a)))])))
  (iter a 0))

(define (filtered-accumulate combiner null-value term a b filter next)
  (define (iter a result)
    (cond [(> a b)
           result]
         [(filter a)
          (iter (next a) (combiner result (term a)))]
         [else
          (iter (next a) result)]))
  (iter a null-value))



(define (inc n)
  (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))



(define (find-root f neg-point pos-point)
  (let ((midpoint
         (average neg-point pos-point)))
    (if close-enough? neg-point pos-point)
    midpoint
    (let ([test-value (f midpoint)])
      (cond
        ([positive? test-value]
         (search (f neg-point midpoint)))

        ))

    (define (average x y)
      (/ (+ x y) 2))

    (define (close-enough? x y)
      (< (abs (- x y)) 0.001))








    ;; (filtered-accumulate + 0 identity 1 6 (lambda (x) (= (remainder x 2) 0)) inc)
