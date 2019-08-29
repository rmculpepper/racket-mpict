#lang racket/base
(require racket/math)
(provide (all-defined-out))

;; Easing functions

(define (ease-out f u)
  (- 1 (f (- 1 u))))
(define (ease-in-out f u)
  (if (< u 1/2)
      (* 1/2 (f (* 2 u)))
      (- 1 (* 1/2 (f (* 2 (- 1 u)))))))

(define (ease-in-sine u) (sin (* u (/ pi 2))))
(define (ease-out-sine u) (ease-out ease-in-sine u))
(define (ease-in-out-sine u)
  (* 1/2 (+ 1 (sin (* (- u 0.5) pi)))))

(define (ease-in-quad u) (* u u))
(define (ease-out-quad u) (ease-out ease-in-quad u))
(define (ease-in-out-quad u) (ease-in-out ease-in-quad u))

(define (ease-in-cube u) (* u u u))
(define (ease-out-cube u) (ease-out ease-in-cube u))
(define (ease-in-out-cube u) (ease-in-out ease-in-cube u))

(define (ease-in-poly n u) (expt u n))
(define (ease-out-poly n u) (ease-out (lambda (u) (expt u n))))
(define (ease-in-out-poly n u) (ease-in-out (lambda (u) (expt u n))))

;; ----

(define (ease-in-quad/hop u [q 1])
  ;; f(x) = ax^2 + bx + c, where f(0)=0, f(?)=-q, f(1)=1
  ;; ==> a = 2q+2\sqrt{q\left(q+1\right)}+1
  (define a (+ (* 2 q) (* 2 (sqrt (* q (+ q 1)))) 1))
  (+ (* a u u) (* (- 1 a) u)))

(define (ease-out-quad/hop u [q 1]) (ease-out (lambda (u) (ease-in-quad/hop u q)) u))
(define (ease-in-out-quad/hop u [q 1]) (ease-in-out (lambda (u) (ease-in-quad/hop u q)) u))
