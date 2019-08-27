#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/match
         racket/stxparam)
(provide (except-out (all-defined-out) tv:app)
         (rename-out [tv:app #%app]))

;; A UReal is a Real in the range [0,1].

;; A SmoothValue[X] is one of
;; - X
;; - (tv (UReal -> X) X X)
(struct tv (f v0 v1))

;; A TimedValue[X] is one of
;; - SmoothValue[X]
;; - (dv TimedValue[X] TimedValue[X])   -- "discrete" time steps
(struct dv (fst snd))

(define Time (tv values 0 1))
(define RTime (tv (lambda (u) (- 1 u)) 1 0))

;; TODO: easing functions!

(define (const? x) (not (or (tv? x) (dv? x))))
(define (smooth? x) (not (dv? x)))

(define (dv* left right)
  (if (eqv? left right) left (dv left right)))

(define (dv-left v)
  (match v
    [(dv v1 v2) v1]
    [(? tv? v) v]
    [v v]))
(define (dv-right v)
  (match v
    [(dv v1 v2) v2]
    [(tv f v0 v1) v1]
    [v v]))

;; ----------------------------------------

(define (-share v)
  (match v
    [(dv v1 v2)
     (let ([v1* (-share v1)] [v2* (-share v2)])
       (if (and (eq? v1* v1) (eq? v2* v2)) v (dv v1* v2*)))]
    [(tv f v0 v1)
     (tv (let ([h (make-hasheqv)])
           (lambda (u) (hash-ref! h u (lambda () (f u)))))
         v0 v1)]
    [_ v]))

;; get : UReal -> SmoothValue[X] -> X
(define ((get u) arg)
  (match arg
    [(tv f _ _) (f u)]
    [v v]))

;; -get{A,Z} : TimedValue[X] -> X
(define (-getA arg)
  (match arg
    [(dv v1 _) (-getA v1)]
    [(tv _ v0 _) v0]
    [v v]))
(define (-getZ arg)
  (match arg
    [(dv _ v2) (-getZ v2)]
    [(tv _ _ v1) v1]
    [v v]))

;; tvapply : (X ... -> Y) (List TimedValue[X] ...) -> TimedValue[Y]
(define (tvapply f args)
  (cond [(ormap dv? args)
         (dv (tvapply f (map dv-left args))
             (tvapply f (map dv-right args)))]
        [(ormap tv? args)
         (tv (lambda (u) (apply f (map (get u) args)))
             (apply f (map -getA args))
             (apply f (map -getZ args)))]
        [else (apply f args)]))

;; tvapp{1,2,N} : (X ... -> Y) TimedValue[X] ... -> TimedValue[Y]
(define (tvapp1 f arg)
  (match arg
    [(dv v1 v2)
     (dv* (tvapp1 f v1) (tvapp1 f v2))]
    [(tv argf v0 v1)
     (tv (lambda (u) (f (argf u)))
         (f v0) (f v1))]
    [v (f v)]))
(define (tvapp2 f arg1 arg2)
  (cond [(or (dv? arg1) (dv? arg2))
         (dv* (tvapp f (dv-left arg1) (dv-left arg2))
              (tvapp f (dv-right arg1) (dv-right arg2)))]
        [(or (tv? arg1) (tv? arg2))
         (tv (lambda (u) (f ((get u) arg1) ((get u) arg2)))
             (f (-getA arg1) (-getA arg2))
             (f (-getZ arg1) (-getZ arg2)))]
        [else (f arg1 arg2)]))
(define (tvappN f . args)
  (tvapply f args))

;; ----------------------------------------

(begin-for-syntax
  (define (unlifted-transformer unlifted)
    (syntax-parser
      [(_ arg ...)
       (with-syntax ([unlifted unlifted])
         (syntax/loc this-syntax (unlifted arg ...)))]
      [_:id
       (raise-syntax-error #f "unlifted function not in operator position" this-syntax)])))

(define-syntax define/unlifted
  (syntax-parser
    [(_ (f:id . formals) . body)
     (with-syntax ([(unlifted-f) (generate-temporaries #'(f))])
       #'(begin (define (unlifted-f . formals) . body)
                (define-syntax f (unlifted-transformer (quote-syntax unlifted-f)))))]))

(define-syntax tvapp
  (syntax-parser
    ;; functions declared with define/unlifted are macros, don't go through tvapp
    [(_ f:expr) #'(#%plain-app f)]
    [(_ f:expr arg:expr) #'(#%plain-app tvapp1 f arg)]
    [(_ f:expr arg1:expr arg2:expr) #'(#%plain-app tvapp2 f arg1 arg2)]
    [(_ f:expr arg:expr ...) #'(#%plain-app tvappN f arg ...)]
    [(_ f:expr (~alt parg:expr (~seq kw:keyword kwarg:expr)) ...)
     (with-syntax ([(ptmp ...) (generate-temporaries #'(parg ...))]
                   [(kwtmp ...) (generate-temporaries #'(kwarg ...))])
       #'(tvapp (lambda (ptmp ... kwtmp ...) (f ptmp ... (~@ kw kwtmp) ...))
                parg ... kwarg ...))]))

(define-syntax-parameter tv-lifting? #t)
(define-syntax tv:app
  (syntax-parser
    [(_ x ...)
     (if (syntax-parameter-value #'tv-lifting?)
         (syntax/loc this-syntax (tvapp x ...))
         (syntax/loc this-syntax (#%app x ...)))]))

(define-syntax-rule (begin/lifted . body)
  (syntax-parameterize ((tv-lifting? #t)) . body))

(define-syntax-rule (begin/unlifted . body)
  (syntax-parameterize ((tv-lifting? #f)) . body))

(define-syntax getA (unlifted-transformer #'-getA))
(define-syntax getZ (unlifted-transformer #'-getZ))
(define-syntax share (unlifted-transformer #'-share))
(define-syntax // (unlifted-transformer #'dv))
(define-syntax S (unlifted-transformer #'-S))

(define (if0 v0 velse) (tv (lambda (u) (if (= u 0) v0 velse)) v0 velse))
(define (if1 v1 velse) (tv (lambda (u) (if (= u 1) v1 velse)) velse v1))

(define-syntax-rule (let/lift ([x rhs] ...) . body)
  (tvapp (lambda (x ...) . body) rhs ...))
