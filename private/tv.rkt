#lang racket/base
(require racket/stxparam (for-syntax racket/base syntax/parse))
(provide (except-out (all-defined-out) tv:app)
         (rename-out [tv:app #%app]))

;; A UReal is a Real in the range [0,1].

;; A TimedValue[X] is one of
;; - X
;; - (tv (UReal -> X) X X)
(struct tv (f v0 v1))

(define Time (tv values 0 1))
(define RTime (tv (lambda (u) (- 1 u)) 1 0))

;; TODO: easing functions!

;; ----------------------------------------

(define (-share v)
  (cond [(tv? v)
         (tv (let ([h (make-hasheqv)] [f (tv-f v)])
               (lambda (u) (hash-ref! h u (lambda () (f u)))))
             (tv-v0 v) (tv-v1 v))]
        [else v]))

;; get : UReal -> TimedValue[X] -> X
(define ((get u) arg) (if (tv? arg) ((tv-f arg) u) arg))

;; -get{0,1} : TimedValue[X] -> X
(define (-get0 arg) (if (tv? arg) (tv-v0 arg) arg))
(define (-get1 arg) (if (tv? arg) (tv-v1 arg) arg))

;; tvapply : (X ... -> Y) (List TimedValue[X] ...) -> TimedValue[Y]
(define (tvapply f args)
  (cond [(ormap tv? args)
         (tv (lambda (u) (apply f (map (get u) args)))
             (apply f (map -get0 args))
             (apply f (map -get1 args)))]
        [else (apply f args)]))

;; tvapp : (X ... -> Y) TimedValue[X] ... -> TimedValue[Y]
(define (tvapp1 f arg)
  (cond [(tv? arg)
         (let ([argf (tv-f arg)])
           (tv (lambda (u) (f (argf u)))
               (f (tv-v0 arg))
               (f (tv-v1 arg))))]
        [else (f arg)]))
(define (tvapp2 f arg1 arg2)
  (cond [(or (tv? arg1) (tv? arg2))
         (tv (lambda (u) (f ((get u) arg1) ((get u) arg2)))
             (f (-get0 arg1) (-get0 arg2))
             (f (-get1 arg1) (-get1 arg2)))]
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
    [(_ (f:id arg:id ...) . body)
     (with-syntax ([(unlifted-f) (generate-temporaries #'(f))])
       #'(begin (define (unlifted-f arg ...) . body)
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

(define/unlifted (get0 mp) (-get0 mp))
(define/unlifted (get1 mp) (-get1 mp))
(define/unlifted (share mp) (-share mp))
