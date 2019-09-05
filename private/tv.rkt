#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/transformer)
         racket/match
         racket/list
         racket/stxparam)
(provide (except-out (all-defined-out) tv:app)
         (rename-out [tv:app #%app]))

;; Idea: instead of escaping via define/unlifted, add frame annotations (cf Remora)?
;; Are there inner vs outer frame constraints?

;; fadeout : Smooth -> _
;; get0 : * -> _
;; // : * * -> _
;; share : Smooth -> _
;; cshadow : ???
;; autoinset : * -> *   --- auto determines biggest bounding box (assumes Smooth max is at endpoint)

;; What would I even *expect* from (fadeout (// A B)), though?

;; A UReal is a Real in the range [0,1].
;; A NNReal is a Real >= 0.

;; ============================================================

;; An AnimatedValue[X] is one of
;; - X                              -- constant
;; - (av NNReal (UReal -> X) X X)   -- animated for duration, then constant
(struct av (dur f v0 v1))

(define FTime (av 1 values 0 1))
(define RTime (av 1 (lambda (u) (- 1 u)) 1 0))

(define current-Time (make-parameter FTime))
(define-syntax Time (make-variable-like-transformer #'(current-Time)))

(define-syntax-parameter agravity (quote-syntax 'left)) ;; one of 'left, 'right, 'stretch

(define (-dur v)
  (match v
    [(? dv?) (error '-dur "expected smooth value, got: ~e" v)]
    [(av dur _ _ _) dur]
    [_ 0]))

;; avapply : (X ... -> Y) (List AnimatedValue[X] ...) -> AnimatedValue[Y]
(define (avapply f args)
  (cond [(ormap av? args)
         (define dur* (apply max (map -dur args)))
         (define args* (map (lambda (a) (-timeclip dur* a)) args))
         (av dur*
             (lambda (u)
               (parameterize ((current-Time u))
                 (apply f (for/list ([arg (in-list args*)]) (-get u arg)))))
             (apply f (map -getA args*))
             (apply f (map -getZ args*)))]
        [else (apply f args)]))

;; avapp{1,2,N} : (X ... -> Y) TimedValue[X] ... -> TimedValue[Y]
(define (avapp1 f arg)
  (match arg
    [(av dur argf v0 v1)
     (av dur (lambda (u) (f (argf u))) (f v0) (f v1))]
    [v (f v)]))
(define (avapp2 f arg1 arg2)
  (cond [(or (av? arg1) (av? arg2))
         (define dur* (max (-dur arg1) (-dur arg2)))
         (define arg1* (-timeclip dur* arg1))
         (define arg2* (-timeclip dur* arg2))
         (av (lambda (u) (f (-get u arg1*) (-get u arg2*)))
             (f (-getA arg1*) (-getA arg2*))
             (f (-getZ arg1*) (-getZ arg2*)))]
        [else (f arg1 arg2)]))
(define (avappN f . args)
  (avapply f args))

(define (-timescale s v)
  (match v
    [(? dv?) (error 'timescale "expected animated value, got: ~e" v)]
    [(av dur f v0 v1) (av (* dur s) f v0 v1)]
    [_ v]))

(define (-timeclip dur* v [gravity 'left])
  (match v
    [(? dv?) (error 'timeclip "expected animated value, got: ~e" v)]
    [(av dur f v0 v1)
     (define scale (/ dur* dur))
     (if (= dur* dur)
         v
         (case gravity
           [(left)
            (cond [(< dur* dur)
                   (define (f* u) (f (* u scale)))
                   (av dur* f* v0 (f* 1))]
                  [(> dur* dur)
                   (define (f* u) (f (min 1 (* u scale))))
                   (av dur* f* v0 v1)])]
           [(right)
            (cond [(< dur* dur)
                   (define (f* u) (f (+ 1 (- scale) (* u scale))))
                   (av dur* f* (f* 0) v1)]
                  [(> dur* dur)
                   (define (f* u) (f (max 0 (- (* u scale) scale -1))))
                   (av dur* f* v0 v1)])]
           [(stretch)
            (av dur* f v0 v1)]))]
    [_ v]))

;; -get : UReal AnimatedValue[X] -> X
(define (-get u arg)
  (match arg
    [(av dur f _ _) (f u)]
    [v v]))

;; ============================================================

;; A TimedValue[X] is one of
;; - AnimatedValue[X]
;; - (dv (NonemptyListof TimedValue[X]))   -- "discrete" time steps
(struct dv (steps))

;; Note: This allows trees of discrete-timed animations, as opposed to
;; dv containing (NEVectorof AnimatedValue[X])!

(define (dv-length v)
  (match v [(dv steps) (length steps)] [_ 1]))
(define (dv-ref v k)
  (match v
    [(dv steps)
     (if (< k (length steps)) (list-ref steps k) (-getZ (last steps)))]
    [v (if (zero? k) v (-getZ v))]))

(define ->>
  (case-lambda
    [(v)      (>>* 1 v)]
    [(n v)    (>>* n v)]
    [(n v0 v) (>>* n v v0)]))
(define (>>* n v [v0 (-getA v)])
  (match v
    [(dv steps) (dv (append (make-list n v0) steps))]
    [_ (dv (append (make-list n v0) (list v)))]))

(define (-// . vs)
  (dv vs))
(define (-++ . vs)
  (dv (apply append (map (lambda (a) (if (dv? a) (dv-steps a) (list a))) vs))))

;; tvapply : (X ... -> Y) (List TimedValue[X] ...) -> TimedValue[Y]
(define (tvapply f args)
  (cond [(ormap dv? args)
         (define len* (apply max (map dv-length args)))
         (dv (for/list ([i (in-range len*)])
               (tvapply f (map (lambda (a) (dv-ref a i)) args))))]
        [else (avapply f args)]))

;; tvapp{1,2,N} : (X ... -> Y) TimedValue[X] ... -> TimedValue[Y]
(define (tvapp1 f arg)
  (match arg
    [(dv steps)
     (dv (for/list ([step (in-list steps)])
           (tvapp1 f step)))]
    [else (avapp1 f arg)]))
(define (tvapp2 f arg1 arg2)
  (cond [(or (dv? arg1) (dv? arg2))
         (define len* (max (dv-length arg1) (dv-length arg2)))
         (dv (for/list ([i (in-range len*)])
               (tvapp2 f (dv-ref arg1 i) (dv-ref arg2 i))))]
        [else (avapp2 f arg1 arg2)]))
(define (tvappN f . args)
  (tvapply f args))

(define (const? x) (not (or (av? x) (dv? x))))
(define (smooth? x) (not (dv? x)))

;; ============================================================

;; -share : TimedValue[X] -> TimedValue[X]
;; Useful when eq?-ness is important.
(define (-share v)
  (match v
    [(dv steps)
     (dv (map -share steps))]
    [(av dur f v0 v1)
     (define h (make-hasheqv))
     (define (f* u) (hash-ref! h u (lambda () (f u))))
     (av dur f* v0 v1)]
    [_ v]))

;; -get{A,Z} : TimedValue[X] -> X
(define (-getA arg)
  (match arg
    [(dv (cons step1 _)) (-getA step1)]
    [(av _ _ v0 _) v0]
    [v v]))
(define (-getZ arg)
  (match arg
    [(dv steps) (-getZ (last steps))]
    [(av _ _ _ v1) v1]
    [v v]))

(define (-cumulative v fps)
  (match v
    [(av dur f v0 v1)
     ;; An Index is a Nat ranging from 0 to (ceiling (* dur fps))
     (define h (make-hasheqv)) ;; Nat -> (Listof X)
     (for/fold ([acc null]) ([i (in-range 0 (add1 (ceiling (* dur fps))))])
       (define u (min 1 (/ i dur fps)))
       (define acc* (cons (f u) acc))
       (hash-set! h i acc*)
       acc*)
     (define (f* u)
       (define i (inexact->exact (floor (* u dur fps))))
       (hash-ref h i))
     (av dur f* (f* 0) (f* 1))]
    [v (list v)]))

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
    ;;[(_ f:expr arg:expr) #'(#%plain-app tvapp1 f arg)]
    ;;[(_ f:expr arg1:expr arg2:expr) #'(#%plain-app tvapp2 f arg1 arg2)]
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

(define-syntax get (unlifted-transformer #'-get))
(define-syntax getA (unlifted-transformer #'-getA))
(define-syntax getZ (unlifted-transformer #'-getZ))
(define-syntax share (unlifted-transformer #'-share))
(define-syntax // (unlifted-transformer #'-//))
(define-syntax ++ (unlifted-transformer #'-++))
(define-syntax >> (unlifted-transformer #'->>))

(define (if0 v0 velse) (av 1 (lambda (u) (if (= u 0) v0 velse)) v0 velse))
(define (if1 v1 velse) (av 1 (lambda (u) (if (= u 1) v1 velse)) velse v1))

(define-syntax-rule (let/lift ([x rhs] ...) . body)
  (tvapp (lambda (x ...) . body) rhs ...))

(define (stepfun vs)
  (let ([vs (list->vector vs)] [n (length vs)])
    (av 1
        (lambda (u) (vector-ref vs (min n (inexact->exact (floor (* n u))))))
        (vector-ref vs 0)
        (vector-ref vs (sub1 n)))))

(define-syntax timescale (unlifted-transformer #'-timescale))
(define-syntax timeclip  (unlifted-transformer #'-timeclip))
(define-syntax cumulative  (unlifted-transformer #'-cumulative))
