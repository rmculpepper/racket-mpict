#lang at-exp racket/base
(require "private/tv.rkt"
         racket/match
         pict
         ppict/align ppict/tag
         slideshow/code
         slideshow/base)
(provide (all-defined-out))

;; An MPict is TimedValue[Pict]
(define FPS 60)

(define/unlifted (slide/mpict mp)
  (begin/unlifted
    (match mp
      [(dv steps)
       (for ([step (in-list steps)])
         (slide/mpict step))]
      [(av dur f v0 v1)
       (for ([u (in-range 0 1 (/ (* dur FPS)))])
         (slide #:timeout (/ FPS) (get u mp)))
       (slide v1)]
      [_ (slide mp)])))

;; ------------------------------------------------------------

(define/unlifted (fadeout p)
  (let ([p0 (values #;share (ghost (getA #;values p)))])
    (refocus (cc-superimpose p0 (scale (cellophane p RTime) (add1 (* 2 Time)))) p0)))

(define/unlifted (fadein p)
  (let ([p1 (ghost (getZ #;values p))])
    (refocus (cc-superimpose p1 (scale (cellophane p Time) (add1 (* 2 RTime)))) p1)))

(define/unlifted (fadeswap p1 p2)
  (lbl-superimpose (fadeout p1) (fadein p2)))

(define/unlifted (hop p)
  (let ([p0 (values (ghost (getA p)))])
    (pin-over/align p0 0 (* (pict-height p0) (hop-quad Time)) 'l 't p)))

(define (hop-quad u)
  (* 4 u (sub1 u)))

(define/unlifted (hop-in p [w 100])
  (let ([p0 (launder (ghost (getA p)))])
    (pin-over/align p0 (* RTime w) (* (pict-height p0) (hop-quad Time)) 'l 't p)))

;; ----

(define (call f x) (f x))

(define (interpolate u a b) (+ a (* u (- b a))))

(require "easing.rkt")
(define/unlifted (fly p tp1 tp2)
  (let/lift ([u Time] [p p] [tp1 tp1] [tp2 tp2])
    (lambda (base)
      (define-values (x1 y1) (cc-find base (find-tag base tp1)))
      (define-values (x2 y2) (cc-find base (find-tag base tp2)))
      (cond [#t ;;(< 0 u 1)
             (pin-over/align base (interpolate u x1 x2) (interpolate (ease-in-quad/hop u) y1 y2)
                             'c 'c p)]
            [else base]))))

(define (fly* from to ps [q 1])
  (let/lift ([u Time])
    (lambda (base)
      (for/fold ([base base]) ([p (in-list ps)])
        (define-values (x1 y1) (cc-find base (list from p)))
        (define-values (x2 y2) (cc-find base (list to p)))
        (pin-over/align base (interpolate u x1 x2) (interpolate (ease-in-quad/hop u q) y1 y2)
                        'c 'c p)))))

(define (add-animations base . anims)
  (for/fold ([base base]) ([anim (in-list anims)])
    (call anim base)))

(define (add-animation base anim)
  (call anim base))

#;
(define/unlifted (cshadow mp)
  (begin/unlifted
    (match-define (av dur f v0 v1) mp)
    (define (f* u)
      (for/fold ([base (ghost v0)])
                ([uu (in-range 0 (+ u (/ FPS)) (/ FPS))] [k (in-range (- 1 u) 1 (/ FPS))])
        (define k* (max k 0.25))
        (let ([p (f uu)]) (refocus (cc-superimpose base (cellophane p k*)) p))))
    (av dur f* v0 (f* 1))))

(define/unlifted (cshadow mp)
  (cshadow* (cumulative mp FPS)))

(define (cshadow* ps)
  (for/fold ([base (ghost (car ps))])
            ([p (in-list ps)] [opacity (in-range 1 0 (/ -1 FPS))])
    (pin-over base 0 0 (cellophane p (max 0.2 opacity)))))

;; ----

(module+ main
(slide (t "ready?"))

#;
(slide/mpict
 (vc-append 20
            (cellophane (t "hello world") Time)
            (t "constant")))

(slide/mpict
 (hc-append 20
            (pin-over (blank 200 800)
                      0 (* 700 Time)
                      (colorize (bt "falling!") "darkred"))
            (pin-over (blank 200 800)
                      0 (* 700 RTime)
                      (colorize (bt "rising!") "blue"))))

#;
(slide/mpict
 (vc-append 20
            (para "This is" (fadeout (colorize (bt "not bad.") "darkred")))
            (para "This is" (fadein (colorize (bt "pretty cool!") "blue")))))

(slide/mpict
 (para "This is"
       (fadeswap (colorize (bt "not bad") "darkred")
                 (colorize (bt "pretty cool") "blue"))
       ", isn't it?"))

#;
(slide/mpict
 (// (fadein (fadeout (t "whoa")))
     (fadein (t "whoa"))))

(let ([idfun (code (lambda (x) x))])
  (slide/mpict
   (add-animations
    (vl-append
     20
     (para (code (define id #,(tag-pict idfun 'idfun1))))
     (para (tag-pict (ghost idfun) 'idfun2) "is the identity function"))
    (fly idfun 'idfun1 'idfun2))))

(let ([var1 (code x)] [var2 (code y)] [rhs1 (code (+ 1 2))] [rhs2 (code (* 3 4))])
  (define body (code (/ x y)))
  (define code1 (code (let ([#,var1 #,rhs1] [#,var2 #,rhs2]) #,body)))
  (define code2 (code ((lambda (#,var1 #,var2) #,body) #,rhs1 #,rhs2)))
  (slide/mpict
   (add-animations
    (vl-append
     20
     (para code1)
     (blank 200)
     (para code2))
    (fly* code1 code2 (list var1 var2 rhs1 rhs2 body) 0.5))))

(require pict/face)
#;
(slide/mpict
 (fadeout
  (scale
   (face (stepfun '(happy sortof-happy sortof-unhappy #;unhappy unhappier unhappiest surprised)))
   3/4)))
(slide/mpict
 (timescale 2
            (fadeout
             (scale
              (face (stepfun '(happy sortof-happy sortof-unhappy unhappier unhappiest surprised)))
              3/4))))

#|
(let ([idfun (code (lambda (x) x))])
  (slide/mpict
   (timescale 4
   (add-animations
    (vl-append
     20
     (para (code (define id #,(tag-pict idfun 'idfun1))))
     (para (tag-pict (ghost idfun) 'idfun2) "is the identity function"))
    (fly (fadein idfun) 'idfun1 'idfun2)))))

(let ([idfun (code (lambda (x) x))])
  (slide/mpict
   (add-animations
    (vl-append
     20
     (para (code (define id #,(tag-pict idfun 'idfun1))))
     (para (tag-pict (ghost idfun) 'idfun2) "is the identity function"))
    (fly (timescale 2 (fadein idfun)) 'idfun1 'idfun2))))
|#

(slide/mpict
 (hc-append 20
            (timeclip 2 (hop (t "hello")) 'left)
            (timeclip 2 (hop (t "world")) 'right)
            (timeclip 2 (hop (t "whee!")) 'stretch)
            (>> 1 (hop (fadein (t "whoa!"))))))

(require "scrib.rkt" (prefix-in s: scribble/base))
(slide/mpict
 (>>
 @flow-pict[#:style 'roman]{
   This whole slide consists of a @s:italic{flow}. It consists of
   multiple @s:tt{paragraphs} and @s:elem[#:style 'sf]{other such stuff}.

   This is a @s:italic{paragraph}. It is written using @s:italic{Scribble's
   at-exp reader}, which means that when I use @code[para] and
   @code[it] and picts, I do not have to break things manually, like
   @hop-in[@fadein[@code[(para "This" (it "is") "a para")]]]; I can write them more naturally.

   This @hop-in[@fadein[@code[λ]]] is good stuff:
   @s:itemlist[
   @s:item{it is @s:italic{functional}}
   @s:item{it is @s:italic{higher-order}}
   ]
   }))
)
