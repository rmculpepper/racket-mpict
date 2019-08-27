#lang racket/base
(require "private/tv.rkt"
         pict
         ppict/align ppict/tag
         slideshow/code
         slideshow/base)

;; An MPict is TimedValue[Pict]
(define FPS 60)

(define/unlifted (slide/mpict mp)
  (begin/unlifted
    (cond [(dv? mp)
           (slide/mpict (dv-left mp))
           (slide/mpict (dv-right mp))]
          [(tv? mp)
           (for ([u (in-range 0 1 (/ FPS))])
             (slide #:timeout (/ FPS) ((get u) mp)))
           (slide (getZ mp))]
          [else (slide mp)])))

;; ------------------------------------------------------------

(define/unlifted (fadeout p)
  (let ([p0 (values #;share (ghost (getA #;values p)))])
    (refocus (cc-superimpose p0 (scale (cellophane p RTime) (add1 (* 2 Time)))) p0)))

(define/unlifted (fadein p)
  (let ([p1 (values #;share (ghost (getZ #;values p)))])
    (refocus (cc-superimpose p1 (scale (cellophane p Time) (add1 (* 2 RTime)))) p1)))

(define/unlifted (fadeswap p1 p2)
  (lbl-superimpose (fadeout p1) (fadein p2)))

;; ----

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

(slide/mpict
 (vc-append 20
            (para "This is" (fadeout (colorize (bt "not bad.") "darkred")))
            (para "This is" (fadein (colorize (bt "pretty cool!") "blue")))))

(slide/mpict
 (vc-append 20
            (para "This is"
                  (fadeswap (colorize (bt "not bad") "darkred")
                            (colorize (bt "pretty cool") "blue"))
                  ", isn't it?")))

(slide/mpict
 (// (fadein (fadeout (t "whoa")))
     (fadein (t "whoa"))))

;; ----

(define (call f x) (f x))

#|
f(x) = ax^2 + bx + c
(0,0) (?, -0.2) (1,1)
----
f(0) = 0 ==> c = 0
f(1) = 1 ==> a + b = 1 ==> b = 1-a
f(x) = ax^2 + (1-a)x
f'(x) = 2ax + 1 - a = a(2x - 1) + 1
f'(m) = 0 ==> 2am + 1 - a = 0 ==> a = -1/(2m-1)
f'(m) = 0 ==> 2am + 1 - a = 0 ==> m = (a-1)/(2a)
f(m) = -0.2 ==> am^2 + (1-a)m = -0.2
            ==> a(a-1)^2/(2a)^2 + (1-a)(a-1)/(2a) = -0.2
            ==> (7+2*sqrt(6))/5
|#
(define (e:quadhop u [q 1])
  ;; 0.2 => (define a (/ (+ 7 (* 2 (sqrt 6))) 5))
  ;; 2q+2\sqrt{q\left(q+1\right)}+1
  (define a (+ (* 2 q) (* 2 (sqrt (* q (+ q 1)))) 1))
  (+ (* a u u) (* (- 1 a) u)))

(define (interpolate u a b) (+ a (* u (- b a))))

#;
(define (fly p tp1 tp2)
  (define ((do-fly u) base)
    (define-values (x1 y1) (cc-find base (find-tag base tp1)))
    (define-values (x2 y2) (cc-find base (find-tag base tp2)))
    (cond [#t ;;(< 0 u 1)
           (pin-over/align base (interpolate u x1 x2) (interpolate (e:quadhop u) y1 y2)
                           'c 'c p)]
          [else base]))
  (do-fly Time))

(define (fly p tp1 tp2)
  (let/lift ([u Time])
    (lambda (base)
      (define-values (x1 y1) (cc-find base (find-tag base tp1)))
      (define-values (x2 y2) (cc-find base (find-tag base tp2)))
      (cond [#t ;;(< 0 u 1)
             (pin-over/align base (interpolate u x1 x2) (interpolate (e:quadhop u) y1 y2)
                             'c 'c p)]
            [else base]))))

(define (add-animations base . anims)
  (for/fold ([base base]) ([anim (in-list anims)])
    (call anim base)))

(define (add-animation base anim)
  (call anim base))

(define idfun (code (lambda (x) x)))

#;
(slide/mpict
 (add-animations
  (vl-append
   20
   (para (code (define id #,(tag-pict idfun 'idfun1))))
   (// (para (code))
       (para (tag-pict idfun 'idfun2) "is the identity function")))
  (// values
      (fly idfun 'idfun1 'idfun2))))

(slide/mpict
 (add-animations
  (vl-append
   20
   (para (code (define id #,(tag-pict idfun 'idfun1))))
   (para (tag-pict (if1 idfun (ghost idfun)) 'idfun2) "is the identity function"))
  (fly idfun 'idfun1 'idfun2)))
