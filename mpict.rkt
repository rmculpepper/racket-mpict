#lang racket/base
(require "private/tv.rkt"
         pict
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
