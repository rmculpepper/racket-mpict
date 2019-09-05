#lang racket/base
(require slideshow/base
         pict
         ppict/align
         "private/tv.rkt"
         "mpict.rkt"
         "easing.rkt"
         plot/pict)

(define/unlifted (test-ease name e)
  ;; (slide (t (format "~a" name)))
  (slide/mpict
   (vc-append
    50
    (t (format "~a" name))
    (let ([DIM 400])
      (hc-append
       50
       ;;(plot (function e) #:x-min 0 #:x-max 1 #:x-label #f #:y-label #f)
       (// (rectangle DIM DIM)
           (cshadow
            (pin-over/align (rectangle DIM DIM)
                            (* DIM Time) (* DIM (- 1 (e Time))) 'c 'c
                            (colorize (disk 10) "red"))))
       (// (rectangle 10 DIM)
           (pin-over/align (rectangle 10 DIM)
                           5 (* DIM (- 1 (e Time))) 'c 'c
                           (colorize (disk 10) "red"))))))))

(test-ease "identity" values)

(test-ease "ease-in-sine" ease-in-sine)
(test-ease "ease-out-sine" ease-out-sine)
(test-ease "ease-in-out-sine" ease-in-out-sine)

(test-ease "ease-in-quad" ease-in-quad)
(test-ease "ease-out-quad" ease-out-quad)
(test-ease "ease-in-out-quad" ease-in-out-quad)

(test-ease "ease-in-cube" ease-in-cube)
(test-ease "ease-out-cube" ease-out-cube)
(test-ease "ease-in-out-cube" ease-in-out-cube)

(test-ease "in quadhop(0.5)" (lambda (u) (ease-in-quad/hop u 0.5)))
(test-ease "out quadhop(0.5)" (lambda (u) (ease-out-quad/hop u 0.5)))
(test-ease "inout quadhop(0.5)" (lambda (u) (ease-in-out-quad/hop u 0.5)))
