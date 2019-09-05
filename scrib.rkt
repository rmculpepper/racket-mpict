#lang at-exp racket/base
(require racket/match
         racket/list
         (prefix-in s: scribble/core)
         pict)

(define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))

(define (get-base-size) 24)
(define (get-para-width) 700)
(define (get-line-sep) 4)

;; ------------------------------------------------------------
;; Styles

;; A Style is (style (U String Symbol #f) (Listof StyleProperty))
;; An ElementStyle is (U String Symbol #f Style).

;; Some element styles:
;; - 'tt, 'italic, 'bold, 'roman, 'sf, 'url, 'subscript, 'superscript, 'smaller, 'larger
;; - 'hspace -- renders its content as monospace blanks
;; Some style properties:
;; - (color-property (U String (list Byte Byte Byte)))
;; - (background-color-property (U String (list Byte Byte Byte)))

;; A PictTextStyle (PTStyle) is one of (see pict `text` style argument):
;; - null or 'default
;; - font%
;; - 'roman, ...?               -- font family
;; - String                     -- font face, eg "Helvetica", ...
;; - (cons String Symbol)       -- font face and fallback family
;; - (cons PTStyleSymbol PTStyle)
;;   where PTStyleSymbol is one of the following:
;;     'bold 'italic 'subscript 'superscript 'large-script 'caps
;;     'combine 'no-combine 'aligned 'unaligned)
;; - (cons 'italic PTStyle)
;; - (cons Color PTStyle)

;; An IStyle is
;; (hasheq 'base (U 'default font% (U 'roman ...) String)
;;         'mods (Listof PTStyleSymbol)
;;         ['color Color] ['bgcolor Color]
;;         ... other effects ...)

(define (add-style s istyle)
  (match s
    [(s:style name props)
     (for/fold ([istyle (add-style name istyle)])
               ([prop (in-list props)])
       (add-style-prop prop istyle))]
    [(? string?)
     (add-simple-style s istyle)]
    [(? symbol?)
     (add-simple-style s istyle)]
    [_
     (when #f (eprintf "add-style: warning, ignoring: ~e" s))
     istyle]))

(define (add-simple-style s istyle)
  (case s
    [(italic bold subscript superscript #||# combine no-combine aligned unaligned)
     (hash-cons istyle 'mods s)]
    [(tt roman sf)
     (hash-set istyle 'base s)]
    [else
     (when #f (eprintf "add-simple-style: warning, ignoring: ~e" s))
     istyle]))

(define (add-style-prop prop istyle)
  (match prop
    [(s:color-property color)
     (hash-set istyle 'color (to-color color))]
    [(s:background-color-property color)
     (hash-set istyle 'bgcolor (to-color color))]
    [_
     (when #f (eprintf "add-style-prop: warning, ignoreing: ~e" prop))
     istyle]))

(define (to-color color) color) ;; FIXME

(define base-istyle '#hasheq((base . default) (scale . 1)))

;; ------------------------------------------------------------

;; A Paragraph is (paragraph Style Content)
;; An Element is (element Style Content)

(define (paragraph->pict p [istyle base-istyle])
  (match p
    [(s:paragraph style content)
     (content->pict content (add-style style istyle))]))

(define (element->pict e [istyle base-istyle])
  (match e
    ;; ???
    [(s:element style content)
     (content->pict content (add-style style istyle))]))

;; A Content is one of
;; - String
;; - Symbol in mdash ndash ldquo lsquo rdquo rsquo larr rarr prime
;; - convertible? to 'text or ???
;; - Element
;; - (Listof Content)
(define (content->pict content istyle #:width [width (get-para-width)])
  (define fragments (content->fragments content istyle))
  (define lines (linebreak-fragments fragments width))
  (apply vl-append
         (get-line-sep)
         (for/list ([line (in-list lines)])
           (apply hbl-append 0 line))))

;; A Fragment is either Pict or (cons String IStyle), where the string
;; either contains no whitespace or only whitespace.
;; FIXME: Start with laxer invariant: fragment never starts or ends
;; with whitespace; then only break if necessary.

;; content->fragments : Content IStyle -> (Listof Fragment)
(define (content->fragments content istyle)
  (match content
    [(? string?)
     (for/list ([seg (in-list (string->segments (regexp-replace* "\n" content " ")))])
       (cons seg istyle))]
    ;; [(? symbol?) (list (cons (content-symbol->string content) istyle))] ;; FIXME
    [(? pict?) (list content)]
    [(s:element style content)
     (content->fragments content (add-style style istyle))]
    [(? list?)
     (apply append
            (for/list ([part (in-list content)])
              (content->fragments part istyle)))]
    [_ (error 'content->fragments "bad content: ~e" content)]))

(define (whitespace-fragment? frag)
  (and (pair? frag) (regexp-match? #px"^\\s*$" (car frag))))

(define (fragment->pict fragment)
  (match fragment
    [(? pict?) fragment]
    [(cons str istyle)
     (define ptstyle (append (hash-ref istyle 'mods null) (hash-ref istyle 'base)))
     (define size (* (hash-ref istyle 'scale) (get-base-size)))
     (text str ptstyle size)]))

;; linebreak-fragments : (Listof fragments) PositiveReal -> (Listof (Listof Pict))
(define (linebreak-fragments fragments width)
  (define (loop frags) ;; -> (Listof (Listof Pict))
    (cond [(null? frags) null]
          [else (let-values ([(line rest-frags) (lineloop frags null 0)])
                  (cons line (loop rest-frags)))]))
  (define (lineloop frags racc accw) ;; -> (Listof Pict) (Listof Fragments)
    (define (return-line [frags frags] [racc racc])
      (define line (reverse (dropf racc whitespace-fragment?)))
      (values line frags))
    (match frags
      ['() (return-line)]
      [(cons frag1 frags2)
       (define p1 (fragment->pict frag1))
       (define w1 (pict-width p1))
       (cond [(<= (+ accw w1) width)
              (lineloop frags2 (cons p1 racc) (+ accw w1))]
             #|
             [(break-fragment frag1)
              => (lambda (frags1) (lineloop (append frags1 frags2) racc raccw))]
             |#
             [(zero? accw) ;; overflows, but already on its own line
              (return-line frags2 (cons p1 racc))]
             [else (return-line)])]))
  (loop fragments))

(define (string->segments s)
  ;; A Segment is a String that contains either all whitespace or no whitespace chars.
  (define ws-zones (regexp-match-positions* #px"\\s+" s))
  (let loop ([start 0] [ws-zones ws-zones])
    (cond [(null? ws-zones)
           (if (< start (string-length s)) (list (substring s start)) null)]
          [(< start (caar ws-zones))
           (cons (substring s start (caar ws-zones)) (loop (caar ws-zones) ws-zones))]
          [else
           (cons (substring s (caar ws-zones) (cdar ws-zones))
                 (loop (cdar ws-zones) (cdr ws-zones)))])))
