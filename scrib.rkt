#lang at-exp racket/base
(require racket/match
         racket/list
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/html-properties)
         (prefix-in s: scribble/latex-properties)
         (prefix-in s: scribble/decode)
         pict)
(provide flow-pict)

(define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))

(define (get-base-size) 32)   ;; (current-font-size) = 32
(define (get-para-width) 800) ;; (current-para-width) = 738
(define (get-line-sep) 5)     ;; (current-line-sep) = 5
(define (get-block-sep) 24)   ;; (current-gap-size) = 24

;; (current-main-font) = 'swiss
;; (current-title-color) = "black"
;; (current-code-font) = (bold . modern)

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
    [#f istyle]
    [_
     (when #t (eprintf "add-style: warning, ignoring: ~e\n" s))
     istyle]))

(define (add-simple-style s istyle)
  (case s
    [(italic bold subscript superscript #||# combine no-combine aligned unaligned)
     (hash-cons istyle 'mods s)]
    [(tt) (hash-set istyle 'base 'modern)]
    [(sf) (hash-set istyle 'base 'swiss)]
    [(roman) (hash-set istyle 'base s)]
    [(larger) (hash-set istyle 'scale (* 3/2 (hash-ref istyle 'scale 1)))]
    [(smaller) (hash-set istyle 'scale (* 2/3 (hash-ref istyle 'scale 1)))]
    [("RktInBG") (hash-set istyle 'bgcolor "lightgray")]
    [("RktIn" "RktPn" "RktSym") (hash-set istyle 'base 'modern)]
    [(hspace) (hash-set istyle 'base 'modern)]
    [else
     (when #t (eprintf "add-simple-style: warning, ignoring: ~e\n" s))
     istyle]))

(define (add-style-prop prop istyle)
  (match prop
    [(s:color-property color)
     (hash-set istyle 'color (to-color color))]
    [(s:background-color-property color)
     (hash-set istyle 'bgcolor (to-color color))]
    [(? s:css-addition?) istyle]
    [(? s:tex-addition?) istyle]
    [(== 'tt-chars) istyle]
    [_
     (when #t (eprintf "add-style-prop: warning, ignoring: ~e\n" prop))
     istyle]))

(define (to-color color) color) ;; FIXME

(define base-istyle '#hasheq((base . default) (scale . 1)))

;; ------------------------------------------------------------

;; A Flow is (Listof Block).
;; A Block is one of
;; - (itemization Style (Listof Flow))
;; - (paragraph Style Content)
;; - ... some other things ...

(define (flow->pict blocks [istyle base-istyle])
  (apply vl-append (get-block-sep)
         (for/list ([block (in-list blocks)])
           (block->pict block istyle))))

(define (block->pict block [istyle base-istyle])
  (match block
    [(s:paragraph style content)
     (content->pict content (add-style style istyle))]
    [(s:compound-paragraph style blocks)
     (apply vl-append (get-line-sep)
            (for/list ([block (in-list blocks)])
              (block->pict block istyle)))]
    [(s:nested-flow style flow)
     (flow->pict flow (add-style style istyle))]
    [(s:itemization style flows)
     ;; FIXME
     (apply vl-append (get-line-sep) ;; ??
            (for/list ([flow (in-list flows)])
              (htl-append 10 (get-bullet) (flow->pict flow istyle))))]))

(define (get-bullet)
  ;;(text "∘" '(bold) (get-base-size))
  (arrowhead (* 2/3 (get-base-size)) 0))


;; An Element is (element Style Content)

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
  (apply vl-append (get-line-sep)
         (for/list ([line (in-list lines)]
                    [index (in-naturals 1)])
           (line->pict line width istyle (= index (length lines))))))

(define (line->pict line width istyle last?)
  (apply hbl-append 0 line))

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
    [(? symbol?) (list (cons (content-symbol->string content) istyle))] ;; FIXME
    [(? pict?) (list content)]
    [(s:element style content)
     (content->fragments content (add-style style istyle))]
    [(s:delayed-element _ _ plain)
     (content->fragments (plain) istyle)]
    [(? list?)
     (apply append
            (for/list ([part (in-list content)])
              (content->fragments part istyle)))]
    [_ (error 'content->fragments "bad content: ~e" content)]))

(define (content-symbol->string sym)
  (case sym
    [(lsquo) "‘"] [(rsquo) "’"]
    [(ldquo) "“"] [(rdquo) "”"]
    [(mdash) "—"] [(ndash) "–"]
    [(prime) "′"]
    [else (error 'content-symbol->string "unknown symbol: ~e" sym)]))

(define (whitespace-fragment? frag)
  (and (pair? frag) (regexp-match? #px"^\\s*$" (car frag))))

(define (fragment->pict fragment)
  (match fragment
    [(? pict?) fragment]
    [(cons str istyle)
     (define ptstyle (append (hash-ref istyle 'mods null) (hash-ref istyle 'base)))
     (define size (* (hash-ref istyle 'scale) (get-base-size)))
     (let* ([p (text str ptstyle size)]
            [p (cond [(hash-ref istyle 'color #f)
                      => (lambda (c) (colorize p c))]
                     [else p])]
            [p (cond [(hash-ref istyle 'bgcolor #f)
                      => (lambda (c) (bg-colorize p c))]
                     [else p])])
       p)]))

(define (bg-colorize p c)
  (pin-under p 0 0 (filled-rectangle (pict-width p) (pict-height p) #:draw-border? #f #:color c)))

;; linebreak-fragments : (Listof fragments) PositiveReal -> (Listof (Listof Pict))
(define (linebreak-fragments fragments width)
  (define (loop frags) ;; -> (Listof (Listof Pict))
    (cond [(null? frags) null]
          [else (let*-values ([(frags*) (dropf frags whitespace-fragment?)]
                              [(line rest-frags) (lineloop frags* null 0)])
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

;; ============================================================

(require racket/pretty)
(define (flow-pict #:style [style #f] . pre-flow)
  (define flow (s:decode-flow pre-flow))
  (pretty-print flow)
  (flow->pict flow (add-style style base-istyle)))

(module+ main
  (require slideshow slideshow/code
           (only-in scribble/core color-property background-color-property)
           (only-in scribble/base elem italic itemlist [tt s:tt] [item s:item])
           (only-in scribble/manual litchar racketblock)
           (for-label racket/base))

  (define (blue . content)
    (apply elem #:style (s:style #f (list (color-property "blue"))) content))

  (define (on-pink . content)
    (apply elem #:style (s:style #f (list (background-color-property "pink"))) content))

  (slide
   @flow-pict[#:style 'roman]{
     This whole slide consists of a @italic{flow}. I consists of
     multiple @s:tt{paragraphs} and @elem[#:style 'sf]{other such stuff}.

     This is a @italic{paragraph}. It is written using @blue{Scribble's
     @litchar["@"]-exp reader}, which means that when I use @code[para] and
     @code[it] and picts, @on-pink{I do not have to break things @elem[#:style 'larger]{manually}}, like
     @code[(para "This" (it "is") "a para")]; I can write them @on-pink{more naturally}.

     This @code[λ] is good stuff:
     @itemlist[
     @s:item{it is @italic{functional@elem[#:style 'superscript]{ish}}}
     @s:item{it is @italic{higher-order}@elem[#:style 'subscript]{for sure}}
     ]

     @; -- Needs table!
     @;{
     @racketblock[
     (define (map f xs)
       (if (pair? xs) (cons (f (car xs)) (map f (cdr xs))) null))
     ]
     }
     }))
