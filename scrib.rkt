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
(define (hash-remove* h ks) (for/fold ([h h]) ([k (in-list ks)]) (hash-remove h k)))

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
;;         ['width PosReal] ['inset-to-width? Boolean]
;;         ['keep-whitespace? Boolean]
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
    [("RktIn") (hash-set* istyle 'base 'modern 'color '(#xCC #x66 #x33))]
    [("RktPn") (hash-set* istyle 'base 'modern 'color '(#x84 #x3C #x24))]
    [("RktSym") (hash-set* istyle 'base 'modern 'color '(#x00 #x00 #x80))] ;; ???
    [("RktVar") (hash-set* (hash-cons istyle 'mods 'italic)
                           'base 'modern 'color '(#x40 #x40 #x40))]
    [("RktRes") (hash-set* istyle 'base 'modern 'color '(#x00 #x00 #xAF))]
    [("RktOut") (hash-set* istyle 'base 'modern 'color '(#x96 #x00 #x96))]
    [("RktCmt") (hash-set* istyle 'base 'modern 'color '(#xC2 #x74 #x1F))]
    [("RktVal") (hash-set* istyle 'base 'modern 'color '(#x22 #x8B #x22))]
    [(hspace) (hash-set* istyle 'base 'modern 'keep-whitespace? #t)]
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
    [(or 'omitable 'never-indents) istyle]
    [_
     (when #t (eprintf "add-style-prop: warning, ignoring: ~e\n" prop))
     istyle]))

(define (to-color color) color) ;; FIXME

(define base-istyle '#hasheq((base . default) (scale . 1) (inset-to-width? . #t)))

;; ------------------------------------------------------------

;; block style keys: 'bgcolor, 'inset-to-width?, 'width
;; ... but not all need to be removed

(define (remove-block-styles istyle)
  (let* ([h (hash-remove istyle 'bgcolor)])
    h))

(define (apply-block-styles istyle p)
  (let* ([p (cond [(hash-ref istyle 'bgcolor #f)
                   => (lambda (c) (bg-colorize p c))]
                  [else p])]
         [p (cond [(hash-ref istyle 'inset-to-width? #f)
                   (inset p 0 0 (- (hash-ref istyle 'width) (pict-width p)) 0)]
                  [else p])])
    p))

;; ============================================================

;; A Flow is (Listof Block).
;; A Block is one of
;; - (itemization Style (Listof Flow))
;; - (paragraph Style Content)
;; - ... some other things ...

(define (flow->pict blocks istyle)
  (apply vl-append (get-block-sep)
         (for/list ([block (in-list blocks)])
           (block->pict block istyle))))

(define (block->pict block istyle)
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
     (apply vl-append (get-line-sep) ;; ??
            (for/list ([flow (in-list flows)])
              (htl-append 10 (get-bullet) (flow->pict flow istyle))))]
    [(s:table style blockss)
     ;; FIXME: handle inset-to-width?, other block styles
     (table->pict blockss (add-table-style style istyle))]))

;; FIXME: fold into add-style? just need to keep remove-table-styles separate
(define (add-table-style style istyle)
  (match style
    [(s:style name props)
     (foldl add-table-style-prop (add-table-style name istyle) props)]
    ;; 'boxed, 'centered, 'block -- see `table` docs
    [_ istyle]))
(define (add-table-style-prop prop istyle)
  (match prop
    [(s:table-cells styless)
     (hash-set istyle 'table-cells styless)]
    [(s:table-columns styles)
     (hash-set istyle 'table-cols styles)]
    [_
     (when #t (eprintf "add-table-style-prop: ignoring ~e\n" prop))
     istyle]))
(define (remove-table-styles istyle)
  (hash-remove* istyle '(table-cells table-cols)))

(define (table->pict cellss istyle)
  (define nrows (length cellss))
  (define ncols (length (car cellss)))
  (define col-styles (or (hash-ref istyle 'table-cols #f) (make-list ncols #f)))
  (define cell-styless (or (hash-ref istyle 'table-cells #f)
                           (make-list nrows (make-list ncols #f))))
  (define cell-istyle
    (hash-set* (remove-table-styles istyle) 'inset-to-width? #f 'width +inf.0))
  (define rendered-cellss ;; (Listof (Listof (U (cons Pict IStyle) #f)))
    (for/list ([cells (in-list cellss)]
               [cell-styles (in-list cell-styless)])
      (for/list ([cell (in-list cells)]
                 [cell-style (in-list cell-styles)]
                 [col-style (in-list col-styles)])
        (cond [(eq? cell 'cont) #f]
              [else (render-table-cell cell cell-style col-style cell-istyle)]))))
  (define col-widths
    #|
    (let ([widthv (make-vector ncols #f)]
          [cellm  (make-vector (* ncols nrows) #f)])
      (define (get-index i j) (+ (* ncols i) j))
      (define (mref i j) (vector-ref cellm (get-index i j)))
      (define (mset! i j v) (vector-set! cellm (get-index i j) v))
      ;; initialize matrix
      (for ([rendered-cells (in-list rendered-cells)] [i (in-naturals)])
        (for ([rendered-cell (in-list rendered-cell) ] [j (in-naturals)])
          (mset! i j (if rendered-cell (pict-width (car rendered-cell)) #f))))
      ;;
      (for ([j (in-range ncols)])
    |#
    (let ([columns (transpose rendered-cellss)])
      (for/fold ([rcolwidths null] [leftovers (make-list nrows 0)]
                 #:result (reverse rcolwidths))
                ([col (in-list columns)]
                 [next-col (in-list (cdr (append columns (list (make-list nrows #t)))))])
        (define eff-cell-widths
          (for/list ([cell (in-list col)] [leftover leftovers])
            (+ leftover (if cell (pict-width (car cell)) 0))))
        (define col-width
          (apply max 0 (for/list ([eff-cell-width (in-list eff-cell-widths)]
                                  [next-cell (in-list next-col)]
                                  #:when next-cell)
                         eff-cell-width)))
        (define next-leftovers
          (for/list ([eff-cell-width (in-list eff-cell-widths)])
            (max 0 (- eff-cell-width col-width))))
        (values (cons col-width rcolwidths) next-leftovers))))
  (define (row->pict rendered-cells)
    (for/fold ([acc null] [extra-width 0]
               #:result (apply hbl-append 0 acc)) ;; FIXME: valign????
              ([cell (in-list (reverse rendered-cells))]
               [width (in-list (reverse col-widths))])
      (cond [(not cell)
             (values acc (+ width extra-width))]
            [else
             (define cell-pict (table-cell->pict (car cell) (cdr cell) (+ width extra-width)))
             (values (cons cell-pict acc) 0)])))
  (apply vl-append 0 (map row->pict rendered-cellss)))

(define (transpose xss)
  (cond [(andmap pair? xss) (cons (map car xss) (transpose (map cdr xss)))]
        [else null]))

(define (add-table-cell-style style istyle)
  (match style
    [(s:style name props)
     (foldl add-table-cell-style-prop (add-table-cell-style name istyle) props)]
    [_ istyle]))
(define (add-table-cell-style-prop prop istyle)
  (match prop
    [(or 'left 'right 'center)
     (hash-set istyle 'cell-halign prop)]
    [(or 'top 'bottom 'vcenter)
     (hash-set istyle 'cell-valign prop)]
    ['border ;; FIXME: left-border, etc
     (hash-set 'istyle 'cell-border #t)]
    [(s:background-color-property color)
     (hash-set 'istyle 'cell-bgcolor (to-color color))]
    [_ istyle]))
(define (remove-table-cell-styles istyle)
  (hash-remove* istyle '(cell-halign cell-valign cell-border cell-bgcolor)))

(define (table-cell->pict p istyle width)
  (define halign (hash-ref istyle 'cell-halign 'left))
  (define bgcolor (hash-ref istyle 'cell-bgcolor #f))
  (let* ([p (let ([dwidth (- width (pict-width p))])
              (case (hash-ref istyle 'cell-halign 'left)
                [(left) (inset p 0 0 dwidth 0)]
                [(right) (inset p dwidth 0 0 0)]
                [(center) (inset p (/ dwidth 2) 0 (/ dwidth 2) 0)]))]
         [p (cond [(hash-ref istyle 'cell-bgcolor #f)
                   => (lambda (c) (bg-colorize p c))]
                  [else p])])
    p))

;; render-table-cell : Block Style Style IStyle -> (cons Pict IStyle)
(define (render-table-cell block cell-style col-style istyle0)
  (define istyle (add-table-cell-style cell-style (add-table-cell-style col-style istyle0)))
  (cons (block->pict block (remove-table-cell-styles istyle)) istyle))

(define (get-bullet)
  ;;(text "∘" '(bold) (get-base-size))
  (arrowhead (* 2/3 (get-base-size)) 0))

;; An Element is (element Style Content)

(define (element->pict e istyle)
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

(define (content->pict content istyle)
  (define istyle* (remove-block-styles istyle))
  (apply-block-styles istyle (content->pict* content istyle* (hash-ref istyle 'width))))

(define (content->pict* content istyle width)
  (define fragments (content->fragments content istyle))
  (define lines (linebreak-fragments fragments width))
  (apply vl-append (get-line-sep)
         (for/list ([line (in-list lines)]
                    [index (in-naturals 1)])
           (line->pict line width istyle (= index (length lines))))))

(define (line->pict line width istyle last?)
  (apply hbl-append 0 line))

;; A Fragment is (cons (U Pict String) IStyle), where a string either
;; contains no whitespace or only whitespace.
;; FIXME: Start with laxer invariant: fragment never starts or ends
;; with whitespace; then only break if necessary.

;; content->fragments : Content IStyle -> (Listof Fragment)
(define (content->fragments content istyle)
  (match content
    [(? string?)
     (for/list ([seg (in-list (string->segments (regexp-replace* "\n" content " ")))])
       (cons seg istyle))]
    [(? symbol?) (list (cons (content-symbol->string content) istyle))] ;; FIXME
    [(? pict?) (list (cons content istyle))]
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
    [(nbsp) " "] ;; non-breaking space
    [else (error 'content-symbol->string "unknown symbol: ~e" sym)]))

(define (whitespace-fragment? frag)
  (and (pair? frag) (string? (car frag))
       (regexp-match? #px"^\\s*$" (car frag))
       (not (hash-ref (cdr frag) 'keep-whitespace? #f))))

(define (fragment->pict fragment)
  (define (finish p istyle)
    (let* ([p (cond [(hash-ref istyle 'color #f)
                     => (lambda (c) (colorize p c))]
                    [else p])]
           [p (cond [(hash-ref istyle 'bgcolor #f)
                     => (lambda (c) (bg-colorize p c))]
                    [else p])])
      p))
  (match fragment
    [(cons (? pict? p) istyle) (finish p istyle)]
    [(cons (? string? str) istyle)
     (define ptstyle (append (hash-ref istyle 'mods null) (hash-ref istyle 'base)))
     (define size (* (hash-ref istyle 'scale) (get-base-size)))
     (finish (text str ptstyle size) istyle)]))

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
      ;; FIXME: drop picts from whitespace fragments from racc before reverse!
      (values (reverse racc) frags))
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
  (flow->pict flow (add-style style (hash-set base-istyle 'width (get-para-width)))))

(module+ main
  (require slideshow slideshow/code
           (only-in scribble/core color-property background-color-property)
           (only-in scribble/base elem tabular hspace)
           (only-in scribble/manual litchar racketblock racketgrammar*)
           (prefix-in s: scribble/base)
           (for-label racket/base))

  (define (blue . content)
    (apply elem #:style (s:style #f (list (color-property "blue"))) content))

  (define (on-pink . content)
    (apply elem #:style (s:style #f (list (background-color-property "pink"))) content))

  (slide
   @flow-pict[#:style 'roman]{
     This whole slide consists of a @s:italic{flow}. It consists of
     multiple @s:tt{paragraphs} and @elem[#:style 'sf]{other such stuff}.

     @s:para[#:style (s:style #f (list (background-color-property "yellow")))]{
     This is a @s:italic{paragraph}. It is written using @blue{Scribble's
     @litchar["@"]-exp reader}, which means that when I use @code[para] and
     @code[it] and picts, @on-pink{I do not have to break things @elem[#:style 'larger]{manually}},
     like @code[(para "This" (it "is") "a para")]; I can write them @on-pink{more naturally}.
     }

     This @code[λ] is good stuff:
     @s:itemlist[
     @s:item{it is @s:italic{functional@elem[#:style 'superscript]{ish}}}
     @s:item{it is @s:italic{higher-order}@elem[#:style 'subscript]{for sure}}
     ]
     })

  (slide
   @frame[
   @flow-pict[#:style 'roman]{
     @; -- Needs table!
     @racketblock[
     (define (map f xs)
       (if (pair? xs)
           (cons (f (car xs)) (map f (cdr xs)))
           null))
     ]
     }]
   @frame[
   @flow-pict[#:style 'roman]{
     @racketgrammar*[
       #:literals (lambda)
       [X #, @s:elem{variable name}]
       [E X (lambda (X) E) (E E)]]
   }])

  (slide
   @flow-pict[#:style 'roman]{

     @tabular[#:sep @hspace[1]
              (list (list "soup" "gazpacho")
                    (list "soup" "tonjiru"))]

     @tabular[#:style 'boxed
              #:column-properties '(left right)
              #:row-properties '(bottom-border ())
              (list (list @s:bold{recipe}   @s:bold{vegetable})
                    (list "caldo verde"   "kale")
                    (list "kinpira gobō"  "burdock")
                    (list "makizushi"     'cont)
                    (list "This line ought to run into the second column." 'cont))]

   }))
