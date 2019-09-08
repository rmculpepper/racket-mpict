#lang at-exp racket/base
(require slideshow slideshow/code
         "scrib.rkt"
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/base)
         (prefix-in s: scribble/manual)
         (prefix-in s: scribble/html-properties)
         (prefix-in s: scribble/latex-properties)
         (for-label racket/base))

(define (blue . content)
  (apply s:elem #:style (s:style #f (list (s:color-property "blue"))) content))

(define (on-pink . content)
  (apply s:elem #:style (s:style #f (list (s:background-color-property "pink"))) content))

(slide
 @flow-pict[#:style 'roman]{
   This whole slide consists of a @s:italic{flow}. It consists of
   multiple @s:tt{paragraphs} and @s:elem[#:style 'sf]{other such stuff}.

   @s:para[#:style (s:style #f (list (s:background-color-property "yellow")))]{
   This is a @s:italic{paragraph}. It is written using @blue{Scribble's
   @s:litchar["@"]-exp reader}, which means that when I use @code[para] and
   @code[it] and picts, @on-pink{I do not have to break things @s:elem[#:style 'larger]{manually}},
   like @code[(para "This" (it "is") "a para")]---I can write them @on-pink{``more naturally''}.
   }

   This @code[λ] is good stuff:
   @s:itemlist[
   @s:item{it is @s:italic{functional@s:elem[#:style 'superscript]{ish}}}
   @s:item{it is @s:italic{higher-order}@s:elem[#:style 'subscript]{for sure}}
   ]
   })

(slide
 @flow-pict[#:style 'roman]{
   This is some centered text within a paragraph:
   @s:centered{carpe diem}
   and this is what comes after.

   And this is a whole new paragraph.
   @s:defproc[(foo [bar baz?]) quux/c]{
   Returns the best @s:italic{quux} appropriate for @s:racket[bar].
   }
 })

(slide
 @frame[
 @flow-pict[#:style 'roman]{
   @; -- Needs table!
   @s:racketblock[
   (define (map f xs)
     (if (pair? xs)
         (cons (f (car xs)) (map f (cdr xs)))
         '()))
   ]
   }]
 @frame[
 @flow-pict[#:style 'roman]{
   @s:racketgrammar*[
     #:literals (lambda)
     [X #, @s:elem{variable name}]
     [E X (lambda (X) E) (E E)]]
 }])

(slide
 @flow-pict[#:style 'roman]{

   @s:tabular[#:sep @s:hspace[1] #:style 'centered
            (list (list "soup" "gazpacho")
                  (list "soup" "tonjiru"))]

   @s:tabular[#:style 'boxed
            #:column-properties '(left right)
            #:row-properties '(bottom-border ())
            (list (list @s:bold{recipe}   @s:bold{vegetable})
                  (list "caldo verde"   "kale")
                  (list "kinpira gobō"  "burdock")
                  (list "makizushi"     'cont)
                  (list "This line ought to run into the second column." 'cont))]

 })
