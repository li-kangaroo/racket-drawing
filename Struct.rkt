#lang racket
(require ppict/2)
(require pict)
(require racket/contract)
(require racket/struct)


(require "Basic_definitions.rkt")
(provide (all-defined-out))


(struct struct-struct (lst-field-names lst-contents lst-pics box final)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'struct-struct-struct)
      (λ (obj) (list 'field-names
                     (struct-struct-lst-field-names obj)
                     'lst-contents
                     (struct-struct-lst-contents obj)
                     'lst-pics
                     (struct-struct-lst-pics obj)
                     'box
                     (struct-struct-box obj)
                     'final
                     (struct-struct-final obj)))))])

(define (create-struct lst-fields lst-contents
                       #:fixed-width [fixed-width null])
  (cond
    [(not (= (length lst-fields) (length lst-contents)))
     (error "need equal number of fields and contents")]
    )
  (define num-fields (length lst-fields))
  (define box-pic (rectangle 25
                             (* 25 num-fields)
                             #:border-color "Blue"
                             #:border-width 2))
  (for ([i (in-range (- num-fields 1))])
    (set! box-pic (ppict-do box-pic
                            #:go (coord 0.5 (/ (+ 1 i) num-fields) 'cc)
                            (hline 25 20)))
    )
  
  (define result
    (struct-struct lst-fields
                   lst-contents
                   (list)
                   box-pic
                   (blank 0)))
  (draw-struct result #:fixed-width fixed-width)
  result)

(define (draw-struct strct
                     #:fixed-width [fixed-width null])
  (define num-fields (length (struct-struct-lst-contents strct)))
  (set-struct-struct-lst-pics! strct
   (build-list num-fields
                               (λ (x)
                                 (define real-content (list-ref
                                                       (struct-struct-lst-contents strct)
                                                       x))
                                 (define content
                                   (cond
                                     [(or (number? real-content) (string? real-content))
                                      (text (~a real-content))]
                                     [(null? real-content) (blank 0)]
                                     [(null-ptr? real-content) (null-ptr-pic)]
                                     [else (ptr-base)]
                                     )
                                   )
                                 content
                                 )))
  
  (define max-width 0)
  (define max-width-content 0)
  (for ([i (in-range num-fields)])
    (set! max-width (max
                     max-width
                     (pict-width (text (list-ref (struct-struct-lst-field-names strct)
                                            i)))))
    (set! max-width-content (max
                             max-width-content
                             (pict-width (list-ref (struct-struct-lst-pics strct)
                                            i))))
    )
  (define box-width 0)
  (if (null? fixed-width)
      (set! box-width max-width-content)
      (set! box-width fixed-width))
  (define filled-box (rectangle (+ 10 box-width)
                             (* 25 num-fields)
                             #:border-color "Blue"
                             #:border-width 2))
  (for ([i (in-range (- num-fields 1))])
    (set! filled-box (ppict-do filled-box
                            #:go (coord 0.5 (/ (+ 1 i) num-fields) 'cc)
                            (hline (+ 10 box-width) 20)))
    )
  (define labels (blank (+ max-width 5) (pict-height (struct-struct-box strct))))
  
  (for ([i (in-range num-fields)])
    (set! labels (ppict-do labels
                            #:go (coord 1 (/ (+ 1 (* 2 i)) (* 2 num-fields)) 'rc)
                            (text (list-ref (struct-struct-lst-field-names strct)
                                            i))))
    (set! filled-box (ppict-do filled-box
                            #:go (coord 0.5 (/ (+ 1 (* 2 i)) (* 2 num-fields)) 'cc)
                            (list-ref (struct-struct-lst-pics strct)
                                      i)))
    )
  (define final-pic (hc-append labels (blank 5 5) filled-box))
  (set-struct-struct-box! strct filled-box)
  (set-struct-struct-final! strct final-pic)
  (struct-struct-final strct))

(define (struct-pict-getter strct)
  (vector strct (struct-struct-final strct)))

(define (struct-loc-getter strct
                           #:field [field #f])
  (cond
        [field
         (define loc (void))
         (for ([i (in-range (length (struct-struct-lst-field-names strct)))])
           (if (equal? (list-ref (struct-struct-lst-field-names strct) i) field)
               (set! loc (list-ref (struct-struct-lst-pics strct) i))
               (void))
           )
         (vector strct loc)]
        [else
         (vector strct (struct-struct-box strct))]
        )
  )


;(define test-struct (create-struct
;                     (list "number"
;                           "name"
;                           "position")
;                     (list 928 (ptr) 4)))
;test-struct