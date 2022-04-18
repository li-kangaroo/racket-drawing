#lang racket
(require ppict/2)
(require pict)
(require racket/contract)
(require racket/struct)

(provide (all-defined-out))


(define (ptr-base)
  (ppict-do (disk 15
                  #:draw-border? #f
                  #:color "Medium Violet Red"
                  )
            #:go (coord 0.5 0.5 'cc)
            (disk 11
                  #:draw-border? #f
                  #:color "White")))

(define (null-ptr-pic)
  (define base (disk 15
                   #:draw-border? #f
                   #:color "Deep Sky Blue"
                   ))
  (define white-out (disk 11
                   #:draw-border? #f
                   #:color "White"))
    
  (rotate (pin-line
           (ppict-do base
                     #:go (coord 0.5 0.5 'cc)
                     white-out)
           base ct-find
           base cb-find
           #:line-width 2
           #:color "Deep Sky Blue")
          (/ (* 3 pi) 4 )))

(struct null-ptr ())
(struct ptr ())