#lang racket
(require ppict/2)
(require pict)
(require racket/contract)
(require racket/struct)


(require "Basic_definitions.rkt")
(provide (all-defined-out))



(define (cons-box)
  (ppict-do (rectangle 25 45 #:border-color "Blue" #:border-width 2)
            #:go (coord 0.5 0.5 'cc)
            (hline 25 20)))

(define (cons-pic)
  ;(grid 2 2 1 1 'lt)
  (ppict-do (blank 50 45)
            #:go (coord 0.5 0 'lt)
            (cons-box)
            #:go (coord 0 0.125 'lt)
            (text "data")
            #:go (coord 0 0.875 'lb)
            (text "next")
            ))


(define (cons-text)
  ;(grid 2 2 1 1 'lt)
  (ppict-do (blank 25 45)
            #:go (coord 0 0.1 'lt)
            (text "data")
            #:go (coord 0 0.9 'lb)
            (text "next")
            ))
;(cons-text)

(struct cons-node (data data-pic next-pic box final)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'cons-node-struct)
      (λ (obj) (list 'data
                     (cons-node-data obj)
                     'data-pic
                     (cons-node-data-pic obj)
                     'next-pic
                     (cons-node-next-pic obj)
                     'box
                     (cons-node-box obj)
                     'final
                     (cons-node-final obj)))))])


(struct cons-list (nodes)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'cons-list-struct)
      (λ (obj) (list (cons-list-nodes obj)))))])


(define (make-list [len 1] #:last-node-next-pic [next-pic (null-ptr-pic)])
  (define first-node (cons-node null (blank 0) next-pic (cons-box) (blank 0)))
  (draw-node first-node)
  ;(println first-node)
  (define result (cons-list (list first-node)))
  (draw-list result)
  result
  )

(define (make-list-from-list lst #:last-node-next-pic [last-next-pic (null-ptr-pic)])
  (define result (cons-list (list)))
  (for-each (λ (data)
              (define node
                (cons-node data (blank 0) (ptr-base) (cons-box) (blank 0)))
              (draw-node node)
              (set-cons-list-nodes! result (append (cons-list-nodes result) (list node)))
              )
            lst)
  
  (draw-list result #:last-node-next-pic last-next-pic)
  result
  )


(define (draw-node node)
  (define real-content (cons-node-data node) )
  (define content
        (cond
          [(or (number? real-content) (string? real-content))
           (text (~a real-content))]
          [(null? real-content) (blank 0)]
          [(null-ptr? real-content) (null-ptr-pic)]
          [else (ptr-base)]
          )
        )
  (set-cons-node-data-pic! node content)
  (set-cons-node-final! node 
                        (hc-append (cons-text)
                                   (ppict-do (cons-node-box node)
                                             #:go (coord 0.5 0.25 'cc)
                                             content
                                             #:go (coord 0.5 0.75 'cc)
                                             (cons-node-next-pic node)))))


(define (draw-list conslist #:last-node-next-pic [last-next-pic (null-ptr-pic)])
  (define lst (cons-list-nodes conslist))
  (define base (blank 0))
  (for-each (λ (node)
              (set-cons-node-next-pic! node (ptr-base))
              (draw-node node)
              )
            lst)
  (set-cons-node-next-pic! (last lst) last-next-pic)
  (draw-node (last lst))
  (for-each (λ (node)
              (set! base (hc-append 20 base (cons-node-final node)))
              )
            lst)
  ;iterate through list - connect the arrows
  (for ([i (in-range (- (length (cons-list-nodes conslist)) 1))])
    (define from (list-ref (cons-list-nodes conslist) i))
    (define to (list-ref (cons-list-nodes conslist) (+ i 1)))
    (set! base
          (pin-arrow-line 10 base
                          (cons-node-next-pic from) rc-find
                          (cons-node-box to) lc-find
                          #:line-width 2
                          #:color "Medium Violet Red"
                          ))
    )
  base)

(define (add-single-node-end new-data conslist)
  (define new-node (cons-node new-data (blank 0) (null-ptr-pic) (cons-box) (blank 0)))
  (draw-node new-node)
  (set-cons-list-nodes! conslist (append (cons-list-nodes conslist) (list new-node)))
  (draw-list conslist)
  )
(define (list-src-getter lst index)
  (cons-node-data-pic (list-ref (cons-list-nodes lst) index)))
(define (list-dst-getter lst index)
  (cons-node-box (list-ref (cons-list-nodes lst) index)))
