#lang racket

(require ppict/2)
(require pict)
(require racket/contract)
(require racket/struct)

(require "Basic_definitions.rkt")
(require "Vectors.rkt")
(require "Cons.rkt")
(require "Struct.rkt")

(provide (all-defined-out))

(struct canvas
  (reprs final)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'canvas-struct)
      (λ (obj) (list (canvas-reprs obj)
                     'final
                     (canvas-final obj)))))])

(define (blank-canvas width height)
  (define base (rectangle width height))
  (canvas (list) base))

(define (insert canvas target x y
                #:finder [finder 'lt])
  (define target-pict
    (cond
      [(vect? target) (vect-final target)]
      [(struct-struct? target) (struct-struct-final target)]
      ))
  ;future custom structs will need to add to this cond case - pull the pict out of the struct
  (set-canvas-reprs! canvas (append (canvas-reprs canvas) (list target)))
  (set-canvas-final! canvas
                     (ppict-do (canvas-final canvas)
                               #:go  (coord x y finder)
                               target-pict))
  )

(define (insert-list canvas lst lst-x-coord lst-y-coord
                     #:link-nodes? [link? #t])
  (cond
    [(not (= (length lst-x-coord) (length lst-y-coord)))
     (error "need equal number of x and y coordinates")]
    [(not (= (length lst-x-coord) (length (cons-list-nodes lst))))
      (error "need equal number of nodes and coordinates")]
    )
  (set-canvas-reprs! canvas (append (canvas-reprs canvas) (list lst)))
  (for ([i (in-range (length lst-x-coord))])
    (define node (list-ref (cons-list-nodes lst) i))
    (define node-pic (cons-node-final node))
    (set-canvas-final! canvas
                       (ppict-do (canvas-final canvas)
                                 #:go  (coord (list-ref lst-x-coord i)
                                              (list-ref lst-y-coord i)
                                              'lt)
                                 node-pic))
    )
  (if link?
      (for ([i (in-range (- (length (cons-list-nodes lst)) 1))])
        (define from (list-ref (cons-list-nodes lst) i))
        (define to (list-ref (cons-list-nodes lst) (+ i 1)))
        (set-canvas-final! canvas
                           (pin-arrow-line 10 (canvas-final canvas)
                                           (cons-node-next-pic from) rc-find
                                           (cons-node-box to) lc-find
                                           ;TODO - user customisable locations
                                           #:line-width 2
                                           #:color "Medium Violet Red"
                                           ))
        )
      (void))
  )

(define (point-x-to-y canvas
                      call-src-getter-on-x
                      call-dst-getter-on-y
                      #:from-find [from-find rc-find]
                      #:to-find [to-find lc-find]
                      #:color [clr "Medium Violet Red"]
                      #:start-angle [sa #f]
                      #:end-angle [ea #f]
                      #:start-pull [sp 1/4]
                      #:end-pull [ep 1/4])
  (define x-loc call-src-getter-on-x)
  (define y-loc call-dst-getter-on-y)
  
  (set-canvas-final! canvas
                     (pin-arrow-line 10 (canvas-final canvas)
                                     x-loc from-find
                                     y-loc to-find
                                     #:line-width 2
                                     #:start-angle sa
                                     #:end-angle ea
                                     #:start-pull sp
                                     #:end-pull ep
                                     #:color clr)
                     ))


(define (link-node-to-node canvas from from-index to to-index
                           #:from-find [from-find rc-find]
                           #:to-find [to-find lc-find]
                           #:color [clr "Medium Violet Red"]
                           #:start-angle [sa #f]
                           #:end-angle [ea #f]
                           #:start-pull [sp 1/4]
                           #:end-pull [ep 1/4])
  (define from-node (list-ref (cons-list-nodes from) from-index))
  (define real-from (cons-node-next-pic from-node))
  (define real-to (cons-node-box (list-ref (cons-list-nodes to) to-index)))
  (set-canvas-final! canvas
                     (pin-arrow-line 10 (canvas-final canvas)
                                     real-from from-find
                                     real-to to-find
                                     #:line-width 2
                                     #:start-angle sa
                                     #:end-angle ea
                                     #:start-pull sp
                                     #:end-pull ep
                                     #:color clr)
                     ))