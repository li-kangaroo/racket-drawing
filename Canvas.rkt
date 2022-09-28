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

(define (insert-vec canvas vect x y
                    #:show-indices? [show-indices? #t])
  (set-canvas-reprs! canvas (append (canvas-reprs canvas) (list vect)))
  (redraw-vec vect)
  (if show-indices?
      (set-canvas-final! canvas
                         (ppict-do (canvas-final canvas)
                                   #:go  (coord x y 'lt)
                                   (vect-final vect)))
      (set-canvas-final! canvas
                         (ppict-do (canvas-final canvas)
                                   #:go  (coord x y 'lt)
                                   (vect-final-no-indices vect)))
      ))


(define (point-vec-to-vec canvas from from-index to
                          #:from-find [from-find rc-find]
                          #:to-find [to-find lc-find]
                          #:color [clr "Medium Violet Red"]
                          #:start-angle [sa #f]
                          #:end-angle [ea #f]
                          #:start-pull [sp 1/4]
                          #:end-pull [ep 1/4])
  (define real-from (vector-ref (vect-pic-contents from) from-index))

  (set-canvas-final! canvas
                     (pin-arrow-line 10 (canvas-final canvas)
                                     real-from from-find
                                     (vect-final-no-indices to) to-find
                                     #:line-width 2
                                     #:start-angle sa
                                     #:end-angle ea
                                     #:color clr
                                     #:start-pull sp
                                     #:end-pull ep)
                     ))

(define (point-vec-to-struct canvas from from-index to
                             #:from-find [from-find rc-find]
                             #:to-find [to-find lc-find]
                             #:color [clr "Medium Violet Red"]
                             #:start-angle [sa #f]
                             #:end-angle [ea #f]
                             #:start-pull [sp 1/4]
                             #:end-pull [ep 1/4])
  (define real-from (vector-ref (vect-pic-contents from) from-index))
  (set-canvas-final! canvas
                     (pin-arrow-line 10 (canvas-final canvas)
                                     real-from from-find
                                     (struct-struct-box to) to-find
                                     #:line-width 2
                                     #:start-angle sa
                                     #:end-angle ea
                                     #:start-pull sp
                                     #:end-pull ep
                                     #:color clr)
                     ))

(define (insert-list canvas lst lst-x-coord lst-y-coord link?)
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

(define (insert-struct canvas strct x y)
  (set-canvas-reprs! canvas (append (canvas-reprs canvas) (list strct)))
  
  (set-canvas-final! canvas
                     (ppict-do (canvas-final canvas)
                               #:go  (coord x y 'lt)
                               (struct-struct-final strct)))
  )


(define (point-struct-to-vec canvas from from-field to
                             #:from-find [from-find rc-find]
                             #:to-find [to-find lc-find]
                             #:color [clr "Medium Violet Red"]
                             #:start-angle [sa #f]
                             #:end-angle [ea #f]
                             #:start-pull [sp 1/4]
                             #:end-pull [ep 1/4])
  (define real-from (void))
  (for ([i (in-range (length (struct-struct-lst-field-names from)))])
    (if (equal? (list-ref (struct-struct-lst-field-names from) i) from-field)
        (set! real-from (list-ref (struct-struct-lst-pics from) i))
        (void))
    )
  

  

  (set-canvas-final! canvas
                     (pin-arrow-line 10 (canvas-final canvas)
                                     real-from from-find
                                     (vect-final-no-indices to) to-find
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


(define (point-struct-to-node canvas from from-field to to-index
                           #:from-find [from-find rc-find]
                           #:to-find [to-find lc-find]
                           #:color [clr "Medium Violet Red"]
                           #:start-angle [sa #f]
                           #:end-angle [ea #f]
                           #:start-pull [sp 1/4]
                           #:end-pull [ep 1/4])
  (define real-from (void))
  (for ([i (in-range (length (struct-struct-lst-field-names from)))])
    (if (equal? (list-ref (struct-struct-lst-field-names from) i) from-field)
        (set! real-from (list-ref (struct-struct-lst-pics from) i))
        (void))
    )
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

(define (point-struct-to-struct canvas from from-field to 
                           #:from-find [from-find rc-find]
                           #:to-find [to-find lc-find]
                           #:color [clr "Medium Violet Red"]
                           #:start-angle [sa #f]
                           #:end-angle [ea #f]
                           #:start-pull [sp 1/4]
                           #:end-pull [ep 1/4])
  (define real-from (void))
  (for ([i (in-range (length (struct-struct-lst-field-names from)))])
    (if (equal? (list-ref (struct-struct-lst-field-names from) i) from-field)
        (set! real-from (list-ref (struct-struct-lst-pics from) i))
        (void))
    )
  
  (set-canvas-final! canvas
                     (pin-arrow-line 10 (canvas-final canvas)
                                     real-from from-find
                                     (struct-struct-box to) to-find
                                     #:line-width 2
                                     #:start-angle sa
                                     #:end-angle ea
                                     #:start-pull sp
                                     #:end-pull ep
                                     #:color clr)
                     ))

#|
(println "begins here")
(define test-canvas (blank-canvas 350 250))
(insert-list test-canvas a (list 0.1 0.3 0.5) (list 0.1 0.3 0.1) true)
;test-canvas



(define a (make-vec 10 0))
(define vecnull (make-vec 10))
(set-vec-cell a 2 (ptr))
(set-vec-cell a 6 (ptr))
(set-vec-cell a 7 (ptr))


(define test-canvas (blank-canvas 350 250))
(insert-vec test-canvas vecnull 0.02 0.5 true)
(insert-vec test-canvas a 0.02 0.02 true)

test-canvas
(point-vec test-canvas a 2 vecnull)

(point-vec test-canvas a 6 vecnull ct-find)

(point-vec test-canvas a 7 a)

test-canvas
|#