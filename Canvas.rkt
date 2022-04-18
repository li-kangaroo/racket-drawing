#lang racket

(require ppict/2)
(require pict)
(require racket/contract)
(require racket/struct)

(require "Basic_definitions.rkt")
(require "Vectors.rkt")
(require "Cons.rkt")




(struct canvas
  (reprs final)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'canvas-struct)
      (λ (obj) (list (canvas-reprs obj)
                     (canvas-final obj)))))])

(define (blank-canvas width height)
  (define base (rectangle width height))
  (canvas '(base) base))

(define (insert-vec canvas vect x y)
  (set-canvas-reprs! canvas (list (append (canvas-reprs canvas) vect)))
  (set-canvas-final! canvas (pin-over (canvas-final canvas) x y (redraw-vec vect))))


(define (point-vec canvas from from-index to [to-func lt-find])
  (define real-from (vector-ref (vect-pic-contents from) from-index))
  ;(set! real-from (ptr-base))
  ;(redraw-vec from)

  (define fromx 0)
  (define fromy 0)
  fromx fromy (cc-find (canvas-final canvas) real-from)
  (define tox 0)
  (define toy 0)
  tox toy (lt-find (canvas-final canvas) (vect-final to))

  (define sa 180)
  (define fromfunc cb-find)
  (cond
    [(< fromy toy) (set! fromfunc ct-find)
                   (set! sa 0)]
    )

  (set-canvas-final! canvas
                     (panorama
                     (pin-arrow-line 10 (canvas-final canvas)
                                     real-from fromfunc
                                     (vect-final to) to-func
                                     #:line-width 2
                                     #:start-angle sa
                                     ;#:end-angle 345
                                     #:color "Medium Violet Red")
                     )))

#|
(define a (make-vec 10 0))
(define vecnull (make-vec 10))
(set-vec-cell a 2 (ptr))
(set-vec-cell a 6 (ptr))
(set-vec-cell a 7 (ptr))


(define test-canvas (blank-canvas 350 250))
(insert-vec test-canvas vecnull 0 50)
(insert-vec test-canvas a 0 0)

test-canvas
(point-vec test-canvas a 2 vecnull)

(point-vec test-canvas a 6 vecnull ct-find)

(point-vec test-canvas a 7 a)

test-canvas
|#