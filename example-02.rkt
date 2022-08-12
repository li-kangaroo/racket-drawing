#lang racket

(require ppict/2)
(require pict)
(require racket/contract)
(require racket/struct)

(require "Canvas.rkt")
(require "Vectors.rkt")
(require "Cons.rkt")
(require "Struct.rkt")
(require "Basic_definitions.rkt")


;Slide 8 - linked list spaghetti
;4 phases of animation
(println "slide 8 demo")

(define (slide-8-animator phase)
  (printf "slide 8, phase ~a \n" phase)
  (define slide-8-canvas (blank-canvas 300 330))
  (define slide-8-lst-1 (make-list-from-list (list 2 4 6 8)))
  (define slide-8-lst-2 (make-list-from-list (list 5)
                                             #:last-node-next-pic (ptr-base)))
  (define slide-8-lst-3 (make-list-from-list (list 7 3)
                                             #:last-node-next-pic (ptr-base)))
  
  (insert-list slide-8-canvas
               slide-8-lst-1
               (list 0.1 0.3 0.5 0.7)
               (list 0.4 0.4 0.4 0.4)
               false)
  
  (insert-list slide-8-canvas
               slide-8-lst-2
               (list 0.4)
               (list 0.7)
               false)
  (cond
    [(< phase 5)
     (link-node-to-node slide-8-canvas slide-8-lst-1 0 slide-8-lst-1 1
                        #:from-find rc-find)
     (link-node-to-node slide-8-canvas slide-8-lst-1 2 slide-8-lst-1 3
                        #:from-find rc-find)]
    [else
     (link-node-to-node slide-8-canvas slide-8-lst-1 0 slide-8-lst-1 1                    
                        #:color "Gray")
     (link-node-to-node slide-8-canvas slide-8-lst-1 2 slide-8-lst-1 3
                        #:color "Gray")])
  
  (cond
    [(= phase 1)
     (link-node-to-node slide-8-canvas slide-8-lst-1 1 slide-8-lst-1 2)]
    [(= phase 2)
     (link-node-to-node slide-8-canvas slide-8-lst-1 1 slide-8-lst-1 2)
     (link-node-to-node slide-8-canvas slide-8-lst-2 0 slide-8-lst-1 2
                        #:to-find cb-find)]
    [(= phase 3)
     (link-node-to-node slide-8-canvas slide-8-lst-1 1 slide-8-lst-1 2
                        #:color "Gray")
     (link-node-to-node slide-8-canvas slide-8-lst-1 1 slide-8-lst-2 0
                        #:from-find cb-find
                        #:to-find lt-find)
     (link-node-to-node slide-8-canvas slide-8-lst-2 0 slide-8-lst-1 2
                        #:to-find cb-find)]
    [(= phase 4)
     (insert-list slide-8-canvas
               slide-8-lst-3
               (list 0.2 0.6)
               (list 0.2 0.2)
               false)
     (link-node-to-node slide-8-canvas slide-8-lst-1 1 slide-8-lst-2 0
                        #:from-find cb-find
                        #:to-find lt-find)
     (link-node-to-node slide-8-canvas slide-8-lst-2 0 slide-8-lst-1 2
                        #:to-find cb-find)
     ]
    [(= phase 5)
     (insert-list slide-8-canvas
               slide-8-lst-3
               (list 0.2 0.6)
               (list 0.2 0.2)
               false)
     (link-node-to-node slide-8-canvas slide-8-lst-1 0 slide-8-lst-3 1
                        #:from-find ct-find
                        #:to-find ct-find
                        #:start-angle 90
                        #:start-pull 1.1
                        #:end-angle 200
                        )
     (link-node-to-node slide-8-canvas slide-8-lst-3 1 slide-8-lst-1 1
                        #:from-find cb-find
                        #:to-find rt-find)
     (link-node-to-node slide-8-canvas slide-8-lst-1 1 slide-8-lst-2 0
                        #:from-find cb-find
                        #:to-find lt-find)
     (link-node-to-node slide-8-canvas slide-8-lst-2 0 slide-8-lst-1 2
                        #:to-find cb-find)
     (link-node-to-node slide-8-canvas slide-8-lst-1 2 slide-8-lst-3 0 
                        #:from-find lc-find
                        #:to-find rb-find)
     (link-node-to-node slide-8-canvas slide-8-lst-3 0 slide-8-lst-1 3 
                        #:from-find cb-find
                        #:to-find cb-find
                        #:start-angle (/ (* 3 pi) 2)
                        #:start-pull 2.4
                        #:end-angle (/ pi 2))
     ]
     ;(link-node-to-node slide-8-canvas slide-8-lst-1 1 slide-8-lst-1 2 lc-find)]
    
    )
  
  (canvas-final slide-8-canvas)
  )


(slide-8-animator 1)
(slide-8-animator 2)
(slide-8-animator 3)
(slide-8-animator 4)
(slide-8-animator 5)


