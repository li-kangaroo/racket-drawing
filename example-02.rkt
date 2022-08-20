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
;various phases of animation
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

;Slides 9-11: linked list manipulation
;various phases of animation
(println "slide 9-11 demo")
(define slide-9-canvas (blank-canvas 550 100))
(define slide-9-lst-1 (make-list-from-list (list 2 3 4 5 6 7 8)))
(define slide-9-lst-2 (make-list-from-list (list 1 2 3 4 5 6 7 8)))

(define (slide-9-animator phase)
  (printf "slide 9, phase ~a \n" phase)
  (define slide-9-canvas (blank-canvas 550 200))
  (define slide-9-lst-1 (make-list-from-list (list 2 3 4 5 6 7 8)))
  (define slide-9-lst-2 (make-list-from-list (list 1 2 3 4 5 6 7 8)))

  (define lst-struct
    (create-struct (list "lst")
                   (list (ptr))))
  (define head-struct
    (create-struct (list "head")
                   (list (ptr))))

  (cond
    [(or (= phase 1) (= phase 3) (= phase 6))
     (insert-list slide-9-canvas
                  slide-9-lst-1
                  (list 0.15 0.25 0.35 0.45 0.55 0.65 0.75)
                  (list 0.75 0.75 0.75 0.75 0.75 0.75 0.75)
                  true)]
    [else
     (insert-list slide-9-canvas
                  slide-9-lst-2
                  (list 0.05 0.15 0.25 0.35 0.45 0.55 0.65 0.75)
                  (list 0.75 0.75 0.75 0.75 0.75 0.75 0.75 0.75)
                  true)])
  (cond
    [(> phase 2)
     (insert-struct slide-9-canvas
                    lst-struct
                    0.03 0.1)])
  (cond
    [(> phase 5)
     (insert-struct slide-9-canvas
                    head-struct
                    0.005 0.45)
     (point-struct-to-struct slide-9-canvas
                             lst-struct
                             "lst"
                             head-struct
                             #:from-find cb-find
                             #:to-find ct-find)])
    
  
  (cond
    [(= phase 1)]
    [(= phase 2)]
    [(= phase 3)
     (point-struct-to-node slide-9-canvas
                           lst-struct
                           "lst"
                           slide-9-lst-1
                           0
                           #:to-find ct-find
                           #:start-angle 0
                           #:end-angle (* 6 (/ pi 4))
                           #:start-pull 0.5
                           )
     
     ]
    [(= phase 4)
     (point-struct-to-node slide-9-canvas
                           lst-struct
                           "lst"
                           slide-9-lst-2
                           1
                           #:to-find ct-find
                           #:start-angle 0
                           #:end-angle (* 6 (/ pi 4))
                           #:start-pull 0.5
                           )
     ]
    [(= phase 5)
     (point-struct-to-node slide-9-canvas
                           lst-struct
                           "lst"
                           slide-9-lst-2
                           1
                           #:to-find ct-find
                           #:start-angle 0
                           #:end-angle (* 6 (/ pi 4))
                           #:start-pull 0.5
                           #:color "Gray"
                           )
     (point-struct-to-node slide-9-canvas
                           lst-struct
                           "lst"
                           slide-9-lst-2
                           0
                           #:to-find rt-find
                           #:start-angle 0
                           #:end-angle (* 5 (/ pi 4))
                           #:start-pull 0.5
                           )
     ]
    [(= phase 6)
     (point-struct-to-node slide-9-canvas
                           head-struct
                           "head"
                           slide-9-lst-1
                           0
                           #:to-find lt-find
                           #:start-angle 0
                           #:end-angle (* 6.5 (/ pi 4))
                           #:start-pull 0.5
                           )
     ]
    [(= phase 7)
     (point-struct-to-node slide-9-canvas
                           head-struct
                           "head"
                           slide-9-lst-2
                           1
                           #:to-find lt-find
                           #:start-angle 0
                           #:end-angle (* 6.5 (/ pi 4))
                           #:start-pull 0.5
                           )
     ]
    [(= phase 8)
     (point-struct-to-node slide-9-canvas
                           head-struct
                           "head"
                           slide-9-lst-2
                           1
                           #:to-find lt-find
                           #:start-angle 0
                           #:end-angle (* 6.5 (/ pi 4))
                           #:start-pull 0.5
                           #:color "Gray"
                           )
     (point-struct-to-node slide-9-canvas
                           head-struct
                           "head"
                           slide-9-lst-2
                           0
                           #:to-find rt-find
                           #:start-angle 0
                           #:end-angle (* 5 (/ pi 4))
                           #:start-pull 0.5
                           )
     ]
    [(= phase 9)
     (point-struct-to-node slide-9-canvas
                           head-struct
                           "head"
                           slide-9-lst-2
                           0
                           #:to-find rt-find
                           #:start-angle 0
                           #:end-angle (* 5 (/ pi 4))
                           #:start-pull 0.5
                           )
     ]
    [(= phase 10)
     (point-struct-to-node slide-9-canvas
                           head-struct
                           "head"
                           slide-9-lst-2
                           0
                           #:to-find rt-find
                           #:start-angle 0
                           #:end-angle (* 5 (/ pi 4))
                           #:start-pull 0.5
                           #:color "Gray"
                           )
     (point-struct-to-node slide-9-canvas
                           head-struct
                           "head"
                           slide-9-lst-2
                           1
                           #:to-find lt-find
                           #:start-angle 0
                           #:end-angle (* 6.5 (/ pi 4))
                           #:start-pull 0.5
                           )
     ]


    )
  (println (canvas-final slide-9-canvas))
  )

(for ([i (in-range 1 11)])
    (slide-9-animator i))




;Slides 18-19: more fields in the struct
(println "Slides 18-19 demo")
(define (slide-18-animator phase)
  (printf "slide 18, phase ~a \n" phase)
  (define slide-18-canvas (blank-canvas 450 250))
  (define slide-18-lst-1 (make-list-from-list (list 1 2 3 4 5 6 )))
  
  (define head-struct
    (create-struct (list "head" "len")
                   (list (ptr) 6)))
  (define head-tail-struct
    (create-struct (list "head" "tail" "len")
                   (list (ptr) (ptr) 6)))
  (insert-list slide-18-canvas
                  slide-18-lst-1
                  (list 0.25 0.375 0.5 0.625 0.75 0.875)
                  (list 0.65 0.65 0.65 0.65 0.65 0.65)
                  true)
  (cond
    [(or (= phase 1))
     (insert-struct slide-18-canvas
                    head-struct
                    0.1 0.15)
     (point-struct-to-node slide-18-canvas
                           head-struct
                           "head"
                           slide-18-lst-1
                           0
                           #:to-find ct-find
                           #:start-angle 0
                           #:end-angle (* 6 (/ pi 4))
                           #:start-pull 0.5
                           )]
    [else
     (insert-struct slide-18-canvas
                    head-tail-struct
                    0.1 0.15)
     (point-struct-to-node slide-18-canvas
                           head-tail-struct
                           "head"
                           slide-18-lst-1
                           0
                           #:to-find ct-find
                           #:start-angle 0
                           #:end-angle (* 6 (/ pi 4))
                           #:start-pull 0.5
                           )
     (point-struct-to-node slide-18-canvas
                           head-tail-struct
                           "tail"
                           slide-18-lst-1
                           5
                           #:to-find ct-find
                           #:start-angle 0
                           #:end-angle (* 7 (/ pi 4))
                           #:start-pull 0.5
                           )
     ])




  
  (canvas-final slide-18-canvas)
  )

(slide-18-animator 1)
(slide-18-animator 2)

;Slide 21: DLL
(println "Slide 21 demo")
(define (make-dll-node data prev next)
  (create-struct (list "data" "prev" "next")
                 (list data prev next)
                 #:fixed-width 10))

(define (slide-21-animator phase)
  (printf "slide 21, phase ~a \n" phase)
  (define slide-21-canvas (blank-canvas 550 250))
  (define slide-21-lst-1 (make-list-from-list (list 1 2 3 4 5 6 )))

  (define len 6)
  (define 5-next (ptr))
  (cond
    [(or (= phase 2))
     (set! len 5)
     (set! 5-next (null-ptr))
     ])
  
  (define head-tail-struct
    (create-struct (list "head" "tail" "len")
                   (list (ptr) (ptr) len)))
  (insert-struct slide-21-canvas
                 head-tail-struct
                 0.01 0.15)
  
  (define dll-node-1 (make-dll-node 1 (null-ptr) (ptr)))
  (define dll-node-2 (make-dll-node 2 (ptr) (ptr)))
  (define dll-node-3 (make-dll-node 3 (ptr) (ptr)))
  (define dll-node-4 (make-dll-node 4 (ptr) (ptr)))
  (define dll-node-5 (make-dll-node 5 (ptr) 5-next))
  (define dll-node-6 (make-dll-node 6 (ptr) (null-ptr)))
  (insert-struct slide-21-canvas
                 dll-node-1
                 0.1 0.65)
  (insert-struct slide-21-canvas
                 dll-node-2
                 0.25 0.65)
  (insert-struct slide-21-canvas
                 dll-node-3
                 0.4 0.65)
  (insert-struct slide-21-canvas
                 dll-node-4
                 0.55 0.65)
  (insert-struct slide-21-canvas
                 dll-node-5
                 0.7 0.65)
  (insert-struct slide-21-canvas
                 dll-node-6
                 0.85 0.65)
  (point-struct-to-struct slide-21-canvas
                          head-tail-struct
                          "head"
                          dll-node-1
                          #:to-find ct-find
                          #:start-angle 0
                          #:start-pull 0.3
                          #:end-angle (* 3 (/ pi 2)))
  (point-struct-to-struct slide-21-canvas
                          dll-node-1
                          "next"
                          dll-node-2
                          #:to-find lb-find
                          #:start-angle 5.8
                          #:start-pull 0.05
                          #:end-angle 0.5
                          #:end-pull 0.5)
  (point-struct-to-struct slide-21-canvas
                          dll-node-2
                          "next"
                          dll-node-3
                          #:to-find lb-find
                          #:start-angle 5.8
                          #:start-pull 0.05
                          #:end-angle 0.5
                          #:end-pull 0.5)
  (point-struct-to-struct slide-21-canvas
                          dll-node-3
                          "next"
                          dll-node-4
                          #:to-find lb-find
                          #:start-angle 5.8
                          #:start-pull 0.05
                          #:end-angle 0.5
                          #:end-pull 0.5)
  (point-struct-to-struct slide-21-canvas
                          dll-node-4
                          "next"
                          dll-node-5
                          #:to-find lb-find
                          #:start-angle 5.8
                          #:start-pull 0.05
                          #:end-angle 0.5
                          #:end-pull 0.5)
  
  (point-struct-to-struct slide-21-canvas
                          dll-node-2
                          "prev"
                          dll-node-1
                          #:from-find lc-find
                          #:to-find rc-find
                          #:start-angle 2.7
                          #:start-pull 0.25
                          #:end-angle 3.3)
  (point-struct-to-struct slide-21-canvas
                          dll-node-3
                          "prev"
                          dll-node-2
                          #:from-find lc-find
                          #:to-find rc-find
                          #:start-angle 2.7
                          #:start-pull 0.25
                          #:end-angle 3.3)
  (point-struct-to-struct slide-21-canvas
                          dll-node-4
                          "prev"
                          dll-node-3
                          #:from-find lc-find
                          #:to-find rc-find
                          #:start-angle 2.7
                          #:start-pull 0.25
                          #:end-angle 3.3)
  (point-struct-to-struct slide-21-canvas
                          dll-node-5
                          "prev"
                          dll-node-4
                          #:from-find lc-find
                          #:to-find rc-find
                          #:start-angle 2.7
                          #:start-pull 0.25
                          #:end-angle 3.3)
  (point-struct-to-struct slide-21-canvas
                          dll-node-6
                          "prev"
                          dll-node-5
                          #:from-find lc-find
                          #:to-find rc-find
                          #:start-angle 2.7
                          #:start-pull 0.25
                          #:end-angle 3.3)
  
  (cond
    [(or (= phase 1))
     (point-struct-to-struct slide-21-canvas
                          dll-node-5
                          "next"
                          dll-node-6
                          #:to-find lb-find
                          #:start-angle 5.8
                          #:start-pull 0.05
                          #:end-angle 0.5
                          #:end-pull 0.5)
     (point-struct-to-struct slide-21-canvas
                          head-tail-struct
                          "tail"
                          dll-node-6
                          #:to-find ct-find
                          #:start-angle 0
                          #:start-pull 0.3
                          #:end-angle (* 7 (/ pi 4)))
     ]
    [(or (= phase 2))
     (point-struct-to-struct slide-21-canvas
                          head-tail-struct
                          "tail"
                          dll-node-5
                          #:to-find ct-find
                          #:start-angle 0
                          #:start-pull 0.3
                          #:end-angle (* 7 (/ pi 4)))
     ])
  
  
  


  
  (canvas-final slide-21-canvas)
  )
(slide-21-animator 1)
(slide-21-animator 2)


;Slide 22: Dynamic array
(println "Slide 22 demo")

(define (slide-22-animator phase)
  (printf "slide 22, phase ~a \n" phase)
  (define slide-22-canvas (blank-canvas 400 150))
  (define len 1)
  (cond
    [(< phase 5)
     (set! len phase)]
    [(< phase 8)
     (set! len 4)]
    [else
     (set! len (- phase 3))]
    )
  (define array-struct
    (create-struct (list "data" "len")
                   (list (ptr) len)))
  (insert-struct slide-22-canvas
                 array-struct
                 0.03
                 0.4)

  (define slide-22-small-vector (make-vec-from-vec (vector 2 null null null)))
  (define slide-22-large-vector (make-vec-from-vec
                                (make-vector 8 null)))
  (cond
    [(= phase 2)
     (set! slide-22-small-vector
           (make-vec-from-vec (vector 2 3 null null)))]
    [(= phase 3)
     (set! slide-22-small-vector
           (make-vec-from-vec (vector 2 3 4 null)))]
    [(> phase 3)
     (set! slide-22-small-vector
           (make-vec-from-vec (vector 2 3 4 5)))])
  (cond
    [(or (= phase 6) (= phase 7))
     (set! slide-22-large-vector
           (make-vec-from-vec
            (vector 2 3 4 5 null null null null)))]
    [(= phase 8)
     (set! slide-22-large-vector
           (make-vec-from-vec
            (vector 2 3 4 5 6 null null null)))]
    [(= phase 9)
     (set! slide-22-large-vector
           (make-vec-from-vec
            (vector 2 3 4 5 6 7 null null)))]
    [(= phase 10)
     (set! slide-22-large-vector
           (make-vec-from-vec
            (vector 2 3 4 5 6 7 8 null)))]
    [(= phase 11)
     (set! slide-22-large-vector
           (make-vec-from-vec
            (vector 2 3 4 5 6 7 8 9)))])
  

  (cond
    [(< phase 7)
     (insert-vec slide-22-canvas
              slide-22-small-vector
              0.5
              0.4
              false)
     (point-struct-to-vec slide-22-canvas
                          array-struct
                          "data"
                          slide-22-small-vector
                          #:to-find ct-find
                          #:start-angle 1
                          #:end-angle 5.5
                          )])
  (cond
    [(> phase 4)
     (insert-vec slide-22-canvas
              slide-22-large-vector
              0.4
              0.7
              false)
     ])
  (cond
    [(> phase 6)
     (point-struct-to-vec slide-22-canvas
                          array-struct
                          "data"
                          slide-22-large-vector
                          #:to-find ct-find
                          #:start-angle 0.5
                          #:end-angle 5.5)
     ])
  
  
  

  (canvas-final slide-22-canvas))


(slide-22-animator 1)
(slide-22-animator 2)
(slide-22-animator 3)
(slide-22-animator 4)
(slide-22-animator 5)
(slide-22-animator 6)
(slide-22-animator 7)
(slide-22-animator 8)
(slide-22-animator 9)
(slide-22-animator 10)
(slide-22-animator 11)