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


;Slide 11 and 12 - simple vector
(println "slide 11 demo")
(define slide-11-canvas (blank-canvas 100 100))
(define slide-11-vector (make-vec-from-vec (vector 14 2 65 23)))
(insert slide-11-canvas (vec-pict-getter slide-11-vector) 0.1 0.1)
(canvas-final slide-11-canvas)


;Slide 13 - simple linked list
;See TODO.txt regarding customizability
(println "slide 13 demo")
(define slide-13-canvas (blank-canvas 300 100))
(define slide-13-lst (make-list-from-list (list 14 2 65 23)))

(insert-list slide-13-canvas
             slide-13-lst
             (list 0.1 0.3 0.5 0.7)
             (list 0.2 0.2 0.2 0.2))

(canvas-final slide-13-canvas)

;Slide 33 - simple structs
(println "slide 33 demo")
(define slide-33-canvas (blank-canvas 300 100))
(define struct-33a (create-struct
                    (list "x"
                           "y")
                    (list -3 4)))

(define struct-33b (create-struct
                    (list "number"
                          "name"
                          "position")
                    (list 928 "Adam" 4)))

(insert slide-33-canvas (struct-pict-getter struct-33a) 0.1 0.3)
(insert slide-33-canvas (struct-pict-getter struct-33a) 0.6 0.1)

(canvas-final slide-33-canvas)


;Slide 35, 36 - arrows
(println "slide 35 demo")
(define slide-35-canvas (blank-canvas 450 300))

(define slide-35-spine-vector (make-vec-from-vec (make-vector 4 (ptr))))

(insert slide-35-canvas
        (vec-pict-getter slide-35-spine-vector)
        0.35 0.1)


(define (make-35-struct val1 val2 val3)
         (create-struct
          (list "number"
                "name"
                "position")
          (list val1 val2 val3)))
                         
(define 35a-struct (make-35-struct 928 (ptr) 4))
(define 35b-struct (make-35-struct 1089 (ptr) 3))
(define 35c-struct (make-35-struct 14 (ptr) 2))
(define 35d-struct (make-35-struct 546 (ptr) 1))
                    
(insert slide-35-canvas (struct-pict-getter 35a-struct) 0.05 0.35)
(insert slide-35-canvas (struct-pict-getter 35b-struct) 0.25 0.35)
(insert slide-35-canvas (struct-pict-getter 35c-struct) 0.55 0.35)
(insert slide-35-canvas (struct-pict-getter 35d-struct) 0.75 0.35)


(define alice (make-vec-from-vec (vector "A" "l" "i" "c" "e")))
(define bob (make-vec-from-vec (vector "B" "o" "b")))
(define carol (make-vec-from-vec (vector "C" "a" "r" "o" "l")))
(define dave (make-vec-from-vec (vector "D" "a" "v" "e")))

(insert slide-35-canvas 
        (vec-pict-getter alice #:show-indices? false)
        0.05 0.7)
(insert slide-35-canvas 
        (vec-pict-getter bob #:show-indices? false)
        0.3 0.85)
(insert slide-35-canvas 
        (vec-pict-getter carol #:show-indices? false)
        0.5 0.7)
(insert slide-35-canvas 
        (vec-pict-getter dave #:show-indices? false)
        0.75 0.85)

(point-x-to-y slide-35-canvas
              (struct-loc-getter 35a-struct #:field "name")
              (vec-loc-getter alice)
              #:from-find rc-find
              #:to-find ct-find
              #:start-angle 5.8
              #:start-pull 0.5
              #:end-angle 4)
(point-x-to-y slide-35-canvas
              (struct-loc-getter 35b-struct #:field "name")
              (vec-loc-getter bob)
              #:from-find rc-find
              #:to-find ct-find
              #:start-angle 5.8
              #:start-pull 0.5
              #:end-angle 4)
(point-x-to-y slide-35-canvas
              (struct-loc-getter 35c-struct #:field "name")
              (vec-loc-getter carol)
              #:from-find rc-find
              #:to-find ct-find
              #:start-angle 5.8
              #:start-pull 0.5
              #:end-angle 4)
(point-x-to-y slide-35-canvas
              (struct-loc-getter 35d-struct #:field "name")
              (vec-loc-getter dave)
              #:from-find rc-find
              #:to-find ct-find
              #:start-angle 5.8
              #:start-pull 0.5
              #:end-angle 4)
(point-x-to-y slide-35-canvas
              (vec-loc-getter slide-35-spine-vector #:content? #t #:index 0)
              (struct-loc-getter 35a-struct)
              #:from-find lc-find
              #:to-find ct-find
              #:start-angle 3.1
              #:end-angle 4)
(point-x-to-y slide-35-canvas
              (vec-loc-getter slide-35-spine-vector #:content? #t #:index 1)
              (struct-loc-getter 35b-struct)
              #:from-find cb-find
              #:to-find ct-find)
(point-x-to-y slide-35-canvas
              (vec-loc-getter slide-35-spine-vector #:content? #t #:index 2)
              (struct-loc-getter 35c-struct)
              #:from-find cb-find
              #:to-find ct-find)
(point-x-to-y slide-35-canvas
              (vec-loc-getter slide-35-spine-vector #:content? #t #:index 3)
              (struct-loc-getter 35d-struct)
              #:from-find rc-find
              #:to-find ct-find
              #:start-angle 0
              #:end-angle 5)


(canvas-final slide-35-canvas)

;Slide 38 - box identity and aliasing
(println "slide 38 demo")
(define slide-38-canvas (blank-canvas 300 200))

(define slide-38-vector (make-vec-from-vec (make-vector 5 (ptr))))
(insert slide-38-canvas 
        (vec-pict-getter slide-38-vector #:show-indices? false)
        0.3 0.2)



(define 38-1-struct (create-struct (list "x" "y")
                                   (list 16 1)))
(define 38-2-struct (create-struct (list "x" "y")
                                   (list 3 7)))
(define 38-3-struct (create-struct (list "x" "y")
                                   (list 4 9)))
(define 38-4-struct (create-struct (list "x" "y")
                                   (list 4 9)))
(insert slide-38-canvas
        (struct-pict-getter 38-1-struct)
        0.15 0.6)
(insert slide-38-canvas
        (struct-pict-getter 38-2-struct)
        0.35 0.6)
(insert slide-38-canvas
        (struct-pict-getter 38-3-struct)
        0.55 0.6)
(insert slide-38-canvas
        (struct-pict-getter 38-4-struct)
        0.75 0.6)

(point-x-to-y slide-38-canvas
              (vec-loc-getter slide-38-vector #:content? #t #:index 0)
              (struct-loc-getter 38-1-struct)
              #:to-find ct-find
              #:from-find cb-find)

(point-vec-to-struct slide-38-canvas slide-38-vector 1 38-2-struct 
                     #:to-find ct-find
                     #:from-find cb-find)
(point-vec-to-struct slide-38-canvas slide-38-vector 2 38-2-struct 
                     #:to-find ct-find
                     #:from-find cb-find)
(point-vec-to-struct slide-38-canvas slide-38-vector 3 38-3-struct 
                     #:to-find ct-find
                     #:from-find cb-find)
(point-vec-to-struct slide-38-canvas slide-38-vector 4 38-4-struct 
                     #:to-find ct-find
                     #:from-find cb-find)

(canvas-final slide-38-canvas)
