#lang racket
(require ppict/2)
(require pict)
(require racket/contract)
(require racket/struct)

(require "Basic_definitions.rkt")
(provide (all-defined-out))


(define (a-rect) (rectangle 20 30 #:border-width 2))


(struct vect
  (len content-value pic-contents boxes final)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'vector-struct)
      (λ (obj) (list (vect-len obj)
                     (vect-content-value obj)
                     (vect-pic-contents obj)
                     (vect-boxes obj)
                     (vect-final obj)))))])


(define make-vec
  (λ (len [default-content null])
    (define result (vect len ;length of vector
                         (make-vector len default-content)  ;holds the actual values
                         (make-vector len (a-rect)) ;holds the picture in the box
                         (make-vector len) ;holds the boxes
                         (blank 0)  ;;holds the final pic
                         ))

    (redraw-vec result)
    result
    ))

(define (set-vec-cell vec index content)
  (vector-set! (vect-content-value vec) index content)
  (redraw-vec vec)
  )


(define (draw-vec vec)
  (vect-final vec))

(define (redraw-vec vec)
  (define final-pic (blank 0))
    (for ([i (in-range (vect-len vec))])
      (define real-content (vector-ref (vect-content-value vec) i) )
      (define content
        (cond
          [(or (number? real-content) (string? real-content))
           (text (~a real-content))]
          [(null? real-content) (blank 0)]
          [(null-ptr? real-content) (null-ptr-pic)]
          [else (ptr-base)]
          )
        )
      (define width 20)
      (if (< 20 (pict-width content))
          (set! width (+ 3 (pict-width content)))
          (void))
    
      (define box (rectangle width 30 #:border-width 2))
      (vector-set! (vect-pic-contents vec)
                   i
                   content)
      (vector-set! (vect-boxes vec)
                   i
                   box)
      (define filled-box
        (ppict-do box
                  #:go (coord 0.5 0.5 'cc)
                  content))
      (set! final-pic (hc-append final-pic filled-box))    

      )
    (set-vect-final! vec final-pic)
    (vect-final vec))



#|
(define vecnull (make-vec 10))
(vect-pic-contents vecnull)
(vect-content-value vecnull)

(println vecnull)
(draw-vec vecnull)

(define a (make-vec 10 0))
(println "this is a")
(draw-vec a)

(println "setting")
(set-vec-cell a 3 null)


(set-vec-cell a 4 (null-ptr))
;(set-vec-cell a 7 123455)
(draw-vec a)
(println "da here")

(define da (redraw-vec a))

|#

