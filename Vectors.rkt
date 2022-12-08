#lang racket
(require ppict/2)
(require pict)
(require racket/contract)
(require racket/struct)

(require "Basic_definitions.rkt")
(provide (all-defined-out))


(define (a-rect) (rectangle 20 30 #:border-width 2))


(struct vect
  (len content-value pic-contents boxes final-no-indices final)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (obj) 'vector-struct)
      (λ (obj) (list (vect-len obj)
                     (vect-content-value obj)
                     (vect-pic-contents obj)
                     (vect-boxes obj)
                     (vect-final-no-indices obj)
                     (vect-final obj)))))])


(define make-vec
  (λ (len [default-content null])
    (define result (vect len ;length of vector
                         (make-vector len default-content)  ;holds the actual values
                         (make-vector len (a-rect)) ;holds the picture in the box
                         (make-vector len) ;holds the boxes
                         (blank 0) ;holds the final pic without indices
                         (blank 0)  ;;holds the final pic
                         ))

    (redraw-vec result)
    result
    ))

(define (make-vec-from-vec vector)
  (define len (vector-length vector))
  (define result (vect len ;length of vector
                       vector  ;holds the actual values
                       (make-vector len (a-rect)) ;holds the picture in the box
                       (make-vector len) ;holds the boxes
                       (blank 0) ;holds the final pic without indices
                       (blank 0)  ;;holds the final pic
                       ))
  
  (redraw-vec result)
  result
  )

(define (set-vec-cell vec index content)
  (vector-set! (vect-content-value vec) index content)
  (redraw-vec vec)
  )


(define (vec-pict-getter vec
                          #:show-indices? [show-indices? #t])
  (if show-indices?
      (vector vec (vect-final vec))
      (vector vec (vect-final-no-indices vec)))
  )

(define (vec-loc-getter vec
                         #:content? [content? #f]
                         #:index [index #f]
                         )
  (cond
        [content?
         (if index
             (vector vec (vector-ref (vect-pic-contents vec) index))
             (error "please specify index - which content do you want to point from?"))]
        [else
         (if index
             (vector vec (vector-ref (vect-boxes vec) index))
             (vector vec (vect-final-no-indices vec)))]
        ))


(define (redraw-vec vec)
  (define final-pic (blank 0))
  (define final-pic-no-indices (blank 0))
  (define final-indices (blank 0))
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
        (set! width (+ 6 (pict-width content)))
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
    (define filled-box-index
      (vl-append (text (~a i) null 10)
                 (blank 2)
                 filled-box))
    (set! final-pic (hc-append final-pic filled-box-index))
    (set! final-pic-no-indices (hc-append final-pic-no-indices
                                          filled-box))
    
    )
  (set-vect-final-no-indices! vec final-pic-no-indices)
  (set-vect-final! vec (pin-over final-pic
                                 (vector-ref (vect-boxes vec)
                                             0)
                                 lt-find
                                 final-pic-no-indices))
#|
  (set-vect-final! vec (pin-arrow-line 30 (vect-final vec)
                                       final-pic-no-indices
                                       lt-find
                                       final-pic
                                       ct-find))
|#
  (vect-final vec))




#|
(define vecnull (make-vec 10))
(vect-pic-contents vecnull)
(vect-content-value vecnull)

(println vecnull)
(draw-vec vecnull)

(define v (make-vec 10 0))
(println "this is a")
(draw-vec v)

(println "setting index 3 to null")
(set-vec-cell v 3 null)
(println "setting index 4 to null-ptr")
(set-vec-cell v 4 (null-ptr))
(println "setting: testing automatic expanding of cell")
(set-vec-cell v 7 123455)
(println "da here")

(define da (redraw-vec v))
da

(println "making vector pic from vector")
(define actual-vector (vector 1 2 3 4 5))
(define made-from-vector (make-vec-from-vec actual-vector))
made-from-vector
|#
