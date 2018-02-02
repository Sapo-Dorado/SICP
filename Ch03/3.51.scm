;;this is messed up idk why
(load "stream-library.scm")

(define (show x)
  (display-line x)
 x )

(define x 
 (stream-map show
             (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)

(show x)
; (define y (cons-stream (show 2) (cons-stream (show 3) the-empty-stream)))
; 
; (display-line (stream-car (stream-cdr y)))
