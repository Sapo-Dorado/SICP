(load "stream-library.scm") 
;; gj Louis
(define (pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) t)
    (pairs (stream-cdr s) (stream-cdr t))))

(define a (pairs integers integers))

(show-stream a 20)

