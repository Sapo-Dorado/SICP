(load "stream-library.scm")
;;this was my answer I think it works but the other one seems to be the consensus of everyone else
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (pairs (stream-cdr s) t))))

(define (pairs2 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (interleave 
     (stream-map (lambda (x) 
                  (list x (stream-car t)))
                 (stream-cdr s))
     (pairs2 (stream-cdr s) (stream-cdr t))))))

(define a (pairs integers integers))
(define b (pairs2 integers integers))

(show-stream a 20)
(show-stream b 20)
