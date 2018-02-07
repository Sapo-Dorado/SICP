(load "stream-library.scm")

(define (weighted-merge s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
            (if (<= (weight s1car) (weight s2car))
                   (cons-stream
                    s1car
                    (weighted-merge (stream-cdr s1) s2 weight))
                  (cons-stream
                  s2car
                  (weighted-merge s1 (stream-cdr s2) weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (weighted-merge
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (divisible? x y)
  (= (remainder x y) 0))
(define (divisibility-check? x)
  (not (or (divisible? x 2) (or (divisible? x 3) (divisible? x 5)))))
(define selected-integers
  (stream-filter divisibility-check? integers))

(define a (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))
(define b (weighted-pairs selected-integers selected-integers (lambda (x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (* (car x) (cadr x)))))))

(show-stream b 20)
