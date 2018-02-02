(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(define (display-stream s) (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high) (if (> low high)
      the-empty-stream
      (cons-stream
        low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define ones 
  (cons-stream 1 ones))
(define integers
  (cons-stream
    1
    (add-streams ones integers)))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
          (cond ((< s1car s2car)
                 (cons-stream
                            s1car
                            (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                              s2car
                              (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream
                                s1car
                                (merge (stream-cdr s1)
                                       (stream-cdr s2)))))))))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
  stream))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
  (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

(define (show-stream s num)
  (if (> num 0)
    (begin
      (display-line (stream-car s))
      (show-stream (stream-cdr s) (- num 1)))))

