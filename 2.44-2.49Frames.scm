(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (let ((x1 (xcor-vect vect1))
        (y1 (ycor-vect vect1))
        (x2 (xcor-vect vect2))
        (y2 (ycor-vect vect2)))
    (cons (+ x1 x2) (+ y1 y2))))

(define (sub-vect vect1 vect2)
  (let ((x1 (xcor-vect vect1))
        (y1 (ycor-vect vect1))
        (x2 (xcor-vect vect2))
        (y2 (ycor-vect vect2)))
    (cons (- x1 x2) (- y1 y2))))

(define (scale-vect scale vect1)
  (let ((x1 (xcor-vect vect1))
        (y1 (ycor-vect vect1)))
    (cons (* x1 scale) (* y1 scale))))

(define a (make-vect 2 5))
(define b (make-vect 3 6))
(define c (make-vect 2 1))

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (car (cdr frame)))

(define (edge2 frame)
  (car (cdr (cdr frame))))

(define (make-segment vect1 vect2)
  (cons vect1 vect2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (frame-coord-map frame) (lambda (v)
    (add-vect
    (origin-frame frame)
    (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
              (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (segments->painter segment-list) (lambda (frame)
  (for-each
  (lambda (segment)
         (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
       segment-list)))

(define (outline frame)
  (let ((tr (make-vect 0.99 0.99))
        (tl (make-vect 0 0.99))
        (br (make-vect 0.99 0))
        (bl (make-vect 0 0)))
    (let ((s1 (make-segment tr tl))
          (s2 (make-segment tl bl))
          (s3 (make-segment bl br))
          (s4 (make-segment br tr)))
    ((segments->painter (list s1 s2 s3 s4)) frame))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame))) 
      (let ((new-origin (m origin)))
            (painter (make-frame
                      new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
    (make-vect 0.0 1.0)  
    (make-vect 1.0 1.0) 
    (make-vect 0.0 0.0))) 
