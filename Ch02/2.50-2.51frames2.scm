#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))

(define wave (segments->painter wave-segments))
(define (sub-vect vect1 vect2)
  (let ((x1 (xcor-vect vect1))
        (y1 (ycor-vect vect1))
        (x2 (xcor-vect vect2))
        (y2 (ycor-vect vect2)))
    (cons (- x1 x2) (- y1 y2))))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))
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

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))


(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-top (transform-painter painter1
                                        split-point
                                        (make-vect 1.0 0.5)
					(make-vect 0.0 1.0)))
	  (paint-bottom (transform-painter painter2
					   (make-vect 0.0 0.0)
					   (make-vect 1.0 0.0)
					   split-point)))
      (lambda (frame)
	(paint-top frame)
	(paint-bottom frame)))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
  (let ((paint-left (transform-painter
                    painter1
                    (make-vect 0.0 0.0)
                    split-point
                    (make-vect 0.0 1.0)))
        (paint-right (transform-painter
                     painter2
                     split-point
                     (make-vect 1.0 0.0)
                     (make-vect 0.5 1.0))))(lambda (frame) (paint-left frame) (paint-right frame)))))

(define (alternate-below painter1 painter2)
  (let ((top (rotate270 painter1))
	(bottom (rotate270 painter2)))
    (rotate90 (beside bottom top))))

(paint (alternate-below einstein wave))
