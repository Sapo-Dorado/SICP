;I didn't calculate for torque on the balanced one so that one is wrong
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch x)
  (car x))

(define (right-branch x)
  (car (cdr x)))

(define (branch-length x)
  (car x))

(define (branch-structure x)
  (car (cdr x)))

(define (weight branch)
  (let ((mobile (branch-structure branch)))
   (if (pair? mobile)
       (+ (weight (left-branch mobile)) (weight (right-branch  mobile)))
        mobile)))

(define (total-weight mobile)
   (+ (weight (left-branch mobile))
      (weight (right-branch mobile))))

(define a (make-branch 4 9))
(define b (make-branch 5 19))
(define c (make-branch 9 (make-mobile a b)))
(define d (make-branch 2 18))
(define e (make-branch 6 10))
(define f (make-branch 8 (make-mobile d e)))
(define real (make-mobile c f))

(define yes (make-mobile a b))

(total-weight yes)
