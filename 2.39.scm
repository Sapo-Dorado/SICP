(define nil ())

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (yes sequence)
   (fold-left (lambda (x y) (cons y x)) nil sequence))
(reverse (list 1 2 3 4))
