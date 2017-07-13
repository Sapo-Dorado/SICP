(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
    (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (double procedure)
  (lambda (x) (procedure (procedure x))))

(define (inc x)
  (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f num)
  (define (iter ans count)
    (if (> 2 count)
        ans
        (iter (compose ans f) (- count 1))))
  (iter f num))

(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3)))

(define (iterative-improve good improve)
  (define (ans guess)
    (if (good guess)
        guess
        (ans (improve guess))))
  ans)

(define (fixed-point f first-guess)
  ((iterative-improve
   (lambda(guess) (< (abs(- (f guess) f)) .0001))
   (lambda(guess) (f guess)))
   first-guess))


