(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (queens board-size)
  (define empty-board
    ())

  (define (adjoin-position row k seq)
    (cons (list k row) seq))

  (define (safe? k positions)
    (define k-row (car (cdr (car positions))))
    (define (safe seq)
      (if (null? seq)
        #t
        (let ((row (car (cdr (car seq))))
             (col (car (car seq))))
             (if (or (= row k-row) (= (abs (- col k)) (abs (- row k-row))))
              #f
              (safe (cdr seq))))))
    (safe (cdr positions)))

  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap
                        (lambda (rest-of-queens)
                          (map (lambda (new-row)
                                 (adjoin-position new-row k rest-of-queens))
                               (enumerate-interval 1 board-size)))
                    (queen-cols (- k 1))))))
  (queen-cols board-size))

(accumulate (lambda (x y) (+ 1 y)) 0 (queens 8))
