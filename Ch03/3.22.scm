(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "front called with empty queue" queue)
        front-ptr))
    (define (return-queue)
      front-ptr)
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?) (set-front-ptr! new-pair) (set-rear-ptr! new-pair) (return-queue))
              (else (set-cdr! rear-ptr new-pair) (set-rear-ptr! new-pair) (return-queue)))))
    (define (delete-queue!)
      (cond ((empty-queue?) (error "DELETE! called with an empty  queue" queue))
            (else (set-front-ptr! (cdr front-ptr)) (return-queue))))

    (define (dispatch m)
      (cond ((eq? m 'insert) insert-queue!)
            ((eq? m 'delete) delete-queue!)
            ((eq? m 'check) (return-queue))
            (else (error "invalid message" m))))
      dispatch))


(define (insert-queue! queue item)
  ((queue 'insert) item))

(define (delete-queue! queue)
  ((queue 'delete)))

(define (display-queue queue)
  (queue 'check))

(define a (make-queue))

(insert-queue! a 'a)
(insert-queue! a 'b)
(insert-queue! a 'c)
(insert-queue! a 'd)
(delete-queue! a)
(display-queue a)
