(define (make-table)

  (define placeholder '())

  (define (assoc key records)
    (let ((records (cdr records)))
      (cond ((null? records) false)
            ((equal? key (caar records)) (car records))
            (else (assoc key records)))))

  (let ((local-table (list 'table*)))
    (define (lookup keys table)
      (if (null? table)
        #f
        (if (null? keys)
          (if (null? (car table))
            #f
            (car table))
          (let ((sub-table (assoc (car keys) table)))
            (if sub-table
              (lookup (cdr keys) (cdr sub-table))
              #f)))))

    (define (insert! keys value table)

      (define (create-branch keys value table)
        (define (iter key-list)
          (if (null? (cdr key-list))
            (list (car key-list) value)
            (cons (car key-list) (list placeholder (iter (cdr key-list))))))
        (if (null? keys)
          (set-cdr! table (list value))
          (let ((new-branch (iter keys)))
            (if (null? (cdr table))
              (set-cdr! table (cons placeholder (list new-branch)))
              (set-cdr! (cdr table) (cons new-branch (cddr table)))))))

      (define (iter keys table)
        (if (null? keys)
          (set-car! table value)
          (let ((sub-table (assoc (car keys) table)))
            (if sub-table
              (if (null? (cddr sub-table))
                (create-branch (cdr keys) value sub-table)
                (iter (cdr keys) (cdr sub-table)))
              (create-branch keys value table)))))


      (if (or (null? table) (null? (cdr table)))
        (create-branch keys value local-table)
        (iter keys table))
      (print-table))

    (define (print-table)
      local-table)

    (define (dispatch m)
      (cond ((eq? m 'lookup) (lambda (keys) (lookup keys (cdr local-table))))
            ((eq? m 'insert!) (lambda (keys value) (insert! keys value (cdr local-table))))
            ((eq? m 'print) (print-table))
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define table (make-table))

((table 'insert!) '(a b) 'd)
((table 'insert!) '(a b c) 'r)
((table 'insert!) '(b a) 'j)
((table 'insert!) '(b a g) 'h)
((table 'insert!) '() 'k)
((table 'lookup) '(a b))
((table 'lookup) '(a b c))
((table 'lookup) '(b a))
((table 'lookup) '(b a g))
((table 'lookup) '())


