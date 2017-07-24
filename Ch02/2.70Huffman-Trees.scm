(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree)) 

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree) (if (leaf? tree)
                              (weight-leaf tree)
                                    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                                    (decode-1 (cdr bits) tree))
                        (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set) (cond ((null? set) (list x))
                                 ((< (weight x) (weight (car set))) (cons x set))
                                 (else (cons (car set)
                                             (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)
      (cadr pair))
      (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (define (symbol-in-set? symbol set)
    (cond ((null? (cdr set)) (eq? symbol (car set)))
          ((eq? symbol (car set)) #t)
          (else (symbol-in-set? symbol (cdr set)))))

  (define (encode-symbol symbol tree)
    (cond ((leaf? tree) '())
          ((symbol-in-set? symbol (symbols (right-branch tree))) (cons 1 (encode-symbol symbol (right-branch tree))))
           ((symbol-in-set? symbol (symbols (left-branch tree))) (cons 0 (encode-symbol symbol (left-branch tree))))
            (else (error: "invalid symbol :" symbol))))

  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
      (encode (cdr message) tree))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (if (null? (cdr pairs))
     (car pairs)
     (let ((tree (make-code-tree (car pairs) (cadr pairs)))
           (rest-of-tree (if (null? (cddr pairs))
                           '()
                           (cddr pairs))))
         (successive-merge (adjoin-set tree rest-of-tree)))))


(define rock-tree (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(length (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) rock-tree))


;the huffman tree encodes the message in 84 bits while the fixed length code would encode it in 144 bits
