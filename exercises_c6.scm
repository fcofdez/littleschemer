(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;; According to the book, an arithmetic expression can be defined as
;; an atom or two arithmetic expressions combined by +, x or pow.
;; Numbered is an arithmetic expression that only contains numbers

(define arithmeticOperator?
  (lambda (op)
    (or (eq? op '+) (eq? op 'x) (eq? op 'p))))

(define numbered?
  (lambda (aexp)
    (innerNumbered? aexp #f)))

(define innerNumbered?
  (lambda (aexp prev)
    (cond
     ((atom? aexp) (number? aexp))
     ((null? aexp) #t)
     ((and (arithmeticOperator? (car aexp)) prev) (innerNumbered? (cdr aexp) #f))
     ((arithmeticOperator? (car aexp)) #f)
     (else (and (innerNumbered? (car aexp) #f) (innerNumbered? (cdr aexp) #t))))))
