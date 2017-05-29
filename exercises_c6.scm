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


;; Using church encodings instead of taking for granted that the aexpression is well formed
(define value
  (lambda (aexp)
    ((innerValue aexp))))

(define innerValue
  (lambda (aexp)
    (cond
     ((and (atom? aexp) (number? aexp))
      (lambda () aexp))
     ((null? aexp)
      (lambda (x) (x)))
     ((eq? (car aexp) '+)
      (lambda (x) (+ (x) ((value (cdr aexp))))))
     ((eq? (car aexp) '*)
      (lambda (x) (+ (x) ((value (cdr aexp))))))
     ((eq? (car aexp) 'p)
      (lambda (x) (exp (x) ((value (cdr aexp))))))
     (else
      (lambda () ((value (cdr aexp)) (value (car aexp))))))))

(define sero?
  (lambda (n) (null? n)))

(define add1
  (lambda (n)
    (cons () n)))

(define sub1
  (lambda (n)
    (cat n)))

(define addL
  (lambda (a b)
    (cond
     ((sero? b) a)
     (else (add1 (addL a (sub1 b))))))
