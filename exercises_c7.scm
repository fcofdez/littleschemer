(load "common.scm")

(define set?
  (lambda (s)
    (cond
     ((null? s) #t)
     ((member? (car s) (cdr s)) #f)
     (else (set? (cdr s))))))

(define makeset
  (lambda (s)
    (cond
     ((null? s) '())
     ((member? (car s) (cdr s)) (makeset2 (cdr s)))
     (else (cons (car s)
                 (makeset2 (cdr s)))))))

(define makesetrember
  (lambda (s)
    (cond
     ((null? s) '())
     (else (cons (car s) (makesetrember (multirember (car s) (cdr s))))))))
