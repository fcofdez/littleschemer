(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((equal? a (car l)) #t)
     (else (member? a (cdr l))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f test?) a (cdr lat))))))))

(define multirember
  (multirember-f equal?))

(define atom?
  (lambda (atom)
    (and (not (pair? x)) (not (null? x)))))
