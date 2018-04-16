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

(define subset?
  (lambda (s1 s2)
    (cond
     ((null? s1) #t)
     ((member? (car s1) s2) (subset? (cdr s1) s2))
     (else #f))))


(define eqset?
  (lambda (s1 s2)
    (and (subset? s1 s2)
         (subset? s2 s1))))

(define eqset?
  (lambda (s1 s2)
    (cond
     ((and (null? s1) (null? s2)) #t)
     ((member? (car s1) s2) (subset? (cdr s1) s2))
     (else #f))))

(define subset-and?
  (lambda (s1 s2)
    (cond
     ((null? s1) #t)
     (else (and (member? (car s1) s2)
                (subset? (cdr s1) s2))))))

(define intersect?
  (lambda (s1 s2)
    (cond
     ((null? s1) #f)
     (else (or (member? (car s1) s2)
               (intersect? (cdr s1) s2))))))

(define intersect
  (lambda (s1 s2)
    (cond
     ((null? s1) '())
     ((member? (car s1) s2) (cons (car s1)
                                  (intersect (cdr s1) s2)))
     (else (intersect (cdr s1) s2)))))

(define union
  (lambda (s1 s2)
    (cond
     ((null? s1) s2)
     ((member? (car s1) s2) (union (cdr s1) s2))
     (else (cons (car s1) (union (cdr s1) s2))))))

(define diff
  (lambda (s1 s2)
    (cond
     ((null? s1) '())
     ((member? (car s1) s2) (union (cdr s1) s2))
     (else (cons (car s1) (union (cdr s1) s2))))))
