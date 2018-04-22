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
     ((member? (car s1) s2) (diff (cdr s1) s2))
     (else (cons (car s1) (diff (cdr s1) s2))))))

(define intersect-all
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
                      (intersect-all (cdr l-set)))))))

(define a-pair?
  (lambda (p-set)
    (cond
     ((null? p-set) #f)
     ((atom? p-set) #f)
     ((null? (car p-set)) #f)
     ((null? (cdr (cdr p-set) #t)))
     (else #f))))

(define first
  (lambda (pair)
    (car pair)))

(define second
  (lambda (pair)
    (car (cdr pair))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build-pair
  (lambda (a b)
    (cons a (cons b '()))))

(define fn?
  (lambda (rel))
  (set? (firsts rel)))

(define reverse-pair
  (lambda (pair)
    (build-pair ((second pair) (first pair)))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (reverse-pair (car rel))
                 (revrel (cdr rel)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l)) (firsts (cdr l)))))))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(define fullfun?
  (lambda (rel))
  (set? (seconds rel)))

(define one-to-one?
  (lambda (rel))
  (fn? (revrel rel)))
