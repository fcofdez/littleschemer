(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember-star
  (lambda (a l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond ((eq? (car l) a) (rember-star a (cdr l)))
            (else (cons (car l) (rember-star a (cdr l)))))
      (else (cons (rember-star a (car l)) (rember-star a (cdr l))))))))

(define insertR-star
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old
              (cons new (insertR-star new old
                                      (cdr l)))))
       (else (cons (car l) (insertR-star new old
                                         (cdr l)))))
      (else (cons (insertR-star new old
                                (car l))
                  (insertR-star new old
                                (cdr l))))))))

(define occur-star
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (+ 1 (occur-star a (cdr l))))
       (else (occur-star a (cdr l)))))
      (else (+ (occur-star a (car l))
               (occur-star a (cdr l)))))))


(define subst-star
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new (subst-star new old
                              (cdr l))))
       (else (cons (car l) (subst-star new old
                                       (cdr l)))))
      (else (cons (subst-star new old
                              (car l))
                  (subst-star new old
                              (cdr l))))))))

(define insertL-star
  (lambda (new old l)
    (cond
     ((null? l) ())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (cons old (insertR-star new old
                                      (cdr l)))))
       (else (cons (car l) (insertR-star new old
                                         (cdr l)))))
      (else (cons (insertR-star new old
                                (car l))
                  (insertR-star new old
                                (cdr l))))))))

(define member-star
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (cond
       ((eq? (car l) a)#t)
       (else (member-star a (cdr l)))))
     (else (or (member-star a (car l))
               (member-star a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (cond
            ((and (atom? (car l1)) (atom? (car l2)))
             (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
            ((or (atom? (car l1)) (atom? (car l2))) #f)
            (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2)) (eq? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist2? s1 s2)))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else (and
            (equal? (car l1) (car l2))
            (eqlist2? (cdr l1) (cdr l2)))))))
