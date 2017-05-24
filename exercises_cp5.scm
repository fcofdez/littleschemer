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
     (else (leftmost (car l)))))
