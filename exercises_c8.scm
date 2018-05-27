(load "common.scm")

(define remberr
  (lambda (l testfn e)
    (cond
     ((null? l) '())
     ((testfn (car l) e) (cdr l))
     (else (cons (car l) (remberr testfn (cdr l) e))))))

(define rember-f
  (lambda (testfn)
    (lambda (l e)
      (cond
       ((null? l) '())
       ((testfn (car l) e) (cdr l))
       (else (cons (car l) ((rember-f testfn) (cdr l) e)))))))

(define insertL
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons new (cons old (cdr l))))
       (else (cons (car l)
                   ((insertL test?) new old (cdr l))))))))

(define insertR
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (cons old (cons new (cdr l))))
       (else (cons (car l)
                   ((insertR test?) new old (cdr l))))))))

(define seqL
  (lambda (a b c)
    (cons a (cons b c))))

(define seqR
  (lambda (a b c)
    (cons b (cons a c))))

(define insertG
  (lambda (test? insertFn)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? (car l) old)
        (insertFn new old (cdr l)))
       (else (cons (car l)
                   ((insertG test? insertFn) new old (cdr l))))))))

(define insertLL (insertG eq? seqL))
(define insertRR (insertG eq? seqR))

(define insertLLL (insertG eq? (lambda (a b c)
                                 (cons a (cons b c)))))
(define insertRRR (insertG eq? (lambda (a b c)
                                 (cons b (cons a c)))))

(define subst (insertG eq? (lambda (a b c)
                             (cons a c))))

(define atom-to-function
  (lambda (at)
    (cond
     ((eq? at '+) +)
     ((eq? at 'x) x)
     (else +))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) atom)
     (else ((atom-to-function (operator nexp))
            (value (first-subexp nexp))
            (value (second-subexp nexp)))))))

(define multirember-f
  (lambda (testfn)
    (lambda (l e)
      (cond
       ((null? l)
        '())
       ((testfn (car l) e)
        ((multirember-f testfn) (cdr l)))
       (else
        (cons (car l)
              ((multirember-f testfn) (cdr l) e)))))))

(define multirember-f-eq?
  (multirember-f eq?))


(define multirember-f-co
  (lambda (testfn l)
    (cond
     ((null? l)
      '())
     ((testfn (car l))
      (multirember-f-co testfn (cdr l)))
     (else
      (cons (car l)
            (multirember-f-co testfn (cdr l)))))))


(define multirember-f-coand
  (lambda (testfn l col)
    (cond
     ((null? l)
      (col '() '()))
     ((testfn (car l))
      (multirember-f-coand testfn
                           (cdr lat)
                           (lambda (newlat seen)
                             (col newlat
                                  (cons (car l) seen)))))
     (else
      (multirember-f-co testfn
                        (cdr lat)
                        (lambda (newlat seen)
                          (col (cons (car l) newlat)
                               seen)))))))

(define multiinsertL
  (lambda (old new lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new
            (cons old
                  (multiinsertL old new
                                (cdr lat))))
      (else (cons (car lat)
                  (multiinsertL old new
                                (cdr lat))))))))

(define multiinsertR
  (lambda (old new lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons old
            (cons new
                  (multiinsertR old new
                                (cdr lat))))
      (else (cons (car lat)
                  (multiinsertR old new
                                (cdr lat))))))))

;; inserts new to the left of oldL and to the right of oldR in lat if oldL and
;; oldR are different
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multiinsertLR new oldL oldR
                                 (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multiinsertLR new oldL oldR
                                 (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertLR new oldL oldR
                                (cdr lat)))))))

(define multiinsertLRCol
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLRCol new oldL oldR (cdr lat)
                        (lambda (newlat lins rins)
                          (col (cons new
                                     (cons oldL newlat))
                               (+ lins 1) rins))))
     ((eq? (car lat) oldR)
      (multiinsertLRCol new oldL oldR (cdr lat)
                        (lambda (newlat lins rins)
                          (col (cons oldR
                                     (cons new newlat))
                               lins (+ rins 1)))))
     (else
      (multiinsertLRCol new oldL oldR (cdr lat)
                        (lambda (newlat lins rins)
                          (col (cons (car lat) newlat)
                               lins rins)))))))

(define evens-only-star
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((atom? (car lat))
      (cond
       ((even? (car lat))
        (cons (car lat) (evens-only-star (cdr lat))))
       (else (evens-only-star (cdr lat)))))
     (else (cons (evens-only-star (car lat))
                 (evens-only-star (cdr lat)))))))

(define evens-only-star-collector
  (lambda (lat col)
    (cond
     ((null? lat) (col '() 1 0))
     ((atom? (car lat))
      (cond
       ((even? (car lat))
        (evens-only-star-collector (cdr lat)
                                   (lambda (newlat e o)
                                     (col
                                      (cons (car lat) newlat)
                                      (* (car lat) e)
                                      o))))
       (else
        (evens-only-star-collector (cdr lat)
                                   (lambda (newlat e o)
                                     (col
                                      newlat
                                      e
                                      (+ (car lat) o)))))))
     (else (evens-only-star-collector (car lat)
                                      (lambda (newlat e o)
                                        (evens-only-star-collector (cdr lat)
                                                                   (lambda (l ee oo)
                                                                     (col
                                                                      (cons newlat l)
                                                                      (* ee e)
                                                                      (+ o oo))))))))))

(define lastfriend
  (lambda ( nl r o )
            (cons o
                (cons r
                      nl))))
