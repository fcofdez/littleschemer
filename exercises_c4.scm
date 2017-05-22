;; Chapter 4
(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (m n)
    (cond
     ((zero? m) n)
     (else (add1 (o+ (sub1 m) n))))))

(define o*
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (o* n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup) (addtup (cdr tup)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((and (null? tup1) (null? tup2)) ())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))


(define greater
  (lambda (m n)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (great (- m 1) (- n 1))))))

(define less
  (lambda (m n)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else
      (less (- m 1) (- n 1))))))

(define eq?
  (lambda (m n)
    (cond
     ((and (not (greater m n)) (not (less m n))) #t)
     (else #f))))

(define eqq?
  (lambda (m n)
    (cond
     ((greater m n) #f)
     ((less m n) #f)
     (else #t))))

(define pow
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (* n (pow n (- m 1)))))))

(define length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (+ 1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (- n 1)) (car lat))
     (else (pick (- n 1) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (- n 1)) (cdr lat))
     (else (cons (car lat) (rempick (- n 1) (cdr lat)))))))

(define number?
  (lambda (n)
    (cond
     ((atom?)))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) ())
     ((number? (car lat))
      (no-nums (cdr lat)))
     (else
      (cons (car lat)
            (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) ())
     ((number? (car lat))
      (cons (car lat) (all-nums (cdr lat))))
     (else
      (all-nums (cdr lat))))))

(define eqan?
  (lambda (a b)
    (cond
     ((and (number? a) (number? b))
      (= a b))
     ((or (number? a) (number? b))
      #f)
     (else (eq? a b)))))

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))

(define one?
  (lambda (n)
    (eqan? n 1)))

(define rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (rempick (- n 1) (cdr lat)))))))
