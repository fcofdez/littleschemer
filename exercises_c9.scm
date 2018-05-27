(load "common.scm")

;; Chapter about total / partial functions
;; and y combinator

(define pick-n
  (lambda (n cur lat)
    (cond
     ((null? lat) '())
     ((eq? n cur) (car lat))
     (else (pick-n n (+ 1 cur) (cdr lat))))))

(define pick
  (lambda (n lat)
    (pick-n n 0 lat)))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 0 lat) lat)))

(define keep-looking
  (lambda (a at lat)
    (cond
     ((number? at)
      (keep-looking a (pick at lat) lat))
     (else (eq? a at)))))

(define eternity
  (lambda (x)
    (eternity x)))


(define nested-head
  (lambda (l)
    (car (car l))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

;; ((a b) c) -> (a (b c))
;; ((a b) (c d)) -> (a (b (c d)))
(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))

(define length-*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else (+ (length-* (first pora))
              (length-* (second pora)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((pair? (first pora))
      (shuffle (revpair pora)))
     (else (build (first pora)
                  (shuffle (second pora)))))))

((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (+ 1 (length (cdr l)))))))
eternity)


((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (+ 1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (+ 1 (length (cdr l)))))))
  eternity))

;; y combinator
((lambda (le)
   ((lambda (f)
      (f f))
    (lambda (f)
      (le
       (lambda (x) ((f f) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (+ 1 (length
                  (cdr l))))))))
