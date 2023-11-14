#lang sicp

(define eqan?
  (lambda (a1 a2)
    (cond ((and (number? a1) (number? a2)) (= a1 a2))
          ((or (number? a1) (number? a2)) false)
          (else (eq? a1 a2)))))

(define multirember
  (lambda (a lat)
    (cond ((null? lat) '())
          ((equal? a (car lat)) (multirember a (cdr lat)))
          (else (cons (car lat)
                      (multirember a (cdr lat)))))))



(define multiinsertR
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
          (else (cons (car lat)
                      (multiinsertR new old (cdr lat)))))))


(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) lat)
          ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
          (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define atom?
  (lambda (sexp)
    (and (not (list? sexp))
         (not (null? sexp)))))

(define add1 (lambda (number)
               (+ number 1)))
(define sub1 (lambda (number)
               (- number 1)))

(define addtup
  (lambda (tup)
    (cond ((null? tup) 0)
          (else (+ (car tup) (addtup (cdr tup)))))))

(define rember*
  (lambda (a l)
    (cond ((null? l) '())
          ((and (atom? (car l)) (eq? a (car l))) (rember* a (cdr l)))
          ((atom? (car l)) (cons (car l) (rember* a (cdr l))))
          (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond ((null? l) '())
          ((and (atom? (car l))
                (eq? old (car l))) (cons (car l)
                                         (cons new (insertR* new old (cdr l)))))
          ((atom? (car l)) (cons (car l) (insertR* new old (cdr l))))
          (else (cons (insertR* new old (car l))
                      (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((and (atom? (car l)) (eq? (car l) a)) (add1 (occur* a (cdr l))))
          ((atom? (car l)) (occur* a (cdr l)))
          (else (+ (occur* a (car l))
                   (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
          ((and (atom? (car l))
                (eq? (car l) old)) (cons new (subst* new old (cdr l))))
          ((atom? (car l)) (cons (car l) (subst* new old (cdr l))))
          (else (cons (subst* new old (car l))
                      (subst* new old (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) (car l))
          (else (leftmost (car l))))))


(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) true)
          ((or (null? l1) (null? l2)) false)
          (else (and (equal? (car l1) (car l2))
                     (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
          ((or (atom? s1) (atom? s2)) false)
          (else (eqlist? s1 s2)))))


(define rember
  (lambda (s l)
    (cond ((null? l) '())
          ((equal? (car l) s) (cdr l))
          (else (cons (car l) (rember s (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond ((atom? aexp) (number? aexp))
          (else (and (numbered? (car aexp))
                     (numbered? (car (cddr aexp))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

;(define value
;  (lambda (nexp)
;    (cond ((number? nexp) nexp)
;          (else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp))
;                                                    (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

;;(define +
;;  (lambda (n m)
;;    (cond ((sero? m) n)
;;          (else (+ (edd1 n) (zub1 m))))))

(define member?
  (lambda (a lat)
    (cond ((null? lat) false)
          ((equal? (car lat) a) true)
          (else (member? a (cdr lat))))))

(define set?
  (lambda (lat)
    (cond ((null? lat) true)
          ((member? (car lat) (cdr lat)) false)
          (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond ((null? lat) '())
          (else (cons (car lat)
                      (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond ((null? set1) true)
          (else (and (member? (car set1) set2)
                     (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond ((null? set1) false)
          ((member? (car set1) set2) true)
          (else (intersect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2) (union (cdr set1) set2))
          (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (l)
    (cond ((atom? l) false)
          ((null? l) false)
          ((null? (cdr l)) false)
          ((null? (cdr (cdr l))) true)
          (else false))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define firsts
  (lambda (l)
    (cond ((null? l) '())
          (else (cons (car (car l)) (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond ((null? rel) '())
          (else (cons (build (second (car rel))
                             (first (car rel)))
                      (revrel (cdr rel)))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond ((null? l) '())
            ((test? a (car l)) (cdr l))
            (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old lat)
      (cond ((null? lat) '())
            ((test? (car lat) old) (cons new (cons (car lat)
                                                   (cdr lat))))
            (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond ((null? l) '())
            ((test? (car l) old) (cons old (cons new (cdr l))))
            (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond ((null? lat) '())
            ((eq? (car lat) old) (seq new old (cdr lat)))
            (else (cons (car lat) ((insert-g seq) new old (cdr lat))))))))

(define insertL
  (insert-g seqL))

(define seqBuild
  (lambda (new l)
    (cons new l)))

(define subst
  (insert-g seqBuild))

(define atom-to-function
  (lambda (op)
    (cond ((eq? op '+) +)
          (else *))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond ((null? lat) '())
            ((test? (car lat) a) ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define multiremberT
  (lambda (test? lat)
    (cond ((null? lat) '())
          ((test? (car lat)) (multiremberT test? (cdr lat)))
          (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(define multirember-eq?
  (multirember-f eq?))


(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          (else (cond ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
                      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
                      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() 0 0))
          ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat leftn rightn)
                                                                            (col (cons new (cons oldL newlat))
                                                                                 (add1 leftn)
                                                                                 rightn))))
          ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat leftn rightn)
                                                                            (col (cons oldR (cons new newlat))
                                                                                 leftn
                                                                                 (add1 rightn)))))
          (else (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat leftn rightn)
                                                            (col (cons (car lat) newlat)
                                                                 leftn
                                                                 rightn)))))))

(define evens-only*
  (lambda (l)
    (cond ((null? l) '())
          ((atom? (car l)) (cond ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))


(define evens-only*&co
  (lambda (l col)
    (cond ((null? l) (col '() 1 0))
          ((atom? (car l)) (cond ((even? (car l)) (evens-only*&co (cdr l) (lambda (rest-evenl pr sum)
                                                                            (col (cons (car l) rest-evenl)
                                                                                 (* (car l) pr)
                                                                                 sum))))
                                 (else (evens-only*&co (cdr l) (lambda (rest-evenl pr sum)
                                                                 (col rest-evenl
                                                                      pr
                                                                      (+ (car l) sum)))))))
          (else (evens-only*&co (car l) (lambda (cars-evl carp cars)
                                          (evens-only*&co (cdr l) (lambda (cdr-evl cdrp cdrs)
                                                                    (col (cons cars-evl cdr-evl)
                                                                         (* carp cdrp)
                                                                         (+ cars cdrs))))))))))

(define keep-looking
  (lambda (a which lat)
    (cond ((number? which) (keep-looking a (pick which lat) lat))
          ((eq? which a) true)
          (else false))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))


(define pick
  (lambda (i lat)
    (cond ((= i 1) (car lat))
          (else (pick (- i 1) (cdr lat))))))


(define shift
  (lambda (x)
    (build (first (first x))
           (build (second (first x))
                  (second x)))))

(define length*
  (lambda (l)
    (cond ((null? l) 0)
          ((atom? (car l)) (add1 (length* (cdr l))))
          (else (+ (length* (car l))
                   (length* (cdr l)))))))

(define eternity
  (lambda (x) (eternity x)))

(define new-entry build)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name (first entry) (second entry) entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond ((null? names) (entry-f name))
          ((eq? (car names) name) (car values))
          (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond ((null? table) (table-f name))
          (else (lookup-in-entry name (car table) (lambda (name)
                                                    (lookup-in-table name (cdr table) table-f)))))))

;(define expression-to-action
;  (lambda (e)
;    (cond ((atom? e) (atom-to-action e))
;          (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond ((number? e) *const)
          ((eq? e true) *const)
          ((eq? e false) *const)
          ((eq? e 'cons) *const)
          ((eq? e 'car) *const)
          ((eq? e 'cdr) *const)
          ((eq? e 'null?) *const)
          ((eq? e 'eq?) *const)
          ((eq? e 'atom?) *const)
          ((eq? e 'zero?) *const)
          ((eq? e 'add1) *const)
          ((eq? e 'sub1) *const)
          ((eq? e 'number?) *const)
          (else *identifier))))

;;(define list-to-action
;;  (lambda (e)
;    (cond ((atom? (car e)) (cond ((eq? (car e) 'lambda) *lambda)
;                                 ((eq? (car e) 'quote) *quote)
;                                 ((eq? (car e) 'cond) *cond)
;                                 (else *application))
;                           (else *application)))))

;(define value
;  (lambda (e)
;;    (meaning e '())))

;(define meaning
;  (lambda (exp table)
;    ((expression-to-action exp) exp table)))

(define *const
  (lambda (e table)
    (cond ((number? e) e)
          ((eq? e 'true) true)
          ((eq? e 'false) false)
          (else (build 'primitive e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
    (car '())))

(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))

(define table-of
  (lambda (lambda-env)
    (car (car (cdr lambda-env)))))


(define formals-of
  (lambda (lambda-env)
    (car (cdr (car (cdr lambda-env))))))





(define else?
  (lambda (quest)
    (cond ((atom? quest) (eq? quest 'else))
          (else false))))

(define question-of first)
(define answer-of second)


(define primitive?
  (lambda (func)
    (eq? (car func) 'primitive)))

(define non-primitive?
  (lambda (func)
    (eq? (car func) 'non-primitive)))

(atom? +)