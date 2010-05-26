;----{ Misc Functions }------------------------
(define index
  (lambda (n lat)
    (cond
      ((null? lat) lat)
      ((eq? n 0) (car lat))
      (else (index (sub1 n) (cdr lat))))))

(define xor
  (lambda (bit1 bit2)
    (cond
      ((and (eq? bit1 0) (eq? bit2 0)) 0)
      (else 1))))

(define xor+
  (lambda (bit1 bit2)
    (cond
      ((null? bit1) bit2)
      ((null? bit2) bit1)
      (else
       (cons (xor (car bit1) (car bit2))
             (xor+ (cdr bit1) (cdr bit2)))))))

;----{ Chapter 1 }-----------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;----{ Chapter 2 }-----------------------------
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (equal? a (car lat))
                (member? a (cdr lat)))))))

;----{ Chapter 3 }-----------------------------
(define rember
  (lambda (s l)
    (cond
      ((null? l) l)
      ((equal? (car l) s) (cdr l))
      (else 
       (cons (car l) (rember s (cdr l)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((equal? (car lat) a) (multirember a (cdr lat)))
      (else 
       (cons (car lat) (multirember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) l)
      (else
       (cons (car (car l)) 
        (firsts (cdr l)))))))

; Optional
(define lasts
  (lambda (l)
    (cond
      ((null? l) l)
      (else
       (cons (pick (len (car l)) (car l)) 
             (lasts (cdr l)))))))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((equal? old (car lat)) (cons (car lat) (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((equal? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((equal? old (car lat)) (cons new lat))
      (else
       (cons (car lat) (insertL new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((equal? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond 
      ((null? lat) lat)
      ((equal? old (car lat)) (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) lat)
      ((or (equal? o1 (car lat)) (equal? o2 (car lat)))
       (cons new (cdr lat)))
      (else
       (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond 
      ((null? lat) lat)
      ((equal? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else
       (cons (car lat) (multisubst new old (cdr lat)))))))

;----{ Chapter 4 }-----------------------------
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(define tup?
  (lambda (tup)
    (cond
      ((null? tup) #t)
      ((number? (car tup)) (tup? (cdr tup)))
      (else #f))))

(define addtup
 (lambda (tup)
   (cond
     ((null? tup) 0)
     (else
      (o+ (car tup) (addtup (cdr tup)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
       (cons (o+ (car tup1) (car tup2))
             (tup+ (cdr tup1) (cdr tup2)))))))

(define addtup+
  (lambda (tup1 tup2)
    (addtup (tup+ tup1 tup2))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
       (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
       (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((o> n m) #f)
      ((o< n m) #f)
      (else #t))))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else
       (x n (^ n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
      ((o< n m) 0)
      (else (add1 (div (o- n m) m))))))

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((null? lat) lat)
      ((eq? n 1) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((null? lat) lat)
      ((eq? n 1) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
       (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else
       (all-nums (cdr lat))))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((equal? a (car lat)) 
       (add1 (occur a (cdr lat))))
      (else
       (occur a (cdr lat))))))

;----{ Chapter 5 }-----------------------------
(define rember*
  (lambda (a l)
    (cond
      ((null? l) l)
      ((atom? (car l)) (cond
                         ((equal? (car l) a)
                          (rember* a (cdr l)))
                         (else (cons (car l)
                                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) 
                  (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) l)
      ((atom? (car l)) 
       (cond ((equal? (car l) old)
              (cons old (cons new (insertR* new old (cdr l)))))
             (else (cons (car l) (insertR* new old (cdr l))))))
       (else
        (cons (insertR* new old (car l)) 
              (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l)) (cond ((equal? a (car l)) 
                              (add1 (occur* a (cdr l))))
                             (else (occur* a (cdr l)))))
      (else
       (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) l)
      ((atom? (car l)) (cond ((equal? old (car l))
                              (cons new (subst* new old (cdr l))))
                             (else (cons (car l)
                                         (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) l)
      ((atom? (car l)) (cond ((equal? old (car l))
                              (cons new (cons old 
                                    (insertL* new old (cdr l)))))
                             (else (cons (car l) 
                                    (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l)) 
             (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l)) (or (equal? a (car l))
                           (member* a (cdr l))))
      (else
       (or (member* a (car l)) 
           (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else
       (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((null? l2) #f)
      (else
       (and (equals? (car l1) (car l2)) 
            (eqlist? (cdr l1) (cdr l2)))))))

(define equals?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (equal? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else
       (eqlist? s1 s2)))))

;----{ Chapter 6 }-----------------------------
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car aexp)))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((equal? (operator nexp) (quote +)) 
       (o+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))) )
      ((equal? (operator nexp) (quote x)) 
       (x  (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))) )
      (else
       (^  (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))) ))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons n (list) )))

(define zub1
  (lambda (n)
    (cdr n)))
(define o+o
  (lambda (m n)
    (cond
      ((sero? (car m)) n)
      (else (edd1 (o+o n (zub1 m)))) )))

;----{ Chapter 7 }-----------------------------
(define set?
  (lambda (lat)
    (cond 
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset-alt
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((member? (car lat) (cdr lat)) (makeset-alt (cdr lat)))
      (else
       (cons (car lat) (makeset-alt (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) lat)
      (else
       (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and
             (member? (car set1) set2) 
             (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) set1)
      ((member? (car set1) set2) (cons 
                                  (car set1)
                                  (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else
       (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (set)
    (cond
      ((null? (cdr set)) (car set))
      (else 
       (intersect (car set) (intersectall (cdr set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define xfirst
  (lambda (x)
    (car x)))

(define xsecond
  (lambda (x)
    (car (cdr x))))

(define xthird
  (lambda (x)
    (car (cdr (cdr x)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (cons (list))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (xsecond pair) (xfirst pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) rel)
      (else (cons 
             (revpair (car rel))
             (revrel (cdr rel)))))))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))


;----{ Chapter 8 }-----------------------------
(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) l)
      ((test? a (car l)) (cdr l))
      (else
       (cons (car l) (rember-f test? a (cdr l)))))))


(define eq?-c
  (lambda (a)
    (lambda (x)
      (equal? x a))))

(define eq?-salad (eq?-c "salad"))

(define rember-f2
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) l)
        ((test? a (car l)) (cdr l))
        (else
         (cons (car l) ((rember-f2 test?) a (cdr l))))))))
