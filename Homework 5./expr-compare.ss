;lambda arguments number                                                                                                          
;keywords                                                                                                                         
;type                                                                                                                             
;(define (prod ls)                                                                                                                
;(call-with-cc (lambda (break)                                                                                                    
;(let pr ((ls ls))                                                                                                                
;(if (null? ls) 1                                                                                                                 
;(if (zero? (car ls)) (break 0) (* (car ls ) (pr (car ls))))                                                                      
;))))                                                                                                                             


(define LAMBDA (string->symbol "\u03BB"))
(define (is_lambda x)
  (if (or (equal? x 'LAMBDA) (equal? x 'lambda)) #t #f)
)

(define (is_key x)
  (if (or (equal? x 'if) (equal? x 'quote) (is_lambda x)) #t #f)
)

(define (unify_lambda x y)
  (if (or (equal? x 'LAMBDA) (equal? y 'LAMBDA)) LAMBDA 'lambda)
)

(define (check_type x y)
  (if (or
       (or (and (list? x) (not(list? y))) (and (list? y) (not(list? x))))
       (or (and (pair? x) (not(pair? y))) (and (pair? y) (not(pair? x))))
      ) #f #t
  )
)

;assume equal length list,is args always list of symbols?                                                                         
(define (lambda_args x y)
  (cond
   [(empty? x) '()]
   [(equal? (car x) (car y)) (cons (car x) (lambda_args (cdr x) (cdr y)))]
   [else (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string(car y)))) (lambda_args (cdr x) (cdr y)\
))]
  )
)

(define (pr_length x)
  (if (pair? (cdr x)) (+ 1 (pr_length (cdr x))) 2)
)

(define (lambda_pair_args x y)
  (cond
   [(not(pair? (cdr x)))
    (if (equal? (car x) (car y))
        (cons (car x) (if (equal? (cdr x) (cdr y)) (cdr x)
                          (string->symbol (string-append (symbol->string (cdr x)) "!" (symbol->string(cdr y))))
                      )
        )
        (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string(car y))))
              (if (equal? (cdr x) (cdr y)) (cdr x)
                  (string->symbol (string-append (symbol->string (cdr x)) "!" (symbol->string(cdr y))))
              )
        )
    )
   ]
   [(equal? (car x) (car y)) (cons (car x) (lambda_pair_args (cdr x) (cdr y)))]
   [else (cons (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string(car y)))) (lambda_pair_args (cdr x) (c\
dr y)))]
  )
)

(define (ptl x)
  (if (pair? (cdr x)) (cons (car x) (ptl (cdr x))) (cons (car x) (cons (cdr x) '())) )
)

(define (ptl x)
  (if (pair? (cdr x)) (cons (car x) (ptl (cdr x))) (cons (car x) (cons (cdr x) '())) )
)
;assume equal length list                                                                                                         
(define (diff x y l1 l2)
   (cond
   [(empty? x) (cons l1 l2)]
   [(equal? (car x) (car y)) (diff (cdr x) (cdr y) l1 l2)]
   [else (diff (cdr x) (cdr y) (cons (car x) l1) (cons (car y) l2))]
  )
)

(define (l_member x y)
  (cond
   [(symbol? y) (equal? x y)]
   [(list? y) (member x y)]
   [else (if (pair? (cdr y)) (or (equal? x (car y)) (l_member x (cdr y))) (or (equal? x (car y)) (equal? x (cdr y))) )]
  )
)

(define (proc_arg x y l1 l2)
       (cond
;        [(symbol? l1) (if (equal? x l1) y l1)]                                                                                   
         [(empty? l1) l2]
         [(equal? x (car l1)) (proc_arg x y (cdr l1) (append l2 (list y)))]
         [else
          (if (list? (car l1))
              (if (and (is_lambda (caar l1)) (l_member x (cadar l1)) )
               (proc_arg x y (cdr l1) (append l2 (list (car l1))) )
               (proc_arg x y (cdr l1) (append l2 (list (proc_arg x y (car l1) '()))) )
              )

              (proc_arg x y (cdr l1) (append l2 (list (car l1))))
          )]
       )
)

(define (proc_list_arg x y l)
  (cond
         [(empty? x) l]
         [else (proc_list_arg (cdr x) (cdr y) (proc_arg (car x) (car y) l '())) ]
       )
)

(define (expr-compare x y)  (cond
           [(equal? x y) x]
           ;x is not equal to y                                                                                                                                                                             
           ;a special case for bool                                                                                                                                                                         
           [(and (boolean? x) (boolean? y)) (if x '% '(not %))]
           ;everything other than lambda                                                                                                                                                                    
           [(or
             (not (and (list? x) (list? y)));if not equal then not same type or same type not same value                                                                                                    
             (not (= (length x) (length y)))
             (empty? x)
             (empty? y)
             (and (is_key (car x)) (not (is_key (car y))))
             (and (is_key (car y)) (not (is_key (car x))))
             (and (not(and (is_lambda (car x)) (is_lambda (car y)))) (is_key (car x)) (is_key (car y)) (not (equal? (car x) (car y))) )
            )
            (list 'if '% x y)
           ]
           [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) (list 'if '% x y)]
           ;[(and (equal? 'quote (car x)) (equal? 'quote (car y))) (list 'if '% (cdr x) (cdr y))]                                                                                                           
           ;process lambda                                                                                                                                                                                  
           [(and (is_lambda (car x)) (is_lambda (car y)))
            (let ( [argx (car (cdr x))] [argy (car (cdr y))] [expx (cddr x)] [expy (cddr y)] [ld (unify_lambda (car x) (car y))])
              (cond
               [(or
                 (not (check_type argx argy))
                 (and (list? argx) (not(= (length argx) (length argy))))
                )
                (list 'if '% x y)
               ]
               [(and (symbol? argx) (symbol? argy))
                (if (equal? argx argy) (cons ld (cons argx (expr-compare expx expy))) ;(display "not equal")                                                                                                
                    (let ([sym (string->symbol (string-append (symbol->string argx) "!" (symbol->string argy)))])
                      (cons ld (cons sym (expr-compare (proc_arg argx sym expx '()) (proc_arg argy sym expy '()) ) ))
                    )
                )
               ]
               [(and (list? argx) (list? argy))
                (
                 if (equal? argx argy) (cons ld (cons argx (expr-compare expx expy)))
                    (let ([symlist (lambda_args argx argy)])
                      (let ([xdiff (diff argx symlist '() '())] [ydiff (diff argy symlist '() '())])
                        (cons ld (cons symlist (expr-compare (proc_list_arg (car xdiff) (cdr xdiff) expx) (proc_list_arg (car ydiff) (cdr ydiff) expy) )))
                      )
                    )
                )
                ]
               [else
                (cond
                 [(not(= (pr_length argx) (pr_length argy)))  (list 'if '% x y)]
                 [(equal? argx argy) (cons ld (cons argx (expr-compare expx expy)))]
                 [else
                  (let ([symlist (lambda_pair_args argx argy)])
                      (let ([xdiff (diff (ptl argx) (ptl symlist) '() '())] [ydiff (diff (ptl argy) (ptl symlist) '() '())])
                        (cons ld (cons symlist (expr-compare (proc_list_arg (car xdiff) (cdr xdiff) expx) (proc_list_arg (car ydiff) (cdr ydiff) expy) )))
                      )
                  )
                 ]
                )
               ]
              )
            )
           ]
           [else (cons (expr-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]
        ))

(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y))))
       (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y))))
  )
)

(define test-expr-x '(list 12 12 #t #f #t #f 'a '(cons a b) '(cons a b)
                         '(cons (cons a b) (cons b c))
                         '(cons a b)
                         '(list)
                         ''(a b)
                         '(quote (a b))
                         '(quoth (a b))
                         '(if x y z)
                         '(if x y z)
                         '((lambda (a) (f a)) 1)
                         '((lambda (a) (f a)) 1)
                         '((lambda (a) a) c)
                         ''((λ (a) a) c)
                         '(+ #f ((λ (a b) (f a b)) 1 2))
                         '((λ (a b) (f a b)) 1 2)
                         '((λ (a b) (f a b)) 1 2)
                         '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a)) a (lambda (a) a)))) (lambda (b a) (b a)))
                         '(lambda (a b) a)
                         '(lambda (a b) a)
                         '(lambda a a)
                         '(lambda a a)
                         '(lambda (a b) a)
                         '(lambda (a b) (a b c))
                         '(lambda (a b) (a b c))
                         '(lambda (a t . x) c)
                     )
)

(define test-expr-y '(list 12 20 #t #f #f #t '(cons a b) '(cons a b) '(cons a c)
                         '(cons (cons a c) (cons a c))
                         '(list a b)
                         '(list a)
                         ''(a c)
                         '(quote (a c))
                         '(quoth (a c))
                         '(if x z z)
                         '(g x y z)
                         '((lambda (a) (g a)) 2)
                         '((λ (a) (g a)) 2)
                         '((lambda (b) b) d)
                         '((lambda (b) b) d)
                         '(+ #t ((lambda (a c) (f a c)) 1 2))
                         '((λ (a b) (f b a)) 1 2)
                         '((λ (a b) (f c a)) 1 2)
                         '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a)) a (λ (b) a)))) (lambda (a b) (a b)))
                         '(lambda (a) a)
                         '(lambda (a . b) a)
                         '(lambda (a) a)
                         '(lambda b b)
                         '(lambda (a b) b)
                         '(lambda (a b) (c b a))
                         '(lambda (a b) a)
                         '(lambda (a x . c) x)

                     )
)
