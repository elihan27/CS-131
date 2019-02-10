(print-as-expression #f)
(define (list-search x y z pairlist)

  (
   if (or (null? x) (null? y))
       z
       ( if (equal? (car x) (car y))
	    ( list-search (cdr x) (cdr y) (cons (car x) z) pairlist)                         
            ( list-search (cdr x) (cdr y) (cons (true-compare (car x) (car y) pairlist) z) pairlist )
	    )))


(define (true-compare x y pairlist)
  ;(display pairlist)
  ( cond ((equal? x y) x)
	 ((and (boolean? x) (boolean? y)) ( if x '% '(not %)))
	 ((and (list? x) (list? y))
	  ( cond ((and (equal? 'if (car x)) (not (equal? 'if (car y)))) `(if % ,x ,y))
                 ((and (equal? 'if (car y)) (not (equal? 'if (car x)))) `(if % ,x ,y))
		 ((or (equal? 'let (car x)) (equal? 'let (car y))) (let-search x y pairlist))
		 ((or (equal? 'quote (car x)) (equal? 'quote (car y)))`(if % ,x ,y ))
		 (else (reverse (list-search x y empty empty)))))
	 ((not(null? pairlist)) (sym-put x y pairlist))
	 (else `(if % ,x ,y ))))

(define (expr-compare x y)
  (true-compare x y empty))


(define (let-search x y pairlist)
  (
   if (and (equal? 'let (car x)) (equal? 'let (car y)))
      (reverse (list-search (cdr x) (cdr y) '(let) (let-list (car (cdr x)) (car (cdr y)) empty)))
      `(if % ,x ,y )
      )
  )


(define (member? x y)
  (if (list? (member x y))
      #t
      #f))

(define (bind a b)
   (display "bind")
  (string->symbol
   (string-append
    (symbol->string a )
    "!"
           (symbol->string b ) ) )
  )

(define (let-list param_x param_y pairlist)
  (display param_x) (newline)
  (cond ((or (empty? param_x) (empty? param_y))  pairlist)
        ((member?
	  (list (car (car param_x)) (car (car param_y))) pairlist) pairlist)
	(let-list (cdr param_x) (cdr param_y) pairlist)
	  (else (cond ((equal? (car (car param_x)) (car (car param_y)))
		(let-list (cdr param_x) (cdr param_y) (cons (list (car (car param_x)) (car (car param_y)))  pairlist)))
               (else  (let-list
		       (cdr param_x)
		       (cdr param_y)
		       (append
			(list (list (car (car param_y)) (bind (first (car (car param_x))) (first (car (car param_y)))))
			      (list (car (car param_x)) (bind (first (car (car param_x))) (first (car (car param_y))))))
			      pairlist)))))))


;let-list essentially just creates a list for let
#|(define (let-list param_x param_y pairlist)
  (cond ((empty? param_x) pairlist)
	((empty? param_y) pairlist))
  (cond ((or (empty? param_x) (empty? param_y))  pairlist)
        ((member? (list (car (car param_x)) (car (car param_y))) (let-list (cdr param_x) (cdr param_y) pairlist)))
        (else
         (cond ((equal? (car (car param_x)) (car (car param_y))) (let-list (cdr param_x) (cdr param_y) (cons (list (car (car param_x)) (car (car param_y)))  pairlist)))
               (else     (let-list (cdr param_x) (cdr param_y) (append (list (list (car (car param_y)) (bind (first (car (car param_x))) (first ((car (car param_y)))) (list (car (car param_x))) (bind (first(car (car param_x))) (first (car (car param_y))))))  pairlist))))))))
|#

;returns cdr of what's left of the pairlist after you find your pair
(define (mem-sym x pairlist)
  (cond ((null? pairlist) #f)
	((member? x (car pairlist)) (cdr pairlist))
	(else (mem-sym x (cdr pairlist)))))
;actually finds your pair
(define (one-sym x pairlist)
  (cond ((null? pairlist) #f)
        ((member? x (car pairlist)) (second (car pairlist)))
        (else (mem-sym x (cdr pairlist)))))

;your proper output
(define (sym-put x y pairlist)
  (cond ((equal? (one-sym x pairlist) (one-sym y pairlist)))
	( else `(if % ,(one-sym x pairlist) ,(one-sym y pairlist))))) 

	  
(define (test-expr-compare x y)
  (if (and (equal? (eval x)  (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y)  (eval (list 'let '((% #f)) (expr-compare x y)))))
      #t
      #f))



(define test-expr-x '(let ((a 1) (c 2) (b 3) (d 4)) (lambda (a b) (+ a b)) 1 25))
(define test-expr-y '(let ((a 2) (b 3) (c 1) (d 4)) (lambda (d c) (+ c d)) 2 12))


