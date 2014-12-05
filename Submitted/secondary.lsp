(defun and-all (x)
  (cond ((null? x) (error "and-all: Given-Argument is null; Expected-Argument must be a non-empty list"))
        ((atom? x) "and-all: Given-Argument is atom; Expected-Argument must be a non-empty list")
        ((and (null? (tail x)) (equal? T (myfirst x))) T)
        ((equal? T (myfirst x)) (and-all (tail x)))
        (T nil)
   )
)


(defun equal-all (x)
  (cond ((null? x) (error "equal-all: Given-Argument is null; Expected-Argument must be a non-empty list"))
        ((atom? x) "equal-all: Given-Argument is atom; Expected-Argument must be a non-empty list")
        ((null? (tail x)) T)
        ((equal? 2 (length x)) (equal? (myfirst x) (myfirst (tail x))))
        ((equal? (myfirst x) (myfirst (tail x))) (equal-all (tail x)))
        (T nil)
   )
)


(defun mminus (x y)
  (cond ((or (not (matrix? x)) (not (matrix? y))) (error "mminus: Given-Atleast one argument is not a matrix; Expected-Two Matrices of same shape"))
        ((not (and-all (apply-to-all2 'equal? (shape x) (shape y)))) (error "mminus: Given-Arguments are matrices of different shapes; Expected-Two Matrices of same shape"))
        ((equal? 1 (length x)) (list (apply-to-all2 '- (myfirst x) (myfirst y))))
        (T (appendl (apply-to-all2 '- (myfirst x) (myfirst y)) (mminus (tail x) (tail y))))
   )
)


(defun square? (x) 
  (cond ((matrix? x) (equal-all (shape x)))
        (T NIL)
   )
)



