
(defun vector? (x) 
  (cond ((null? x) T)
        ((atom? x) nil) ;; (error "vector-all: Given-Argument is atom; Expected-Argument must be a non-empty list"))
        (T (and-all (funcall 'apply-to-all 'integer? x)))
   )
)


(defun matrix? (x)
  (cond ((null? x) nil)
        ((list? x) (and (and-all (funcall 'apply-to-all 'vector? x)) (equal-all (funcall 'apply-to-all 'length x))))
        (T nil)
   )
)


(defun shape (x)
  (cond ((matrix? x) (appendl (length x) (list (length (myfirst x)))))
        (T (error "shape:Given-Argument is not a matrix; Expected-Argument is a matrix"))
   )
)



(defun zero (n) 
  (cond ((not (integer? n)) (error "zero: Given-Argument is not an intger; Expected-Argument must be a positive integer"))
        ((equal? n 1) (list (list 0)))
        ((not (> n 0)) (error "zero: Given-Argument is not a positive integer; Expected-Argument must be a positive integer"))
        (T (apply-to-all1 'appendl '0 (appendl (myfirst (zero (- n 1))) (zero (- n 1)))))
   )
)


(defun id (n) 
  (cond ((not (integer? n)) (error "ID: Given-Argument is not an intger; Expected-Argument must be a positive integer"))
        ((equal? n 1) (list (list 1)))
        ((not (> n 0)) (error "ID: Given-Argument is not an intger; Expected-Argument must be a positive integer"))
        (T (appendr (apply-to-all 'reverse (apply-to-all1 'appendl '0 (reverse (ID (- n 1))))) (appendl '0 (reverse (myfirst (ID (- n 1)))))))
   )
)


(defun transpose (x) 
  (cond ((and (matrix? x) (equal? 1 (length (myfirst x)))) (list (apply-to-all 'myfirst x)))
        ((matrix? x) (appendl (apply-to-all 'myfirst x) (transpose (apply-to-all 'tail x))))
        (T (error "transpose: Given Argument is not a matrix; Expected-Argument must be matrix"))
   )
)



(defun dot (x y)
  (cond ((and (null? x) (null? y)) 0)
        ((and (and (vector? x) (vector? y)) (equal? (length x) (length y))) (myreduce '+ (apply-to-all2 '* x y)))    ;;((or (not (vector? x)) (not (vector? y))) (error "dot: Given Arguments are not vectors; Expected-Arguments must be vectors"))
        (T (error "dot: Given-Arguments are not vectors of same length; Expected-Arguments must be vectors of same length"))
   )
)



(defun mplus (x y)
  (cond ((or (not (matrix? x)) (not (matrix? y))) (error "mplus: Given-Atleast one argument is not a matrix; Expected-Two Matrices of same shape"))
        ((not (and-all (apply-to-all2 'equal? (shape x) (shape y)))) (error "mplus: Given-Arguments are matrices of different shapes; Expected-Two Matrices of same shape"))
        ((equal? 1 (length x)) (list (apply-to-all2 '+ (myfirst x) (myfirst y))))
        (T (appendl (apply-to-all2 '+ (myfirst x) (myfirst y)) (mplus (tail x) (tail y))))
   )
)


(defun mtimes (x y) 
  (cond  ((or (not (matrix? x)) (not (matrix? y))) (error "mtimes: Given-Atleast one argument is not a matrix; Expected-Two Matrices of same shape"))
         ((not (equal? (mylast (shape x)) (myfirst (shape y)))) (error "mtimes: Given-Arguments are matrices of incompatible shapes; Expected-Two Matrices of compatable shape"))
         ((equal? 1 (length x)) (list (apply-to-all1 'dot (myfirst x) (transpose y))))
         (T (appendl (apply-to-all1 'dot (myfirst x) (transpose y)) (mtimes (tail x) y)))
   )
)

(defun btimes (x y)
  (cond ((or (not (matrix? x)) (not (matrix? y))) (error "btimes: Given-Atleast one argument is not a matrix; Expected-Two square Matrices of same size"))
        ((or (not (square? x)) (not (square? y))) (error "btimes: Given-Atleast one argument is not a square matrix; Expected-Two square Matrices of same size"))
        ((not (and-all (apply-to-all2 'equal? (shape x) (shape y)))) (error "btimes: Given-Square Matrices are not of same size; Expected-Two square Matrices of same size"))
        (T (mminus (mtimes x y) (mtimes y x)))
   )
)


