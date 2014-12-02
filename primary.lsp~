(defun integer? (x) (integerp x))

(defun zero? (x) (zerop x))

(defun plus? (x) (plusp x))

(defun minus? (x) (minusp x))

(defun equal? (x y) (equal x y))






(defun and-all (x)
  (cond ((null? x) (error "and-all: Given-Argument is null; Expected-Argument must be a non-empty list"))
        ((atom? x) "and-all: Given-Argument is atom; Expected-Argument must be a non-empty list")
        ((and (null? (tail x)) (eq T (myfirst x))) T)
        ((eq T (myfirst x)) (and-all (tail x)))
        (T nil)
   )
)


(defun equal-all (x)
  (cond ((null? x) (error "equal-all: Given-Argument is null; Expected-Argument must be a non-empty list"))
        ((atom? x) "equal-all: Given-Argument is atom; Expected-Argument must be a non-empty list")
        ((null? (tail x)) T)
        ((eq 2 (length x)) (eq (myfirst x) (myfirst (tail x))))
        ((eq (myfirst x) (myfirst (tail x))) (equal-all (tail x)))
        (T nil)
   )
)


(defun vector? (x) 
  (cond ((null? x) T)
        ((atom? x) "vector-all: Given-Argument is atom; Expected-Argument must be a non-empty list")
        (T (and-all (funcall 'apply-to-all 'integer? x)))
   )
)


(defun matrix? (x)
  (cond ((null? x) T)
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
  (cond ((not (integer? n)) (error "zero: argument must be an non-negative integer"))
        ((equal? n 1) (list (list 0)))
        ((not (> n 0)) (error "zero: argument must be a non-negative integer"))
        (T (apply-to-all1 'appendl '0 (appendl (myfirst (zero (- n 1))) (zero (- n 1)))))
   )
)


(defun id (n) 
  (cond ((not (integer? n)) (error "zero: argument must be an non-negative integer"))
        ((equal? n 1) (list (list 1)))
        ((not (> n 0)) (error "zero: argument must be a non-negative integer"))
        (T (appendr (apply-to-all 'reverse (apply-to-all1 'appendl '0 (reverse (ID (- n 1))))) (appendl '0 (reverse (myfirst (ID (- n 1)))))))
   )
)


(defun transpose (x) 
  (cond ((and (matrix? x) (eq 1 (length (myfirst x)))) (list (apply-to-all 'myfirst x)))
        ((matrix? x) (appendl (apply-to-all 'myfirst x) (transpose (apply-to-all 'tail x))))
        (T (error "transpose: Given Argument is not a matrix; Expected-Argument must be matrix"))
   )
)



(defun dot (x y)
  (cond ((and (and (vector? x) (vector? y)) (eq (length x) (length y))) (myreduce '+ (apply-to-all2 '* x y)))    ;;((or (not (vector? x)) (not (vector? y))) (error "dot: Given Arguments are not vectors; Expected-Arguments must be vectors"))
        (T (error "dot: Given-Arguments are not vectors of same length; Expected-Arguments must be vectors of same length"))
   )
)



(defun mplus (x y)
  (cond ((null? x) nil)
        ((or (not (matrix? x)) (not (matrix? y))) (error "mplus: Given-Atleast one argument is not a matrix; Expected-Two Matrices of same shape"))
        ((not (and-all (apply-to-all2 'eq (shape x) (shape y)))) (error "mplus: Given-Arguments are matrices of different shapes; Expected-Two Matrices of same shape"))
        (T (appendl (apply-to-all2 '+ (myfirst x) (myfirst y)) (mplus (tail x) (tail y))))
   )
)


(defun mtimes (x y) 
  (cond  ((null? x) nil)
         ((or (not (matrix? x)) (not (matrix? y))) (error "mtimes: Given-Atleast one argument is not a matrix; Expected-Two Matrices of same shape"))
         ((not (eq (mylast (shape x)) (myfirst (shape y)))) (error "mtimes: Given-Arguments are matrices of incompatible shapes; Expected-Two Matrices of compatable shape"))
         (T (appendl (apply-to-all1 'dot (myfirst x) (transpose y)) (mtimes (tail x) y)))
   )
)


