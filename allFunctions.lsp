(defun atom? (x)
   (atom x)
)



(defun null? (x)
	(null x)
)


(defun list? (x)
	(listp x)
)

(defun integer? (x) (integerp x))

(defun zero? (x) (zerop x))

(defun plus? (x) (plusp x))

(defun minus? (x) (minusp x))

(defun equal? (x y) (equal x y))


(defun myfirst (x)
	( cond ((atom? x) ( error "myfirst: Given-Argument is an or a nil; Expected-Argument must be a non-empty list"))
		(T (car x))
	)
)


(defun mylast (x)
	( cond ((atom? x) ( error "mylast: Given-Argument is an or a nil; Expected-Argument must be a non-empty list"))
		(T (car (reverse x)))
	)
)


(defun tail (x)
	(cond ((atom? x) ( error "tail: Given-Argument is an atom or a nil; Expected-Argument must be a non-empty list"))
	 	(T (cdr x))
	)
)

(defun head (x)
	(cond	((atom? x) (error "head: Given-Argument is an atom or a nil; Expected-Argument must be a non-empty list"))
	 	(T (reverse (tail ( reverse x))))
	)
)

(defun appendl (y x)
	(cond ((null? x) (list y))
              ((atom? x) (error "appendl: Given-Second argument is an atom; Expected- Second arg must be a list"))
              (T (cons y x))
        )
)

(defun appendr (x y)
        (cond ((null? x) (list y))
              ((atom? x) (error "appendr: Given-First argument is an atom; Expected- First arg must be a list"))
              (T (reverse (appendl y (reverse x))))
        )
)
(defun myreduce (f x)
	( cond ((null? x) (error "myreduce: Given-Second argument is nil; Expected-Second argument must be a list of atleast one object"))
		((null? (tail x)) (myfirst x))
		(T (funcall f (myfirst x) (myreduce f (tail x))))
	)
)

(defun apply-to-all (f x)
	(cond ((null? x) nil)
		((atom? x) (error "apply-to-all: argument must be a list"))
		(T (mapcar f x))
	)
)

(defun apply-to-all1 (f x y)
	(cond ((null? y) nil)
		((atom? y) (error "apply-to-all1: 2nd argument must be a list"))
		(T (appendl (funcall f x (myfirst y)) (apply-to-all1 f x (tail y))))

	)
)

(defun apply-to-all2 (f x y)
	(cond ((null? x) (cond ((null? y) nil) (T (apply-to-all2 f y x))))
		  ((null? x) (cond ((not (null? y)) (error "apply-to-all2: Given-Only one argument as null; Expected-Both arguments must be null or non-empty lists with equal lengths"))))
		  ((null? y) (cond ((not (null? x)) (error "apply-to-all2: Given-Only one argument as null; Expected-Both arguments must be null or non-empty lists with equal lengths"))))
		  ((atom? x) (error "apply-to-all2: Given-One of the argument as an atom; Expected-Both arguments must be null or non-empty lists with equal lengths"))
		  ((atom? y) (error "apply-to-all2: Given-One of the argument as an atom; Expected-Both arguments must be null or non-empty lists with equal lengths"))
		  ((not (equal (length x) (length y))) (error "apply-to-all2: Given: Two lists of unequal lengths; Expected-Two lists of equal lengths "))
		  ((null? (tail x)) (list (funcall f (myfirst x) (myfirst y))))
		  (T (appendl (funcall f (myfirst x) (myfirst y)) (apply-to-all2 f (tail x) (tail y))))
	)
)


(defun construction (x y)
	(cond ((null? x) nil)
		  ((atom? x) (error "construction: Given- First argument as an atom; Expected-First argument to be a list of functions"))
		  ((null? (tail x)) (list (funcall (myfirst x) y)))
		  (T (appendl (funcall (myfirst x) y) (construction (tail x) y)))
	)
)

(defun constant (x y) x)


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



