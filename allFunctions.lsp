(defun atom? (x)
   (atom x)
)



(defun null? (x)
	(null x)
)


(defun list? (x)
	(listp x)
)



(defun m-first (x)
	( cond ((null? x) ( error "first: The given arg is null and the expected arg must be a non-empty list"))
		((atom? x) (error "first: The given arg is atom and the expected arg must be a non-empty list"))
		(T (car x))
	)
)


(defun m-last (x)
	( cond ((null? x) ( error "last The given arg is null and the expected arg must be a non-empty list"))
		((atom? x) ( error "last: The given arg is atom and the expected arg must be a non-empty list"))
		(T (car (reverse x)))
	)
)


(defun m-tail (x)
	(cond ((null? x) ( error "tail: The given arg is null and the expected arg must be a non-empty list"))
		((atom? x) (error "tail: The given arg is atom and the expected arg must be a non-empty list"))
	 	(T (cdr x))
	)
)

(defun m-head (x)
	(cond ((null? x) ( error "head: The given arg is null and the expected arg must be a non-empty list"))
		((atom? x) (error "head: The given arg is atom and the expected arg must be a non-empty list"))
	 	(T (reverse (m-tail ( reverse x))))
	)
)

(defun m-appendl (y x)
	(cond ((null? x) (list y))
              ((atom? x) (error "appendl: Given-Second argument is an atom; Expected- Second arg must be a list"))
              (T (cons y x))
        )
)

(defun m-appendr (x y)
        (cond ((null? x) (list y))
              ((atom? x) (error "appendl: Given-Second argument is an atom; Expected- Second arg must be a list"))
              (T (reverse (cons y (reverse x))))
        )
)
(defun m-reduce (f x)
	( cond ((null? x) nil)
		((null? (m-tail x)) (m-first x))
		(T (funcall f (m-first x) (m-reduce f (m-tail x))))
	)
)

(defun m-apply-to-all (f x)
	(cond ((null? x) nil)
		((atom? x) (error "apply-to-all: argument must be a list"))
		(T (mapcar f x))
	)
)

(defun m-apply-to-all1 (f x y)
	(cond ((null? y) nil)
		((atom? y) (error "apply-to-all1: 2nd argument must be a list"))
		(T (m-appendl (funcall f x (m-first y)) (m-apply-to-all1 f x (m-tail y))))

	)
)

(defun m-apply-to-all2 (f x y)
	(cond ((null? x) (cond ((null? y) nil) (T (m-apply-to-all2 f y x))))
		  ((null? x) (cond ((not (null? y)) (error "m-apply-to-all2: Given-Only one argument as null; Expected-Both arguments must be null or non-empty lists with equal lengths"))))
		  ((null? y) (cond ((not (null? x)) (error "m-apply-to-all2: Given-Only one argument as null; Expected-Both arguments must be null or non-empty lists with equal lengths"))))
		  ((atom? x) (error "m-apply-to-all2: Given-One of the argument as an atom; Expected-Both arguments must be null or non-empty lists with equal lengths"))
		  ((atom? y) (error "m-apply-to-all2: Given-One of the argument as an atom; Expected-Both arguments must be null or non-empty lists with equal lengths"))
		  ((not (equal (length x) (length y))) (error "m-apply-to-all2: Given: Two lists of unequal lengths; Expected-Two lists of equal lengths "))
		  ((null? (m-tail x)) (list (funcall f (m-first x) (m-first y))))
		  (T (m-appendl (funcall f (m-first x) (m-first y)) (m-apply-to-all2 f (m-tail x) (m-tail y))))
	)
)


(defun m-construction (x y)
	(cond ((null? x) nil)
		  ((atom? x) (error "m-construction: Given- First argument as an atom; Expected-First argument to be a list of functions"))
		  ((null? (m-tail x)) (list (funcall (m-first x) y)))
		  (T (m-appendl (funcall (m-first x) y) (m-construction (m-tail x) y)))
	)
)

(defun m-constant (x y)	(cond (T x)))
