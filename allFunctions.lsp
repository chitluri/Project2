(defun atom? (x)
   (atom x)
)



(defun null? (x)
	(null x)
)


(defun list? (x)
	(listp x)
)



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
              ((atom? x) (error "appendr: Given-Second argument is an atom; Expected- Second arg must be a list"))
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
