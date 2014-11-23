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
