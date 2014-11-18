(defun atom? (x)
   (atom x)
)



(defun null? (x)
	(null x)
)


(defun list? (x)
	(listp x)
)



(defun first (x)
	( cond ((null? x) ( error "first: The given arg is null and the expected arg must be a non-empty list"))
		((atom? x) (error "first: The given arg is atom and the expected arg must be a non-empty list"))
		(T (car x))
	)
)


(defun last (x)
	( cond ((null? x) ( error "last The given arg is null and the expected arg must be a non-empty list"))
		((atom? x) ( error "last: The given arg is atom and the expected arg must be a non-empty list"))
		(T (car (reverse x)))
	)
)


(defun tail (x)
	(cond ((null? x) ( error "tail: The given arg is null and the expected arg must be a non-empty list"))
		((atom? x) (error "tail: The given arg is atom and the expected arg must be a non-empty list"))
	 	(T (cdr x))
	)
)

(defun head (x)
	(cond ((null? x) ( error "head: The given arg is null and the expected arg must be a non-empty list"))
		((atom? x) (error "head: The given arg is atom and the expected arg must be a non-empty list"))
	 	(T (reverse (tail ( reverse x))))
	)
)

(defun appendl (y x)
	(cond ((null? y) (error "appendl: Given-First argument is null; Expected-First argument must be an atom"))
	      (T (cond ((null? x) (list y))
                               ((atom? x) (error "appendl: Given-Second argument is an atom; Expected- Second arg must be a list"))
                               (T (cons y x))
                         )
              )
	)
)

(defun appendr (x y)
	(cond ((null? y) (error "appendr: Given-Second argument is null; Expected-Second argument must be an atom"))
              (T (cond ((null? x) (list y))
                               ((atom? x) (error "appendr: Given-First argument is an atom; Expected- First argumnet must be a list"))
                               (T (reverse (cons y (reverse x))))
                         )
              )
	)
)