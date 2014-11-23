(myfirst 'a)
(myfirst '())
(myfirst '(1))
(myfirst '(1 2 3))


(mylast 'a)
(mylast '())
(mylast '(1))
(mylast '(1 2 3))



(tail 'a)
(tail '())
(tail '(1))
(tail '(1 2 3))



(head 'a)
(head '())
(head '(1))
(head '(1 2 3))



(appendl 'a 'b)
(appendl 'a '())
(appendl 'a '(1))
(appendl 'a '(1 2 3))
(appendl '() 'b)
(appendl '() '())
(appendl '() '(1))
(appendl '() '(1 2 3))
(appendl '(1) 'b)
(appendl '(1) '())
(appendl '(1) '(1))
(appendl '(1) '(1 2 3))
(appendl '(1 2 3) 'b)
(appendl '(1 2 3) '())
(appendl '(1 2 3) '(1))
(appendl '(1 2 3) '(1 2 3))



(appendr 'a 'b)
(appendr 'a '())
(appendr 'a '(1))
(appendr 'a '(1 2 3))
(appendr '() 'b)
(appendr '() '())
(appendr '() '(1))
(appendr '() '(1 2 3))
(appendr '(1) 'b)
(appendr '(1) '())
(appendr '(1) '(1))
(appendr '(1) '(1 2 3))
(appendr '(1 2 3) 'b)
(appendr '(1 2 3) '())
(appendr '(1 2 3) '(1))
(appendr '(1 2 3) '(1 2 3))
