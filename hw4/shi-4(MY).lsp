;;;Q.A
(defun my-sum (L)
	(let ((x (sum (cdr L))))
		(+ (car L) x)))
;;;Q1
(defun SUM (L)
	(cond ((null L)0)
		((not (numberp (car L))) 'ERR!)
		(T (let ((x (SUM (cdr L))))
			(cond ((numberp X) (+ (car L)X))
				(t 'ERR!))))))


;;;Q.B
(defun my-neg-nums (L)
	(let ((x (neg-nums (cdr L))))
		(cond ((< (car L) 0) (cons (car L) x))
			(t x))))

;;;Q2
(defun NEG-NUMS (L)
	(cond ((null L) NIL)
		((not (numberp (car L))) 'ERR!)
		(T (let ((x (neg-nums (cdr L))))
				(cond ((not (listp x)) 'ERR!) 
				((< (car L) 0) (cons (car L) x))
				(t x))))))
			
			
;;;Q.C
(defun my-inc-list-2 (L N)
	(let ((x (inc-list-2 (cdr L) N)))
		(cons(+ (car L) N) x)))
		
;;;Q3
(defun inc-list-2 (L N)
	(cond ((NULL L) NIL)
		((or (not (numberp N))(not (numberp (car L)))) 'ERR!)
		(T (let ((x (inc-list-2 (cdr L) N)))
				(cond ((not (listp x)) 'ERR!)
					(t (cons(+ (car L) N) X)))))))
				
;;;Q.D
(defun my-insert (N L)
	(if (null L)
		(list N)
		(let(( x(insert N (cdr L))))
			(if (<= N (car L))
				(cons N L)
				(cons (car L) X)))))

;;;Q4
(defun insert (N L)
	(cond ((null L) (list N))
		((not(numberp N)) 'ERR!)
		(t (let(( x(insert N (cdr L))))
			(if (<= N (car L))
				(cons N L)
				(cons (car L) X))))))

;;;Q.E
(defun my-isort (L)
	(let ((x (isort (cdr L))))
		(insert (car L) x)))
		
;;;Q5
(defun isort (L)
	(cond ((null L) NIL)
		(T (insert (car L) (ISORT (cdr L))))))
	

;;;Q.F
(defun  my-split-list (L)
	(let ((x (split-list (cdr L))))
		(list (cons (car L)(SECOND x)) (FIRST x) )))
;;;Q6
(defun  split-list (L)
	(cond ((null L) '(()()))
		(t(let ((x (split-list (cdr L))))
			(list (cons (car L)(SECOND x)) (FIRST x))))))

		
	
;;;Q.G
(defun my-partition (L P)
	(let ((X (partition (cdr L) P)))
		(if (< (car L) P)
			(list (cons (car L)(FIRST X)) (SECOND X))
			(list (FIRST X)(cons (car L)(SECOND X))))))
			
;;;Q7
(defun my-partition (L P)
	(cond ((null L) '(()()))
		(t(let ((X (partition (cdr L) P)))
			(if (< (car L) P)
				(list (cons (car L)(FIRST X)) (SECOND X))
				(list (FIRST X)(cons (car L)(SECOND X))))))))

;;;Q8

(defun POS (E L)
	(cond ((null L) 0)
		((EQUAL E (CAR L)) 1)
		(T (let ((X (POS E (CDR L))))
			(cond ((= x 0) 0)
				(t (+ x 1)))))))
				