
;;;Solution to Problem 1
(defun SUM (L)
	(cond ((null L)0)
		((not (numberp (car L))) 'ERR!)
		(T (let ((x (SUM (cdr L))))
			(cond ((numberp X) (+ (car L)X))
				(t 'ERR!))))))




;;;Solution to Problem 2
(defun NEG-NUMS (L)
	(cond ((null L) NIL)
		((not (numberp (car L))) 'ERR!)
		(T (let ((x (neg-nums (cdr L))))
				(cond ((not (listp x)) 'ERR!) 
				((< (car L) 0) (cons (car L) x))
				(t x))))))
			

		
;;;Solution to Problem 3
(defun inc-list-2 (L N)
	(cond ((NULL L) NIL)
		((or (not (numberp N))(not (numberp (car L)))) 'ERR!)
		(T (let ((x (inc-list-2 (cdr L) N)))
				(cond ((not (listp x)) 'ERR!)
					(t (cons(+ (car L) N) X)))))))

;;;Solution to Problem 4
(defun insert (N L)
	(cond ((null L) (list N))
		((not(numberp N)) 'ERR!)
		(t (let(( x(insert N (cdr L))))
			(if (<= N (car L))
				(cons N L)
				(cons (car L) X))))))

	
;;;Solution to Problem 5
(defun isort (L)
	(cond ((null L) NIL)
		(T (insert (car L) (ISORT (cdr L))))))
	


;;;Solution to Problem 6
(defun  split-list (L)
	(cond ((null L) '(()()))
		(t(let ((x (split-list (cdr L))))
			(list (cons (car L)(SECOND x)) (FIRST x))))))

		
	
			
;;;Solution to Problem 7
(defun my-partition (L P)
	(cond ((null L) '(()()))
		(t(let ((X (partition (cdr L) P)))
			(if (< (car L) P)
				(list (cons (car L)(FIRST X)) (SECOND X))
				(list (FIRST X)(cons (car L)(SECOND X))))))))

;;;Solution to Problem 8

(defun POS (E L)
	(cond ((null L) 0)
		((EQUAL E (CAR L)) 1)
		(T (let ((X (POS E (CDR L))))
			(cond ((= x 0) 0)
				(t (+ x 1)))))))
				
;;;Solution to Problem 9
(defun split-nums (N)
	(cond ((< N 0)NIL)
		((zerop N) (list (list 0) NIL))
		(T (let ((x(split-nums(- N 1))))
			(if(evenp N)
				(list (cons N (car x))(cadr X))
				(list (car x)(cons N (cadr X))))))))
				
;;;Solution to Problem 10
(defun set-union (s1 s2)
	(cond ((null s1) s2)
		((null s2) s1)
		(t (let ((x (set-union (cdr s1) s2)))
			(if(member (car s1) x)
			x
			(cons (car s1) x))))))


;;;Solution to Problem 11	
(defun set-remove (r s)		
		(cond ((null s) NIL)
			((not(atom r)) NIL)
			(t(let ((x (set-remove R (cdr s))))
				(cond((eq r (car s)) x)
					(t(cons (car s)x )))))))

;;;Solution to Problem 12		
(defun set-excl-union (s1 s2)
	(cond ((null s1) s2)
		((null s2) s1)
		(t (let ((x (set-excl-union (cdr s1) s2)))
			(if (member (car s1) x)
				(set-remove (car s1) x)
				(cons (car s1) x))))))
		
;;;Solution to Problem 13
(defun singletons	(e)
	(cond ((null e) nil)
		(t(let ( (x (singletonS (cdr e))) )
		(if (member (car e) x)
			(set-remove (car e) x)
			(cons (car e) x ))))))
	
	


				