;;CSCI-316 
;;Assignments #5
;;Cheng Shi
;;ID#23375304

;;;Solution to Question 1
(defun index (N L)
	(cond ((OR (<= N 0) (NULL L))'ERROR)
		((and (= N 1)(null L))'ERROR)
		((and (= N 1)(not(null L)))(car L))
		(T(let ((x (index (- N 1) (cdr L))))
				x))))
		
;;;Solution to Question 2
(defun min-first (L)
	(cond((null L) NIL)
		((null(cdr L)) L)
		(t(let ((x (min-first (cdr L ))))
			(if(<= (car L) (car x))
				L
				(cons (car x)(cons (car L)(cdr x))))))))

;;;Solution to Question 3
(defun ssort (L)
	(if(null L)
		NIL
		(let* ((L1 (min-first L))
				(x (ssort (cdr L1))))
				(cons (car L1)X))))

;;;Solution to Question 4
(defun QSORT(L)
		(if(endp L)
			NIL
			(if (null (cdr L))
			L
			(Let((PL(PARTITION L(CAR L))))
				(if (null (FIRST PL)) 
				(SECOND PL)
				(append (QSORT (FIRST PL))(QSORT(SECOND PL))))))))

;;;Solution to Question 5
(defun MERGE-LISTS (L1 L2)
	(cond ((endp L1) L2)
		((endp L2) L1)
		((<(car L1) (car L2))(let ((x(MERGE-LIST(cdr L1) L2)))
									(cons (car L1) x)))
		(t (let ((x (MERGE-LIST L1 (cdr L2))))
									(cons (car L2) x)))))
									
;;;Solution to Question 6
(defun MSORT (L)
	(if(or(endp L)(endp (cdr L)))
	L
	(let* ((SL(split-list L))
			(A(MSORT(CAR SL)))
			(B(MSORT(CADR SL))))
			(MERGE-LIST A B))))
			
;;;Solution to Question 7	
(defun remove-adj-dupl (L)
	(if (endp L) 
		NIL
		(let ((x(remove-adj-dupl (cdr L))))
		(if (equalp (car L) (car x))
			x
			(cons (car L) x)))))
			
;;;Solution to Question 8
; (defun unrepeated-elts (L)
	; (if (endp L)
		; nil
		; (let ((x(unrepeated-elts (cdr L))))
			; (cond ((endp (member (car L) x))(cons (car L) x))
					; (t x))))))

;;;Solution to Question 8
(defun UNREPEATED-ELTS (L)
	(if (endp L)
		NIL
		(let ((x(unrepeated-elts (cdr L))))
		(cond ((or (endp (cdr L)) (not (equal (car L)(cadr L)))) (cons (first L)x))
				((or (endp (cddr L)) (not (equal (car L)(caddr L)))) (UNREPEATED-ELTS (cddr L)))
				(t x)))))		

;;;Solution to Question 9				
(defun REPEATED-ELTS (L)
   (cond ((endp L) nil)
         ((or (endp (cdr L)) (not (equal (car L) (cadr L)))) (REPEATED-ELTS (cdr L)))
		((or (endp (cddr L)) (not (equal (car L) (caddr L)))) (cons (car L) (REPEATED-ELTS (cddr L))))
		(t (REPEATED-ELTS (cdr L)))))

;;;Solution to Question 10		
(defun COUNT-REPETITIONS (L)
   (if (NULL L)
	NIL
	(let ((x (COUNT-REPETITIONS (cdr L))))
	   (if (equal (first L) (second L))
	       (append (list (list (+ 1 (caar x)) (cadar x))) (cdr x))
	       (append (list (list 1 (car L))) x)))))
		 
;;;Solution to Question 11
(defun subset (F L)
	(if (endp L)
	NIL
	(let ((x (subset F (cdr L))))
		(if (funcall F (car L))
			(cons (car L) x)
			x))))

;;;Solution to Question 12
(defun OUR-SOME (F L)
   (if (NULL L)
	nil
	(let ((x (OUR-SOME F (cdr L))))
	   (if (funcall F (car L))
		L
		x))))
		
(defun OUR-EVERY (F L)
   (if (NULL L)
	T
	(let ((X (OUR-EVERY F (cdr L))))
	   (if X (funcall F (car L))))))
	   
;;;Solution to Question 13
(defun PARTITION1 (f L p)
   (if (NULL L)
       (list '() '())
       (let ((x (PARTITION1 f (cdr L) p)))
		(cond ((funcall f (car L) p)
		      (append (list (cons (car L) (car x))) (list (cadr x))))
		      (t (append (list (car x)) (list (cons (car L) (cadr x)))))))))

(defun QSORT1 (f L)
   (cond ((endp L) nil)
	 (t (let ((Z (PARTITION1 f L (car L))))  
	          (cond ((endp (car Z)) (cons (car L) (QSORT1 f (cdr L))))
		        (t (let ((x (QSORT1 f (car Z)))
		                (y (QSORT1 f (cadr Z)))) 
			        (append  x  y))))))))

;;;Solution to Question 14
(defun FOO (f L)
   (if (NULL L) 
       NIL 
       (let* ((x (FOO f (cdr L))) (ls (cdr L))
	     (Z (list (cons (funcall f (car L)) ls)))
	     (Y (mapcar (lambda (a) (cons (car L) a)) x)))
	     (append Z Y))))

;;;Solution to Question 15
;PART a
(defun TR-ADD (L AC)
   (if (endp L)
	AC
	(TR-ADD (cdr L) (+ (car L) AC))))
(defun TR-MUL (L AC)
   (if (endp L)
	AC
	(TR-MUL (cdr L) (* (car L) AC))))
(defun TR-FAC (x AC)
   (if (zerop x)
	AC
	(TR-FAC (- x 1) (* x AC))))
;PART b
(defun SLOW-PRIMEP (N)
  (if (equal (mod (TR-FAC (- N 1) 1) N) (- N 1))
      t
      nil))
	  
;;;Solution to Question 16
;a
(defun TRANSPOSE1 (M)
   (cond ((null (cdr M)) (mapcar #'list (car M)))
	 (t (mapcar #'cons (car M) (TRANSPOSE1 (cdr M))))))
;b
(defun TRANSPOSE2 (M)
   (cond ((endp (cdar M)) (list (mapcar #'car M)))
	 (t (cons (mapcar #'car M) (TRANSPOSE2 (mapcar #'rest m))))))
;c
(defun TRANSPOSE3 (M)
   (apply #'mapcar #'list M))