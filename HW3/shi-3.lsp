;;CSCI-316 
;;Assignments #3
;;Cheng Shi
;;ID23375304

;;; Solution to Problem 1
(defun MIN-2 (A B) 
	(cond ((not (numberp A)) 'error)
		((not (numberp B)) 'error)
			((<= A B) A)
			((> A B) B)
	)

)
;;; Solution to Problem 2
(defun SAFE-AVG (A B)
	(cond   ((not(numberp A)) NIL)
			((not(numberp B)) NIL)
			(t (/ (+ A B) 2))
	)
)

;;; Solution to Problem 3
(defun ODD-GT-MILLION (x)
	(and (integerp x)
		(oddp x)
		(> x 1000000)
	)
)

;;; Solution to Problem 4
(defun MULTIPLE-MEMBER (A B)
	(and (Member A B)
		(Member A(CDR (Member A B)))
	)
)

;;; Solution to Problem 5
(defun MONTH->INTEGER (A)
	(COND	((equal A 'JANUARY) 1)
			((equal A 'FEBRUARY) 2)
			((equal A 'MARCH) 3)
			((equal A 'APRIL) 4)
			((equal A 'MAY) 5)
			((equal A 'JUNE) 6)
			((equal A 'JULY) 7)
			((equal A 'AUGUST) 8)
			((equal A 'SEPTEMBER) 9)
			((equal A 'OCTOBER) 10)
			((equal A 'NOVEMBER) 11)
			((equal A 'DECEMBER) 12)
			(t 'ERROR)
	)
)

;;; Solution to Problem 6

(defun SCORE->GRADE (X)
	(cond
		((not(numberp X)) NIL)
		((>= X 90) 'A)
		((AND (< X 90)(>= X 87)) 'A-)	
		((AND (< X 87)(>= X 83)) 'B+)
		((AND (< X 83)(>= X 80)) 'B)
		((AND (< X 80)(>= X 77)) 'B-)
		((AND (< X 77)(>= X 73)) 'C+)
		((AND (< X 73)(>= X 70)) 'C)
		((AND (< X 70)(>= X 60)) 'C-)
		((< X 60) 'F)
	)
)

;;; Solution to Problem 7
(defun GT (A B)
	(AND (numberp A)(numberp B)(> A B))
)

;;; Solution to Problem 8
(defun SAME-PARITY (A B)
	(AND (numberp A)
		(numberp B)
		(OR (AND (evenp A)(evenp B))(AND (oddp A)(oddp B)))
	)
)

;;; Solution to Problem 9
(defun SAFE-DIV (A  B)
	(AND(numberp A)
		(numberp B)
		(not(zerop B))
		(/ A B)
	)
)





; 'q1
; (defun test ()
; (MIN-2 21.3 7/2)
; (MIN-2 17.5 29)
; (MIN-2 5 'APPLE)
; (MIN-2 '(31) '(54)) 
; )

; 'q2
; (SAFE-AVG 23 47.4) 
; (SAFE-AVG 3 8)
; (SAFE-AVG '(23.1) 47.3)
; (SAFE-AVG 'ORANGE 'PLUM) 

; 'q3
; (ODD-GT-MILLION 92010231) 
; (ODD-GT-MILLION 17) 
; (ODD-GT-MILLION 92010232) 
; (ODD-GT-MILLION 21/5)  
; (ODD-GT-MILLION 1718671.24) 
; (ODD-GT-MILLION '(2010231)) 


; 'q4
; (MULTIPLE-MEMBER 'A '(B A B B A C A D)) 
; (MULTIPLE-MEMBER 'A '(B A B B C C A D)) 
; (MULTIPLE-MEMBER 'A '(B A B B C D)) 


; 'q5
; (MONTH->INTEGER 'MARCH) 
; (MONTH->INTEGER 'JUNE) 
; (MONTH->INTEGER 'C)
; (MONTH->INTEGER 'QUOTE)


; 'q6
; (SCORE->GRADE 86.3)  
; (SCORE->GRADE 106) 
; (SCORE->GRADE -10.1) 
; (SCORE->GRADE 59.9)  
; (SCORE->GRADE 83)  
; (SCORE->GRADE 74) 
; (SCORE->GRADE 67)  
; (SCORE->GRADE 87.0) 
; (SCORE->GRADE '(86.3)) 
; (SCORE->GRADE 'DOG) 