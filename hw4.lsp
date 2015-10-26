;
; check-same-col (L)
; checks if there are duplicates in the L
;
; returns t if duplicate, nil otherwise
;
(defun check-same-col (L)
	(cond ((= (length L) 1) nil)
		  ((> (count (first L) L) 1) t)
		  (t (check-same-col (rest L)))
	)
)

;
; create-coordinates (L)
;
; returns a list of coordiantes of the queens
;
(defun create-coordinates (L index)
	(cond ((NULL L) nil)
		  (t (cons (list index (first L)) (create-coordinates (rest L) (+ index 1))))
	)
)

;
; sum-coords (L)
; sums each list of coords in L
;
; returns a list of sums
;
(defun sum-coords (L)
	(cond ((NULL L) nil)
		  (t (cons (+ (first (first L)) (second (first L))) (sum-coords (rest L))))
	)
)

;
; sub-coords (L)
; subtracts each list of coords in L
;
; returns a list of differences
;
(defun sub-coords (L)
	(cond ((NULL L) nil)
		  (t (cons (- (first (first L)) (second (first L))) (sub-coords (rest L))))
	)
)

;
; check-invalid (L)
; checks if state is invalid
;
; returns t if invalid, nil otherwise
;
(defun check-invalid (L)
	(let* ((sums (sum-coords (create-coordinates L 0)))
		   (diff (sub-coords (create-coordinates L 0))))
		(or (check-same-col L) (check-same-col sums) (check-same-col diff))
	)
)

;
; generate-states (L)
; generates valid next states given a list L
;
; returns a list of valid states
;
(defun generate-states (L N index)
	(cond ((>= index N) nil)
		  (t (let* ((state (cons index L))
					(invalid (check-invalid state)))
				(cond (invalid (generate-states L N (+ 1 index)))
					  (t (cons state (generate-states L N (+ 1 index))))
				)
			 )
		 )
    )
)

;
;
(defun dfs (L N)
	(let ((states (generate-states nil N 0)))
		(cond ((NULL states) nil)

		)
	)
)

;
;
(defun QUEENS (N)
	(cond ((= N 1) '(0))
		  ((or (= N 2) (= N 3)) nil)
		  (t ())
	)
)
