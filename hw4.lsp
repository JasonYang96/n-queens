;
; check-same-col (L)
; checks if there are duplicates in the L
;
; returns t if duplicate, nil otherwise
;
(defun check-same-col (L value)
	(>= (count value L) 1)
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
(defun check-invalid (L value)
	(or (check-same-col L value)
		(check-same-col (sum-coords (create-coordinates L 2)) (+ 1 value))
		(check-same-col (sub-coords (create-coordinates L 2)) (- 1 value))
	)
)

;
; generate-states (L)
; generates valid next states given a list L
;
; returns a list of valid states
;
(defun generate-states (L N index)
	(cond ((>= index (+ N 1)) nil)
		  ((check-invalid L index) (generate-states L N (+ 1 index)))
		  (t (cons (cons index L) (generate-states L N (+ 1 index))))
    )
)

;
;
(defun dfs (L N)
	(cond ((NULL (first L)) nil)
		  ((= (LENGTH (first L)) N) L)
		  ((dfs (generate-states (first L) N 1) N))
		  (t (dfs (rest L) N))
	)
)

;
;
(defun QUEENS (N)
	(cond ((= N 1) '(1))
		  ((or (= N 2) (= N 3)) nil)
		  (t (first (dfs (generate-states nil N 1) N)))
	)
)
