;
; sum-coords (L)
; sums each list of coords in L
;
; returns a list of sums
;
(defun sum-coords (L index)
	(cond ((NULL L) nil)
		  (t (cons (+ (first L) index) (sum-coords (rest L) (+ index 1))))
	)
)

;
; sub-coords (L)
; subtracts each list of coords in L
;
; returns a list of differences
;
(defun sub-coords (L index)
	(cond ((NULL L) nil)
		  (t (cons (- (first L) index) (sub-coords (rest L) (+ index 1))))
	)
)

;
; check-invalid (L)
; checks if state is invalid
;
; returns t if invalid, nil otherwise
;
(defun check-invalid (L value)
	(or (>= (count value L) 1)
		(>= (count (+ value 1) (sum-coords L 2)) 1)
		(>= (count (- value 1) (sub-coords L 2)) 1)
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
	(first (dfs (generate-states nil N 1) N))
)
