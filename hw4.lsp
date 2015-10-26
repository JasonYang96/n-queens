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
; add-queen (L N spot col)
; finds a col from 1 to N where a queen can be added
;
; returns index of valid spot, or nil otherwise
;
(defun add-queen (L N spot col) 
	(cond ((> spot N) nil)
		  ((check-spot L N spot col) spot)
		  (t (add-queen L N (+ spot 1)))
	)
)

;
; add-queens (L N row)
; incrementally adds queens from rows 0 to N - 1
;
; returns
;
(defun add-queens (L N row)
	(let ((spot (add-queen L N 0 row)))
		(cond ((> row N) nil)
			  ((NULL spot) nil)
			  (t (add-queens (append L (list spot)) N (+ 1 row)))
		)
	)
)

;
;
(defun QUEENS (N)
	(let ((board (add-queens nil N 0)))
		(cond ((= (length board) N) board)
			  (t nil)
		)
	)
)
