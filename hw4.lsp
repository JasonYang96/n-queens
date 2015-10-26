;
; find-conflicts (L N index values)
; checks if any values are at index of L
;
; returns t if there exists a conflict, nil otherwise
;
(defun find-conflicts (L index values)
	(cond ((> (count (nthcdr index L) values) 0) t)
		  (t nil)
	)
)

;
; check-spot (L N spot col)
; checks if queen can go into spot in L
;
; returns t if valid spot, nil otherwise
;
(defun check-spot (L N spot col)
	(cond ((> spot N) nil)
		  (())
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
