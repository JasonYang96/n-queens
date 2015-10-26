;
; find-conflicts (L N index values)
; checks if any values are at index of L
;
; returns t if there exists a conflict, nil otherwise
;
(defun find-conflicts (L index values) 
	(let ((found (nthcdr index L))) 
		((find found values) t)
	)
)

;
; check-spot (L N spot col)
; checks if queen can go into spot in L
;
; returns t if valid spot, nil otherwise
;
(defun check-spot (L N spot col)
	(cond )
)

;
; add-queen (L N spot col)
; finds a spot from 1 to N where a queen can be added
;
; returns index of valid spot, or nil otherwise
;
(defun add-queen (L N col spot) 
	(cond ((> spot N) nil)
		  ((check-spot L N spot col) spot)
		  (t (add-queen L N (+ spot 1)))
	)
)

;
;
(defun add-queens (L N col)
	(let ((spot (add-queen L N col 0)))
		(cond ((> col N) nil)
			  ((NULL spot) nil)
			  (t (append L (list spot)))
		)
	)
)
