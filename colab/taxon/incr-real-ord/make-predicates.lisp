;*******************************************************************************
;*
;*	 Interface between REAL-ORD and TAXON
;*
;*******************************************************************************

(defun RO-predicate->int-RO-pred (predicate)
  (cond ((eq predicate '>) 'RO->>)
	((eq predicate '<) 'RO-<<)
	((eq predicate '>=) 'RO->>=)
	((eq predicate '<=) 'RO-<<=)
	((eq predicate '=) 'RO-==)
	((eq predicate '<>) 'RO-<>)
	(t (error "Unknown predicate in Predicate->Int-Pred"))
	))

(defun negated-RO-predicate->int-RO-pred (predicate)
  (cond ((eq predicate '>) 'RO-<<=)
	((eq predicate '<) 'RO->>=)
	((eq predicate '>=) 'RO-<<)
	((eq predicate '<=) 'RO->>)
	((eq predicate '=) 'RO-<>)
	((eq predicate '<>) 'RO-==)
	(T (error "Unknown predicate in Negated-Predicate->Int-Pred"))
	))

(defun RO-predicate? (pred)
  (member pred '(> < >= <= = <>)))

(setf (get 'equal-rule 'RO) 'RO-==)
(setf (get 'unequal-rule 'RO) 'RO-<>)
