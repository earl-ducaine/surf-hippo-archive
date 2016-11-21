;*******************************************************************************
;*
;*	File : Get-Rules
;*
;*******************************************************************************

(defun get-concrete-equal-rule (domain)
  (get 'equal-rule domain))


(defun get-concrete-unequal-rule (domain)
  (get 'unequal-rule domain))

#|
(defun get-concrete-predicate (domain pred)
  (cond ((eq domain 'RO)
	 (predicate->int-pred pred))
	(t (error "Unknown DOMAIN in Get-Concrete-Predicate"))
	))

(defun get-concrete-domain-entry (concrete-object)
  (cond ((eq (object-domain concrete-object) 'RO)
	 (get-RO-object concrete-object))
	(t (error "Unknown DOMAIN in Get-Concrete-Domain-Entry"))
	))
|#
