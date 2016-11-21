;*******************************************************************************
;*
;*	Representation of Objects
;*
;*******************************************************************************

;;; compiler instructions

(proclaim '(inline get-unequals get-id get-reference get-dereferenced-id 
		   equal-objects? object-in? 
		   ))

(defstruct (object (:conc-name object-)
		   (:predicate object?)
		   )
  (reference 	'NUNA) 
  (unequals	nil :type list)
  (id		nil)
  )

;;; selectors

(defun get-unequals (obj)
  (object-unequals obj))
(defun get-id (obj)
  (object-id obj))
(defun get-reference (obj)
  (object-reference obj))
(defun get-dereferenced-id (obj)
  (object-id (deref obj)))

;;; predicates

(defun equal-objects? (obj1 obj2)
  (eq (deref obj1) (deref obj2)))

(defun object-in? (obj list)
  (member obj list :test #'equal-objects?))

(defun UNA? (obj)
  (eq 'UNA (object-reference (deref obj))))

;;; modifier

(defun add-unequal-erasable! (obj unequal)
  (push unequal (object-unequals obj))
  (trail! #'(lambda () (pop (object-unequals obj))))
  (copy-trail! #'(lambda () (pop (object-unequals (map-to-new-obj obj)))))
  )
