;*******************************************************************************
;*
;*	Representation of Abstract Objects
;*
;*******************************************************************************

;;; compiler instructions

(proclaim '(inline get-prims get-neg-prims get-concepts get-neg-concepts 
		   get-apreds get-neg-apreds
		   get-attr-fillers get-role-fillers get-cond-funcs 
		   concept-in? primitive-in? apred-in? 
		   common-primitive? common-concept? 
;		   make-role-filler make-attr-filler get-role get-filler
		   get-family-name get-prims-of-family
		   make-definition-entry get-pathes-of-definition-entry
		   get-restrictor-of-definition-entry
		   ))


(defstruct (abstract (:predicate abstract?)
		     (:print-function print-abstract) 
		     (:conc-name object-)
		     (:include object)
		     )
  (prim		nil :type list)
  (neg-prim 	nil :type list)
  (family	nil :type list)
  (neg-family	nil :type list)
  (concept	nil :type list)
  (neg-concept	nil :type list)
  (role-filler 	nil :type list)
  (attr-filler	nil :type list)
  (cond-funcs	nil :type list)
  (print-cf	nil :type list)
  (apred	nil :type list)
  (neg-apred	nil :type list)
  (msc		nil :type list)
  )


;;; constructors

(defun make-abstract-object-with-UNA ()
  (make-abstract :reference 'UNA))

;;; selectors

(defun get-prims (obj)
  (object-prim obj))
(defun get-neg-prims (obj)
  (object-neg-prim obj))

(defun get-pos-family (obj)
  (object-family obj))
(defun get-neg-family (obj)
  (object-neg-family obj))

(defun get-concepts (obj)
  (object-concept obj))
(defun get-neg-concepts (obj)
  (object-neg-concept obj))

(defun get-apreds (obj)
  (object-apred obj))
(defun get-neg-apreds (obj)
  (object-neg-apred obj))

(defun get-attr-fillers (obj)
  (object-attr-filler obj))
(defun get-role-fillers (obj)
  (object-role-filler obj))

(defun get-cond-funcs (obj)
  (object-cond-funcs obj))


;;; modifiers

(defun add-concept-erasable! (obj concept) 
  (push concept (object-concept obj)) 
  (trail! #'(lambda () (pop (object-concept obj))))
  ;(copy-trail! #'(lambda () (pop (object-concept (map-to-new-obj obj)))))
  )
(defun add-neg-concept-erasable! (obj concept)
  (push concept (object-neg-concept obj))
  (trail! #'(lambda () (pop (object-neg-concept obj))))
 ; (copy-trail! #'(lambda () (pop (object-neg-concept (map-to-new-obj obj)))))
  )

(defun add-primitive-erasable! (obj prim)
  (push prim (object-prim obj))
  (trail! #'(lambda () (pop (object-prim obj))))
  ;(copy-trail! #'(lambda () (pop (object-prim (map-to-new-obj obj)))))
  )
(defun add-neg-primitive-erasable! (obj prim)
  (push prim (object-neg-prim obj))
  (trail! #'(lambda () (pop (object-neg-prim obj))))
  ;(copy-trail! #'(lambda () (pop (object-neg-prim (map-to-new-obj obj)))))
  )

(defun add-family-erasable! (obj fam)
  (push (list fam) (object-family obj))
  (trail! #'(lambda () (pop (object-family obj))))
  ;(copy-trail! #'(lambda () (pop (object-family (map-to-new-obj obj)))))
  )
(defun add-neg-family-erasable! (obj fam)
  (push (list fam) (object-neg-family obj))
  (trail! #'(lambda () (pop (object-neg-family obj))))
  ;(copy-trail! #'(lambda () (pop (object-neg-family (map-to-new-obj obj)))))
  )

(defun reset-entry! (fam neg)
  (let ((entry (get-fam-entry fam neg))
	)
       (trail! #'(lambda () (rplacd entry (cdr entry))))
       ;(copy-trail! #'(lambda () (rplacd entry (cdr entry))))
       (rplacd entry nil)
       ))

(defun add-sprim-erasable! (obj fam prim)
  (let ((entry (get-fam-entry fam (object-family obj)))
	)
       (cond (entry
	      (trail! #'(lambda () (rplacd entry (cdr entry))))
	      (copy-trail! #'(lambda () (rplacd entry (cdr entry))))
	      (rplacd entry (list prim)))
	     (t (push (list fam prim) (object-family obj))
		(trail! #'(lambda () (pop (object-family obj))))
		;(copy-trail! #'(lambda () (pop (object-family (map-to-new-obj obj)))))
		)
	     )))

(defun add-neg-sprim-erasable! (obj fam prim)
  (let ((entry (get-fam-entry fam (object-neg-family obj)))
	)
       (cond (entry
	      (trail! #'(lambda () (rplacd entry (cdr entry))))
	      ;(copy-trail! #'(lambda () (rplacd entry (cdr entry))))
	      (rplacd entry (cons prim (cdr entry))))
	     (t (push (list fam prim) (object-neg-family obj))
		(trail! #'(lambda () (pop (object-neg-family obj))))
		;(copy-trail! #'(lambda () (pop (object-neg-family (map-to-new-obj obj)))))
		)
	     )))

(defun add-role-filler-erasable! (obj role-filler)
  (push role-filler (object-role-filler obj))
  (trail! #'(lambda () (pop (object-role-filler obj))))
  ;(copy-trail! #'(lambda () (pop (object-role-filler (map-to-new-obj obj)))))
  )
(defun add-attr-filler-erasable! (obj attr-filler)
  (push attr-filler (object-attr-filler obj))
  (trail! #'(lambda () (pop (object-attr-filler obj))))
  ;(copy-trail! #'(lambda () (pop (object-attr-filler (map-to-new-obj obj)))))
  )

(defun add-apred-erasable! (obj apred)
  (push apred (object-apred obj))
  (trail! #'(lambda () (pop (object-apred obj))))
  ;(copy-trail! #'(lambda () (pop (object-apred (map-to-new-obj obj)))))
  )
(defun add-neg-apred-erasable! (obj apred)
  (push apred (object-neg-apred obj))
  (trail! #'(lambda () (pop (object-neg-apred obj))))
  ;(copy-trail! #'(lambda () (pop (object-neg-apred (map-to-new-obj obj)))))
  )

(defun add-cond-funcs-erasable! (obj cf)
  (push cf (object-cond-funcs obj))
  (trail! #'(lambda () (pop (object-cond-funcs obj))))
  ;(copy-trail! #'(lambda () (pop (object-cond-funcs (map-to-new-obj obj)))))
  )

(defun add-printable-cond-funcs-erasable! (obj lambda-exp &rest args)
  (let ((rest-args (copy-list args))
	)
       (push (list lambda-exp rest-args) (object-print-cf obj))
       (trail! #'(lambda () (pop (object-print-cf obj))))
       ;(copy-trail! #'(lambda () (pop (object-print-cf (map-to-new-obj obj)))))
  ))


;;; Predicates

(defun subconcept-exists? (cp neg-cps)
  (or (member cp neg-cps :test #'subsumes-look-up?)
      (member cp neg-cps)))
(defun superconcept-exists? (cp cps)
  (or (member cp cps :test #'subsumed-by-look-up?)
      (member cp cps))
  )

(defun concept-exists? (cp cps)
  (member cp cps))

(defun primitive-in? (prim prims)
  (member prim prims))
(defun apred-in? (apred apreds)
  (member apred apreds :test #'equal-apreds?))

(defun common-primitive? (prims1 prims2)
  (intersection prims1 prims2))
(defun common-concept? (concs1 concs2)
  (dolist (concept concs1)
	  (member concept concs2 :test #'subsumes-look-up?)))

(defun equal-apreds? (apred1 apred2)
  (and (eq (get-pred-of-pred apred1) (get-pred-of-pred apred2))
       (equal-args? (get-all-args-of-pred apred1)
		    (get-all-args-of-pred apred2))))

(defun equal-args? (args1 args2)
  (cond ((null args1) (not args2))
	((null args2) nil)
	((equal-objects? (car args1) (car args2))
	 (equal-args? (cdr args1) (cdr args2)))
	))

(defun equal-role-fillers? (rf1 rf2)
  (and (eq (get-role rf1) (get-role rf2))
       (equal-objects? (get-filler rf1) (get-filler rf2))))

(defun role-equal-role-of-role-filler? (role role-filler)
  (eq role (get-role role-filler)))

(defun incompatible-apreds? (apreds1 apreds2)
  (intersection apreds1 apreds2 :test #'equal-apreds?))

(defun family-exists? (fam pos)
  (get-fam-entry fam pos)) 
(defun neg-family-exists? (fam neg)
  (= (length (get-fam-entry fam neg))
     1))
(defun neg-family-has-entry? (fam neg)
  (> (length (get-fam-entry fam neg))
     1))
(defun only-family-exists? (fam-slot)
  (= (length fam-slot) 1))
(defun get-family-name (fam-slot)
  (car fam-slot))
(defun get-prims-of-family (fam-slot)
  (cdr fam-slot))

(defun prim-exists? (fam prim negs)
  (find prim (get-fam-entry fam negs)))

(defun disj-prim-exists? (fam prim negs)
  (let ((entry (cadr (get-fam-entry fam negs)))
	)
       (if entry
	   (not (eq prim entry)))))

(defun all-neg-exist? (fam prim negs)
  (let ((entry (cdr (get-fam-entry fam negs)))
	)
       (= (get-number-of-sprims fam)
	  (length (remove-duplicates (cons prim entry))))))

;;; Macros

(defmacro add-prims-erasable! (prim-slot prims)
  `(let ((old-prim-slot ,prim-slot)
	)
       (trail! #'(lambda () (setf ,prim-slot old-prim-slot)))
       ;(copy-trail! #'(lambda () (setf ,prim-slot old-prim-slot)))
       (setf ,prim-slot (delete-duplicates (append ,prims ,prim-slot)))
  ))

(defmacro add-concepts-erasable! (conc-slot concepts)
  `(let ((old-conc-slot ,conc-slot)
	)
       (trail! #'(lambda () (setf ,conc-slot old-conc-slot)))
       ;(copy-trail! #'(lambda () (setf ,conc-slot old-conc-slot)))
       (setf ,conc-slot (delete-duplicates (append ,concepts ,conc-slot)))
  ))

(defmacro add-unequals-erasable! (unequal-slot unequals)
  `(let ((old-unequal-slot ,unequal-slot)
	)
       (trail! #'(lambda () (setf ,unequal-slot old-unequal-slot)))
       ;(copy-trail! #'(lambda () (setf ,unequal-slot old-unequal-slot)))
       (setf ,unequal-slot 
	     (delete-duplicates (append ,unequals ,unequal-slot) :test #'equal-objects?))
  ))

(defmacro add-fcts-erasable! (func-slot functions)
  `(let ((old-func-slot ,func-slot)
	)
       (trail! #'(lambda () (setf ,func-slot old-func-slot)))
       ;(copy-trail! #'(lambda () (setf ,func-slot old-func-slot)))
       (setf ,func-slot 
	     (delete-duplicates (append ,functions ,func-slot)))
	     ;;; functions are equal iff they are eq-equal
  ))

(defmacro add-pfcts-erasable! (func-slot functions)
  `(let ((old-func-slot ,func-slot)
	)
       (trail! #'(lambda () (setf ,func-slot old-func-slot)))
       ;(copy-trail! #'(lambda () (setf ,func-slot old-func-slot)))
       (setf ,func-slot 
	     (delete-duplicates (append ,functions ,func-slot)))
  ))

(defmacro add-apreds-erasable! (apred-slot apreds)
  `(let ((old-apred-slot ,apred-slot)
	)
       (trail! #'(lambda () (setf ,apred-slot old-apred-slot)))
       ;(copy-trail! #'(lambda () (setf ,apred-slot old-apred-slot)))
       (setf ,apred-slot 
	     (delete-duplicates (append ,apreds ,apred-slot) :test #'equal-apreds?))
  ))

(defmacro add-role-fillers-erasable! (role-slot roles)
  `(let ((old-role-slot ,role-slot)
	)
       (trail! #'(lambda () (setf ,role-slot old-role-slot)))
       ;(copy-trail! #'(lambda () (setf ,role-slot old-role-slot)))
       (setf ,role-slot 
	     (delete-duplicates (append ,roles ,role-slot) :test #'equal-role-fillers?))
  ))

;*******************************************************************************
;*
;*	
;*
;*******************************************************************************

;;; complex selectors

(defun get-attr-filler (attr obj)
  (let ((value (member attr 
		       (get-attr-fillers obj) 
		       :test #'role-equal-role-of-role-filler?))
	)
       (if value
	   (deref (get-filler (car value))))
       ))
(defun get-other-attr-filler (attr other-fillers)
  (let ((value (member attr 
		       other-fillers
		       :test #'role-equal-role-of-role-filler?))
	)
       (if value
	   (deref (get-filler (car value))))
       ))
(defun get-all-role-fillers (role obj)
  (find-all-role-fillers role (get-role-fillers obj)))
(defun find-all-role-fillers (role role-fillers)
  (let ((rest-role-fillers (member role role-fillers 
				   :test #'role-equal-role-of-role-filler?))
	)
       (cond (rest-role-fillers
	      (cons (deref (get-filler (car rest-role-fillers)))
		    (find-all-role-fillers role (cdr rest-role-fillers))))
	     )))

;;; 

(defun make-role-filler (role filler)
  (list role filler))
(defun make-attr-filler (attr filler)
  (list attr filler))
(defun get-role (rf)
  (car rf))
(defun get-filler (rf)
  (cadr rf))

;;; predicates

(defun role-exists? (obj1 role obj2)
  (member `(,role ,obj2) (get-role-fillers obj1)))

;*******************************************************************************
;*
;*	Representation of Families
;*
;*******************************************************************************

(defun get-fam-entry (fam list)
  (find fam list :test #'search-first))
(defun search-first (x y)
  (eq x (car y)))


;*******************************************************************************
;*
;*	Mapping from Lisp-Function to Definition
;*
;*******************************************************************************

(defvar *print-cf (make-hash-table))

(defun make-definition-entry (path restrictor)
  (cons path restrictor))
(defun get-pathes-of-definition-entry (entry)
  (car entry))
(defun get-restrictor-of-definition-entry (entry)
  (cdr entry))

(defun map-from-function-to-definition! (function path restrictor)
  (setf (gethash function *print-cf) (make-definition-entry path restrictor)))

(defun get-definition-from-function (function)
  (gethash function *print-cf))

