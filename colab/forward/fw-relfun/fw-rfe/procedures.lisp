;;;; used to define instructions, used in procedures.asm

(definstr not-member (A1 A2)(NAT NAT)  ; A1 --> areg. pts to element
                                       ; A2 --> areg. pts to list
  :standard
  (gwam.not-member 
      (let ((elem (deref (argument-reg A1)))
	    (list (deref (argument-reg A2))))
	(if (l-member-p elem list)
	    (fail)
	  ))))


(defun l-member-p (elem list)
  (cond ((nil-cell? list) nil)
	((varp list) t)
	((varp elem) t)
	((varp (deref (mem list))) t)
	((not (and (eq (word-tag elem)
		       :const)
		   (eq (word-tag (deref (mem list)))
		       :const)))
	 (gerror "procedures" "l-member-p" "only CONSTANTS are allowed ..."))
	((word-equal  elem (mem list)) t)
	( t (l-member-p elem (mem (ref-plus list 1))))))

#|
(defun ny-unify-p (ref1 ref2)    ;; predicate, no side-effects
  (let ((res (ny-unify-p-aux ref1 ref2)))
    (tidy-fail)
    res))
  
(defun ny-unify-p-aux (ref1 ref2)
  (cond  ((equal ref1 ref2) t)
         ((and (varp ref1)(varp ref2))
	  (if (ref-lessp ref1 ref2)
	      (bind ref2 ref1)
	    (bind ref1 ref2))
	  t)
	 ((varp ref1)
	  (bind ref1 ref2)
	  t)
	 ((varp ref2)
	  (bind ref2 ref1)
	  t)
	 ((not (eq (word-tag ref1) (word-tag ref2)))
	  nil)
	 (t (case (word-tag ref1)
		  ((:const)
		   (if (equal (word-value ref1)
			      (word-value ref2))
		       t
		     nil))
		  ((:struct)
		   (ny-unify-structures-p (mem ref1) (mem ref2) ref1 ref2))
		  ((:list)
		   (ny-unify-p-aux (deref (mem ref1))
				   (deref (mem ref2)))
					;(ny-unify (mem (ref-plus ref1 1)) 
					;	     (mem (ref-plus ref2 1))))))))
		   (ny-unify-p-aux (deref (mem (ref-plus ref1 1)))    ; derefence cdr
				   (deref (mem (ref-plus ref2 1))))))))) ; M.S. 6/92 


(defun ny-unify-structures-p (fun1 fun2 ref1 ref2)
  (if (not (equal (word-tag fun1) (word-tag fun2)))
      nil)
  (case (word-tag fun1)
	((:fun)
	 (if (equal (word-value fun1) (word-value fun2))
	     (let ((arity (second (word-value fun1))))
	       (do ((i arity (1- i)))
		   ((or (zerop i)
			(not (ny-unify-p-aux
			      (deref (ref-plus ref1 i))
			      (deref (ref-plus ref2 i)))))
		    
	   nil))))


(defun tidy-fail ()    ;;;; only an unwind trail
  (let ((temp (reg TR)))
    (set-reg TR (mem (ref-plus (reg B) (offset TR1))))
    (unwind-trail temp (reg TR))))

|#
