;;; exp-pred: allows new concrete predicates defined in terms of 
;;;           previously defined ones
;;;           copied from /home/colab/taxon.alone/front-end/exp-pred
;;; 24-08-92 aa
;;; ATTENTION PLEASE: this file knows the syntax of a predicate definition

(defun expand-pred (def)
	(list (first def)
              
              (_expand-pred (second def))))

(defun _expand-pred (p-term)
	(if (atom p-term)
	    (_exp-term-item p-term)
            (mapcar #'_exp-term-item p-term)))

(defun _exp-term-item (t-item)
	(declare (special tbox-table))
	(if (atom t-item)
	    t-item
	    (let ((entry (get-definition (car t-item))))
		 (if (null entry)
		     (cons (car t-item) 
                           (_expand-pred (cdr t-item)))
                     (let ((dt (second entry)))
		          (_expand-pred (bind-arg-values (get-term-of-cpred-definition dt)
						       	(get-args-of-cpred-definition dt)
							(cdr t-item)))))))  )

(defun bind-arg-values (term args values)
       (sublis (mapcar #'cons args values) term))
