;;; Making Assertions
;;; 29-06-92 aa
;;; uses: Syntax of Assertions (absynt)
;;;       Assert-Functions over Constraint Net
;;; 

;;; VORSICHT: das Benutzen von Zahlen als Individuen
;;;           ist erst PROVISORISCH in Attributfuellern erlaubt,
;;;           sonst kanns noch krachen

(defmacro asse (a)
   `(fasse ',a))

(defun fasse (a &key (visible? T))
   (cond ((parse-membership-assertion a)
             (abox-insert :intern 
			  #'(lambda ()
				    (make-membership-assertion 
				     (implant-rules (shift-not (get-conc-of-member-asse a))) 
				     (create-abstract-ind!     (get-ind-of-member-asse a))
				     ))
                          :extern a
                          :visible? visible?
                          :type 'membership-assertion)
           )

          ((parse-apred-instantiation a)
	   (abox-insert :intern 
			#'(lambda ()
				  (make-abstract-predicate-assertion  
				   (cons (get-pred-of-pred-instantiation a) 
					 (mapcar #'aa-create-unknown-ind-as-struct! 
						 (get-ind-list-of-pred-instantiation a)))))
			:extern a
                        :visible? visible?
			:type 'abstract-predicate-instantiation)
	   )
           ((parse-cpred-instantiation a)
	    (abox-insert :intern
			 #'(lambda ()
				   (make-concrete-predicate-assertion  
				    (cons (get-pred-of-pred-instantiation a) 
					  (mapcar #'create-concrete-ind-or-number! 
						  (get-ind-list-of-pred-instantiation a)))))
			 :extern a
                         :visible? visible?
			 :type 'concrete-predicate-instantiation)
	    ) 
           ((parse-equal-assertion a)
	    (abox-insert :intern 
			 #'(lambda ()
				   (make-equation-assertion 
				    (aa-create-unknown-ind-as-struct!   (get-ind1-of-equal-asse a)) 
				    (aa-create-unknown-ind-as-struct!   (get-ind2-of-equal-asse a))))
			 :extern a
                         :visible? visible?
			 :type 'equation-assertion)
	    )
           ((parse-unequal-assertion a)
	    (abox-insert :intern  
			 #'(lambda ()
				   (make-negated-equation-assertion 
				    (aa-create-unknown-ind-as-struct!   (get-ind1-of-equal-asse a)) 
				    (aa-create-unknown-ind-as-struct!   (get-ind2-of-equal-asse a))))
			 :extern  a
                         :visible? visible?
			 :type 'negated-equation-assertion)
	    )
         ((parse-role-filler-assertion a)
	  (let ((filler (get-ind2-of-filler-asse a)))
	       (if (numberp filler)
		   (let ((name (make-number filler)))
			(fasse (list (get-ind1-of-filler-asse a)
				     (get-rel-of-filler-asse a)
				     name) :visible? NIL)
			(fasse (list '= name filler) :visible? NIL)
			(abox-insert :intern nil
				     :extern a
				     :visible? T
				     :type 'role-filler-assertion)
			)
		   (abox-insert :intern 
				#'(lambda ()
					  (make-role-filler-assertion 
					   (first (get-rel-of-filler-asse  a))
					   (aa-create-unknown-ind! (get-ind1-of-filler-asse a))
					   (aa-create-unknown-ind!   (get-ind2-of-filler-asse a))))
				:extern a
				:visible? visible?
				:type 'role-filler-assertion)
		   )
	       )
	  )
          ((parse-attr-filler-assertion a)
	   (let ((filler (get-ind2-of-filler-asse a)))
		(if (numberp filler)
		    (let ((name (make-number filler)))
			 (fasse (list (get-ind1-of-filler-asse a) 
				      (get-rel-of-filler-asse a)
				      name) :visible? NIL) 
			 
			 (fasse (list '= name filler) :visible? NIL)
			 (abox-insert :intern nil
				      :extern a
				      :visible? T
				      :type 'attr-filler-assertion)
			 )
		    (abox-insert :intern 
				 #'(lambda ()
					   (make-attr-filler-assertion   
					    (first    (get-rel-of-filler-asse  a)) 
					    (aa-create-unknown-ind!   (get-ind1-of-filler-asse a)) 
					    (if (number? filler)
						(create-concrete-ind! filler 'RO)
						(aa-create-unknown-ind! filler))))
				 :extern a
				 :visible? visible?
				 :type 'attr-filler-assertion)
		    )))
	 ((parse-complex-filler-assertion a)
	  (expand-filler-assertion a)
	  )
         (t (princ (format nil "Assertion ~A ignored, syntax error!!" a)))
        )
  T
)

                                         
(defun implant-rules (ct)
  (if (negation? ct)
      (make-neg-conceptname-rule (get-CNF-of-conceptname (get-concept-of-negation ct)))
      (make-conceptname-rule (get-CNF-of-conceptname ct))
  )
)

(defvar *msc-table (make-hash-table))
(defun set-msc! (ind list)
  (setf (gethash ind *msc-table) list))

(defun get-msc (ind)
  (gethash ind *msc-table))

(defvar *individual-table* (make-hash-table))

(defun get-object (name)
  (gethash name *individual-table*))
(defun get-dereferenced-object (name)
  (deref (gethash name *individual-table*)))


(defun clear-individual-table ()
  (clrhash *individual-table*))

(defun show-model ()
  (maphash #'print-model *individual-table*))	

(defvar *print-new nil)

(defun print-model (key value)
  (if *print-new
      (print (deref value))
      (unless (new? value)
	      (print (deref value))
	      )
      ))


(defun create-abstract-ind! (item)
 (let ((old (gethash item *individual-table*)))
    (if old
        old
        (setf (gethash item *individual-table*)
              (make-abstract :id item))
    )
  )
)

(defun aa-create-unknown-ind-or-number! (i)
   (if (numberp i)
       i
       (create-unknown-ind! i)))

(defun create-concrete-ind-or-number! (i)
  (if (numberp i)
      i
      (create-concrete-ind! i 'RO)
      ))

(defun aa-create-unknown-ind! (i)
   (if (numberp i)
       i
       (create-unknown-ind! i)))

(defun aa-create-unknown-ind-as-struct! (i)
   (if (numberp i)
       (let ((name (gentemp "concindi")))
          (let ((result (create-unknown-ind! name)))
            ; (fcpred (list (format nil "ro=~A" i) (list 'ro '(x) (list '= 'x i))))
              (fasse (list '= name i))
              result))
       (create-unknown-ind! i)))

             



(defun create-unknown-ind! (item)
 (let ((old (gethash item *individual-table*)))
    (if old
        old
        (setf (gethash item *individual-table*)
              (make-unknown :id item))
    )
  )
)


(defun membership-assertion? (entry) 
   (eq 'membership-assertion (abox-entry-type entry)))

(defun equational-assertion? (a)
   (member (abox-entry-type a) '(equation-assertion negated-equation-assertion)))

(defun role-filler-assertion? (entry) 
   (eq 'role-filler-assertion (abox-entry-type entry)))

(defun attribute-filler-assertion? (entry) 
   (eq 'attr-filler-assertion (abox-entry-type entry)))

(defun apred-instantiation? (entry) 
   (eq 'abstract-predicate-instantiation (abox-entry-type entry)))

(defun cpred-instantiation? (entry) 
   (eq 'concrete-predicate-instantiation (abox-entry-type entry)))

(defvar *number-counter 0)
(defun clear-number-counter ()
  (setf *number-counter 0))

(defun make-number (number)
  (format nil "Number~A=~A" (incf *number-counter) number))

(defun number? (string)
  (and (stringp string) (eq (aref string 0) #\N) (eq (aref string 1)
  #\u)))

