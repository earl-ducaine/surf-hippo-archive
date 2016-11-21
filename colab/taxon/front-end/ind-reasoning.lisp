                   
(defvar *trace-realize  nil)
(defvar *trace-concept-closure nil)

;;; realization
;;; 10-08-92 aa

(defmacro realize (i &optional (roots '(top)))
   `(frealize ',i ',roots)
)
 


#|
(defun frealize (i &optional (roots '(top)))
  (let ((nodes 
	 (compute-msn  (mapcar 'get-node-of-item roots)
		       #'(lambda (x) (let ((concept (if (listp (node-value x))
							(first (node-value x))
							(node-value x))))
					  (finstance? i concept)))
		       )
	 ))
       ;(mapcar #'node-value nodes)
       (mapcar #'(lambda (x) (car (node-value x))) nodes)
       )
  )
|#

(defun frealize (i &optional (roots '(top))) 
  (reduced-frealize i roots))

(defun frealize-only-if-necessary (i &optional (roots '(top)))
  (cond ((get-msc i))
	(t (frealize i roots))
	))

(defun la (abox) (mapcar #'print-abox-entry abox))

(defun reduced-frealize (i &optional (roots '(top)))
  (let* ((rabox  (remove-if #'(lambda (x) (null (abox-entry-intern x)))
                (reduce-abox *abox-table* i)))
        ; (forgetit (la rabox))
	 (nodes 
	  (compute-msn  
	   (mapcar 'get-node-of-item roots)
	   #'(lambda (x) 
		     (let ((concept (if (listp (node-value x))
					(first (node-value x))
					(node-value x)))
			   )
			  (not (progn 
				(CLEAR-individual-table)
				(satisfy-abox 
				 (mapcar 
				  #'(lambda (x) (funcall (abox-entry-intern x)))
				  (reverse
				   (cons 
				    (make-abox-entry 
				     :intern
				     #'(lambda 
					()
					(make-membership-assertion
					 (implant-rules 
					  (shift-not 
					   (MAKE-Negation concept)))
					 (create-abstract-ind!     i)))
				     )
				    rabox))))))))))
	 )
       ;	(print (list 'length (length rabox)))
	(set-msc! 
	 i
	 (remove-if #'null 
		    (mapcar #'(lambda
			       (x) 
			       (find-if #'visible-item?
					(node-value x)))
			    nodes)))))

(defmacro instance? (i concept)
  `(finstance? ',i ',concept)
  )


#|
(defun finstance? (i concept)
   
   (not (fcompatible? (membership-assertion! (make-negation concept) i)))
)
|#

;; ATTENTION PLEASE: now finstance looks up and does not compute anything

(defun finstance? (i concept)
   (member concept (concept-closure i)))


(defmacro compatible? (assertion)
   `(fcompatible? ',assertion)
)


(defun fcompatible? (assertion)
   (fasse assertion)
   (let ((result (check-abox)))
      (prog1 result
             (cut-abox))
   )
)


(defmacro weakly-realize (i &optional (roots '(top)))
  `(fweakly-realize ',i ',roots))


(defun fweakly-realize (i &optional (roots '(top)))
  (let ((nodes 
              (compute-msn  (mapcar #'(lambda (x)
                                              (get-node-of-item x))
                                    roots)
                           #'(lambda (x) (let ((concept (if (listp (node-value x))
                                                            (first (node-value x))
                                                            (node-value x))))
                                               (fcompatible? (membership-assertion! concept i))))
              )
        ))
    (mapcar #'node-value nodes)
  )
)



(defun assert-ind? (ind concept attr_value_pairs)
  ;;; ATTR_VALUE_PAIRS --> knowledgebase
  ;;; IND : CONCEPT    --> knowledgebase
  ;;; then realize IND                   
  
  ;; NOT YET: schauen, ob ind neu
  ;;          realize-result in Datenstruktur
  ;;          Konsistenz der Zusicherung ueberpruefen
  
  (fmake-indi (list ind))
  (fasse (membership-assertion! concept ind))
  (dolist (avp (if (eq 'user::tup (car attr_value_pairs))
		   (cdr attr_value_pairs) attr_value_pairs)
	       (reduced-frealize ind (list concept)))
	  (fasse 
	   (let ((role (if (eq 'user::tup (first avp))
			   (second avp)
			   (first avp)))
		 (filler (if (eq 'user::tup (first avp))
			     (third avp)
			     (second avp)))
		 )
		(make-filler-assertion (list role) ind filler)
		)
	   )
	  )
  )






(defun realize-individual (ind)       ;; soll hier nachgekuckt oder gerechnet werden ???
  (if *trace-realize                  ;; diese Variable gibts noch nicht !!, s.o. 
      (time (frealize ind))
      (frealize ind)))

(defun realize-ind (ind)
  (if *trace-realize                  ;; was? nachkucken oder rechnen??
      (time (frealize ind))            ;; hier soll noch das preprocessing rein !!
      (frealize ind)))

(defun realize-individual-incomplete (ind concept)
  (if *trace-realize                                  ;; incomplete fehlt noch !!
      (time (frealize ind))                           ;; preprocess fehlt noch !!
      (frealize ind)))


#|        ;; fehlerhaft und ungeschickt!!
(defun concept-closure (ind)                          ;; nachschauen in Datenstrukturen fehlt !!       
  ;;; get ALL concepts which have IND as an instance
  ;; *trace-concept-closure fehlt !!
  (let ((initial-concept-set (frealize-only-if-necessary ind))
	(result-nested nil)
	(result-list nil))
       (dolist (concept initial-concept-set (dolist (conc-list 
						     (remove-duplicates (mapcar #'node-value result-nested))
						     ;(union-nested-list-to-list result-list initial-concept-set))
						    (remove-if #'null 
							     ;  (remove-if-not #'visible-item?
									      (union-nested-list-to-list result-list initial-concept-set))) ;)
			
			(setf result-list (union result-list conc-list))
			))
       ; (setf result-nested (union result-nested (get-uppers (car concept)))))))
(setf result-nested (union result-nested (get-uppers  concept))))))



(defun union-nested-list-to-list (l nl)
  (let ((result l))
       (dolist (memberliste nl result)
            (if (symbolp memberliste)
                (setf result (adjoin memberliste result))
	       (setf result (union result memberliste))))))


|#

(defun concept-closure (ind)
  (flat
   (mapcar #'node-value
           (nodes-all-uppers (mapcar #'get-node-of-item 
                                    (frealize-only-if-necessary ind)))))) 
