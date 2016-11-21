
;;; Constructors Selectors and Predicates handling
;;; the Abstract Syntax of Taxon3

;;; last change: 17-06-92 aa: no conceptname? test for restrictor
;;;              in some-frp-C-restriction? etc.
;;;              17-06-92 aa: function term? for restrictors
;;;              17-06-92 aa: abstract predicates and primitive concepts are
;;;              distinguished using diiferent fu.s to define
  

;;; (apred pred prim sprim conc attr role)' s are
;;; declared in the tbox-table

(defun conceptname? (item)
  (or (eq (get-type-of-item item) 'conc)
      (created-conceptname? item))
  )

(defun prim? (item)
	(eq (get-type-of-item item)
            'prim
        )
)

(defun apred? (item)
	(eq (get-type-of-item item)
            'apred
        )
)

(defun cpred? (item)
	(eq (get-type-of-item item)
            'cpred
        )
)


(defun sprim? (item)
	(eq (get-type-of-item item)
            'sprim
        )
)

(defun attr? (item)
	(eq (get-type-of-item item)
            'attr
        )
)

(defun role? (item)
	(eq (get-type-of-item item)
            'role
        )
)

(defun open-family? (item)
	(eq (get-type-of-item item)
            'ofam
        )
)

(defun closed-family? (item)
	(eq (get-type-of-item item)
            'cfam
        )
)

(defun subfamily? (item)
  (eq (get-type-of-item item) 'sfam))


(defun family? (item)
   (or (closed-family? item)
       (open-family? item)
   )
)

(defun pconc? (item)
   (get-pconc?-of-item item)
)



;; an arbitrary predicate belongs either to the abstract or to the concrete domain

(defun predicate? (item)
       (or (cpred? item)
           (apred? item)
        )
)

;;;

(defun functional? (role)
  (attr? role)
  )

(defun relational? (role)
  (role? role)
  )

(defun relation? (item)
  (or (role? item)
      (attr? item)
      )
  )

;;; (agree    attr-chain)
;;; (disagree attr-chain)

(defun agreement? (conceptterm)
    (and (listp conceptterm)
       	(eq 'agree
            (first conceptterm)
        ))
)

(defun disagreement? (conceptterm)
    (and (listp conceptterm)
	(eq 'disagree
            (first conceptterm)
        ))
)

(defun make-agreement (l) 
	(cons 'agree l)
)

(defun make-disagreement (l) 
	(cons 'disagree l)
)


(defun agree-with-empty-path? (ct)
   (and (agreement? ct)
        (or (empty-path? (get-first-path-of-agreement ct))
            (empty-path? (get-second-path-of-agreement ct))
         )
   )
)

(defun disagree-with-empty-path? (ct)
   (and (disagreement? ct)
        (or (empty-path? (get-first-path-of-agreement ct))
            (empty-path? (get-second-path-of-agreement ct))
         )
   )
)




(defun get-first-path-of-agreement (a)
   (second a))

(defun get-second-path-of-agreement (a)
  (third a))

(defun get-pathes-of-agreement (a)
  (rest a))


;;; definition of concrete predicates 
;;; (name (domain (arglist) (term)))

(defun get-edom-of-cpred-definition (dt) (first dt))
(defun get-args-of-cpred-definition (dt) (second dt))
(defun get-term-of-cpred-definition (dt) (third dt))


;;; (and c d e f ...)
;;; (or  c d e f ...)
;;; (not c)

(defun conjunction? (ct)
   (and (listp ct)
        (eq 'and (get-connective ct))))
(defun disjunction? (ct)
   (and (listp ct)
        (eq 'or (get-connective ct))))
(defun negation? (ct)
   (and (listp ct)
        (eq 'not (get-connective ct))))


(defun make-negation (c)
   (list 'not c))
(defun make-conjunction (clist)
   (cons 'and clist))
(defun make-disjunction (clist)
   (cons 'or clist))


(defun get-concepts-of-conjunction (ct)
  (rest ct)
  )


(defun get-concepts-of-disjunction (ct)
  (rest ct)
  )


(defun get-concept-of-negation (ct)
  (second ct)
  )

(defun get-first-concept (ctlist)
  (first ctlist)
  )

(defun get-rest-concepts (ctlist)
  (rest ctlist)
  )


;;; old

(defun get-first-term (term)
	(second term)
)

(defun get-second-term (term)
	(third term)
)

(defun get-connective (term)
	(first term)
)

;;; generalized quantification
;;; allowed restrictors: abstract/concrete predicates, concept terms

(defun restriction? (conceptterm)
       (or (value-restriction? conceptterm)
	   (exists-in-restriction? conceptterm)
       )
)

(defun make-qp-restriction (q pl r)
   (list q pl r))


(defun value-restriction? (conceptterm)
       (and (listp conceptterm)
            (eq 'forall (first conceptterm))
       )
)

(defun make-value-restriction (list-of-chains restrictor)
       (list 'forall list-of-chains restrictor)
)

(defun exists-in-restriction? (conceptterm)
       (and (listp conceptterm)
            (eq 'some (first conceptterm))
       )
)

(defun make-exists-in-restriction (list-of-chains restrictor)
       (list 'some list-of-chains restrictor)
)


(defun concept-term? (ct)
   (or (and (not (listp ct))
            (or (prim? ct)
                (sprim? ct)            
                (conceptname? ct)
                (open-family? ct)
                (closed-family? ct)
                (subfamily? ct)
               ; (prim-of-of? ct)
               ; (prim-of-cf? ct)
                    )
         )
        (and (listp ct)
             (member (get-connective ct) '(and or not some forall agree disagree))
        )
    )
)
                            

;;;

(defun abstract-predicate-restriction? (restriction)
  (and (restriction? restriction)
       (apred? (get-restrictor restriction)))
  )

(defun concrete-predicate-restriction? (restriction)
  (and (restriction? restriction)
       (predicate? (get-restrictor restriction)))
  )


(defun forall-frp-C-restriction? (restriction)            ;; -C- restriction? ohne C-test?
  (and (value-restriction? restriction)
       (concept-term? (get-restrictor restriction))
       (eval (cons 'and 
	       (mapcar #'attr? 
		       (get-path-of-conceptname restriction)))
	))   
  )


(defun forall-rp-C-restriction? (restriction)
  (and (value-restriction? restriction)
       (concept-term? (get-restrictor restriction))
	   (eval (cons 'and 
		       (mapcar #'(lambda (x) (or (attr? x) (role? x)))
			       (get-path-of-conceptname restriction)))
	   ))
  )


(defun some-frp-C-restriction? (restriction)
  (and (exists-in-restriction? restriction)
       (concept-term? (get-restrictor restriction))
	   (eval (cons 'and 
		       (mapcar #'attr? 
			       (get-path-of-conceptname restriction)))
	))   
  )



(defun some-rp-C-restriction? (restriction)
  (and (exists-in-restriction? restriction)
       (concept-term? (get-restrictor restriction))
	   (eval (cons 'and 
		       (mapcar #'(lambda (x) (or (attr? x) (role? x)))
			       (get-path-of-conceptname restriction)))
	   ))
  )


;;;


(defun get-quantifier (restriction)
  (car restriction)
  )

(defun some? (quantifier)
  (eq 'some quantifier)
  )

(defun forall? (quantifier)
  (eq 'forall quantifier)
  )

(defun make-quantifier-some () 'some)
(defun make-quantifier-forall () 'forall)

(defun negate-quantifier (quantifier)
  (if (some? quantifier)
      'forall
      'some)
  )




(defun get-restrictor (restriction)
   (let ((r (car (last restriction))))
      (if (listp r) 
          (second r)
          r)
   )
)

(defun negated-restrictor? (restriction)
    (and (restriction? restriction)
         (negation? (car (last restriction)))))


(defun get-pathes-of-restriction (conceptterm)
       (if (or (agreement? conceptterm) (disagreement? conceptterm))
           (cdr conceptterm)
       (cdr (reverse (cdr (reverse conceptterm))))
       )
)

(defun get-path-of-conceptname (restriction)
  (first (get-pathes-of-restriction restriction))
  )

(defun get-pathes-of-predicate-restriction (restriction)
  (get-pathes-of-restriction restriction)
  )

(defun get-first-path-of-pathes (paths)
  (first paths)
  )

(defun get-rest-paths-of-pathes (paths)
  (rest paths)
  )

(defun get-rest-path (path)
  (rest path)
  )

(defun get-first-relation-of-path (path)
  (first path)
  )



(defun get-rest-relations-of-path (path)
  (rest path)
  )

(defun get-first-relation-list (pathlist)
  (if (listp (first pathlist))
      (mapcar #'(lambda (x)  (first x))
	      pathlist)
      (list (first pathlist))
      ))


;;; parse concept term for syntactic correctness


(defun parse-conc (ct)
   
  
   (if (tbox-member? ct)

        #|
    
        (cond  ((sprim? ct))

               ((and (prim? ct)))

               ((conceptname? ct))
 
               ((or (open-family? ct) (closed-family? ct))) 
         )
         |#
       (or (prim? ct)
	   (sprim? ct)
	   (conceptname? ct)
	   (open-family? ct)
	   (closed-family? ct)
	   (subfamily? ct))

       (if (listp ct)

       (let ((ct1 (first ct))
        (ct2 (second ct)) 
        (ct3 (third ct))
        (ctr (rest ct))
        (ctl (car (last ct)))
        (ctll (last ct))
        (l (length ct)))
  
     (cond 
          

         ((and (eq 'not ct1)
               (= l 2)
               (parse-conc ct2)))

         ((and (eq 'and ct1)
               (eval (cons  'and
                      (mapcar #'parse-conc
                              ctr))
                )
           ))

         ((and (eq 'or ct1)
               (eval (cons 'and
                      (mapcar #'parse-conc
                              ctr))
                )
           ))

          ((and (eq 'agree ct1)
                  (= l 3)
                  (parse-attr-chain ct2)
                  (parse-attr-chain ct3)))

            ((and (eq 'disagree ct1)
                  (= l 3)
                  (parse-attr-chain ct2)
                  (parse-attr-chain ct3)))

         
            ((and (eq 'forall ct1)
                (predicate? ctl)
                (let ((list-of-role-attr-chains (set-difference ctr              
                                                           ctll))    ;;Problem moeglich
                      (arity (tbox-entry-arity (get-tbox-entry ctl))))
                
                     (and (= (length list-of-role-attr-chains)
                             arity)
                          (eval (cons 'and
                                 (mapcar #'parse-role-attr-chain
                                         list-of-role-attr-chains)))))))
             
            ((and (eq 'some ct1)
                (predicate? ctl)
                (let ((list-of-role-attr-chains (set-difference ctr              
                                                           ctll))    ;;Problem moeglich
                      (arity (tbox-entry-arity (get-tbox-entry ctl))))
                     (and (= (length list-of-role-attr-chains)
                             arity)
                          (eval (cons 'and
                                 (mapcar #'parse-role-attr-chain
                                         list-of-role-attr-chains)))))))

            ((and (eq 'forall ct1)
                (negated-apred? ctl)
                (let ((list-of-role-attr-chains (set-difference ctr              
                                                           ctll))    ;;Problem moeglich
                      (arity (tbox-entry-arity (get-tbox-entry (second ctl)))))
                
                     (and (= (length list-of-role-attr-chains)
                             arity)
                          (eval (cons 'and
                                 (mapcar #'parse-role-attr-chain
                                         list-of-role-attr-chains)))))))
             
            ((and (eq 'some ct1)
                (negated-apred? ctl)
                (let ((list-of-role-attr-chains (set-difference ctr              
                                                           ctll))    ;;Problem moeglich
                      (arity (tbox-entry-arity (get-tbox-entry (second ctl)))))
                     (and (= (length list-of-role-attr-chains)
                             arity)
                          (eval (cons 'and
                                 (mapcar #'parse-role-attr-chain
                                         list-of-role-attr-chains)))))))


          ((and (eq 'forall ct1)
                (= l 3)
                (parse-role-attr-chain ct2)
                (parse-conc ct3)))
                
          ((and (eq 'some ct1)
                (= l 3)
                (parse-role-attr-chain ct2)
                (parse-conc ct3)))
              
            

            )

          
         
))))



(defun negated-apred? (x)
   (and (negation? x)
        (apred? (second x))))

(defun parse-role-attr-chain (c)           ;; Kette muss jetzt geklammert sein
   (cond  ((null c) T)
          
         
         (T (and (or (role? (first c))
                     (attr? (first c)))
                 (parse-role-attr-chain (rest c))))
    )
)

(defun parse-attr-chain (c)    
   (cond ((null c) T)
 
         (T (and (attr? (first c))
                 (parse-attr-chain (rest c))))
    )
)

(defun empty-path? (path)
    (null path)
    )

(defun empty-pathes? (pathlist)
   (null pathlist)
)

(defun first-path (pathlist)
   (first pathlist)
)

(defun rest-pathes (pathlist)
   (rest pathlist)
)



(defun make-empty-path ()
   '()
)


;;; ABox Reasoning
;;; 1.) Syntax of Assertions

(defun parse-membership-assertion (a)
   (and (listp a)
        (let ((a1 (first a)))
           (parse-conc a1)
        )
   )
)

(defun membership-assertion! (concept i)
   (list concept i)
)

(defun get-conc-of-member-asse (a)
   (first a)
)

(defun get-ind-of-member-asse (a)
   (second a)
)

(defun parse-complex-filler-assertion (a)         ;; erlaube momentan nur Pfade der Laenge 1
   (and (listp a)
        (= 3 (length a))
        (let ((a2 (second a)))
           (and
           (parse-role-attr-chain a2)
           (> (length a2) 1)
           )
        )
    )
)


(defun parse-role-filler-assertion (a)         ;; erlaube momentan nur Pfade der Laenge 1
   (and (listp a)
        (= 3 (length a))
        (let ((a2 (second a)))
           (and
           (parse-role-attr-chain a2)
           (= (length a2) 1)
           (role? (first a2))
           )
        )
    )
)


(defun parse-attr-filler-assertion (a)      ;; siehe oben
   (and (listp a)
        (= 3 (length a))
        (let ((a2 (second a)))
           (and
           (parse-role-attr-chain a2)
           (= (length a2) 1)
           (attr? (first a2))
           )
        )
    )
)

(defun parse-apred-instantiation (a)
   (and (listp a)
        (apred? (first a))
        (= (length (rest a)) 
           (get-arity-of-pred (first a))))
)

(defun get-pred-of-pred-instantiation (a)
   (first a))

(defun get-ind-list-of-pred-instantiation (a)
   (rest a))

(defun parse-cpred-instantiation (a)
   (and (listp a)
        (cpred? (first a))
        (= (length (rest a)) 
           (get-arity-of-pred (first a))))
)



(defun make-filler-assertion (rel i1 i2)
   (list i1 rel i2)
)

(defun get-ind1-of-filler-asse (a)
   (first a)
)

(defun get-rel-of-filler-asse (a)
   (second a)
)

(defun get-ind2-of-filler-asse (a)
   (third a)
)



(defun parse-equal-assertion (a)
   (and (listp a)
        (let ((a2 (second a)))
           (eq 'equal a2)
        )
    )
)

(defun parse-unequal-assertion (a)
   (and (listp a)
        (let ((a2 (second a)))
           (eq 'unequal a2)
        )
    )
)

(defun get-ind1-of-equal-asse (a)
  (first a))

(defun get-ind2-of-equal-asse (a)
  (third a))


;;;; jetzt doch unnoetig!!


(defun implant-predicates (cterm)
   (cond ((symbolp cterm) cterm)
         ((or (agreement? cterm) (disagreement? cterm)) cterm)
         ((negation? cterm) (make-negation (implant-predicates (get-concept-of-negation cterm))))
         ((conjunction? cterm) (make-conjunction (mapcar #'implant-predicates (get-concepts-of-conjunction cterm))))
         ((disjunction? cterm) (make-disjunction (mapcar #'implant-predicates (get-concepts-of-disjunction cterm))))
         ((value-restriction? cterm) (make-value-restriction (get-pathes-of-restriction cterm) (implant-predicates (get-restrictor cterm))))
         ((exists-in-restriction? cterm) (make-exists-in-restriction (get-pathes-of-restriction cterm) (implant-predicates (get-restrictor cterm))))
         (t NIL)
   )
)



        
             
