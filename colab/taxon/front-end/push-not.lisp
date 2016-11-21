;;; external representation of concept terms:
;;; push-not pushes negation one position 
;;; shift-not shifts negation as far as possible 
;;; both do not shift over quantification
;;; uses: absynt
;;; knows: absynt
;;; 11-06-92 aa


(defun push-not (cterm)
   (if (or (not (listp cterm))
           (not (negation? cterm)))
       (error "input of push-not was not a negated term")
     (let ((ct (get-concept-of-negation cterm)))
       (cond ((negation? ct)
              (get-concept-of-negation ct))
             ((conjunction? ct)
              (make-disjunction (mapcar 'make-negation (get-concepts-of-conjunction ct))))
             ((disjunction? ct)
              (make-conjunction (mapcar 'make-negation (get-concepts-of-disjunction ct))))
             ((agreement? ct)
              (make-disagreement (get-pathes-of-agreement ct)))
             ((disagreement? ct)
              (make-agreement (get-pathes-of-agreement ct)))
             ((value-restriction? ct)
	      ct)
             ((exists-in-restriction? ct)
              ct)
           

      ))))


(defun shift-not (cterm)
  
       (cond ((or (conceptname? cterm) (prim? cterm)) cterm)
             ((negation? cterm)
                 
              (let ((ct (get-concept-of-negation cterm)))
                 (cond ((negation? ct) (shift-not (get-concept-of-negation ct)))
                       ((conjunction? ct)
                        (make-disjunction (mapcar #'(lambda (x) (shift-not (make-negation x))) (get-concepts-of-conjunction ct))))
                       ((disjunction? ct)
                        (make-conjunction (mapcar #'(lambda (x) (shift-not (make-negation x))) (get-concepts-of-disjunction ct))))
                       (t cterm))))



              
             ((conjunction? cterm)
              (make-conjunction (mapcar #'(lambda (x) (shift-not x)) (get-concepts-of-conjunction cterm))))
             ((disjunction? cterm)
              (make-disjunction (mapcar #'(lambda (x) (shift-not x)) (get-concepts-of-disjunction cterm))))
             (t cterm)
            )
     
)


