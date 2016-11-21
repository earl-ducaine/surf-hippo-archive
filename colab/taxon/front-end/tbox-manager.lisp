;;; TBox-Manager
;;; ------------
;;; TBox als Hash-Table sowie TBox-Eintraege als Strukte
;;; 17-03-92 aa
;;; altes colab-listing modifiziert 15-07-92 aa


(defstruct (tbox-entry 
                      (:print-function print-tbox-entry)
            )
                      identifier
                      visible?
                      type
                      family
                      definition
                      output
                      shifted-not
                      arity
                      disjoint
                      cpred-domain
                      cpred-varlist
                      cpred-term
                      members
		      c-members
                      internal
                      negated
                      hierarchy-node
                      pconc?
                      NF)

(defun print-tbox-entry (r s k)
  
  (print-if (tbox-entry-identifier r)        "Identifier                      : ")
  (print-if (tbox-entry-definition r)        "was defined by the expression   : ")
  (print-if (tbox-entry-type r)              "Type                            : ")
  (print-if (tbox-entry-family r)            "Type of family                  : ")
  (print-if (tbox-entry-arity r)             "Arity                           : ")
  (print-if (tbox-entry-disjoint r)          "Disjoint primitive Concepts     : ")
  (print-if (tbox-entry-shifted-not r)       "Definition with shifted not     : ")
  (print-if (tbox-entry-cpred-domain r)      "Domain of Concrete Pred.        : ")
  (print-if (tbox-entry-cpred-varlist r)     "Variable list of Concr. Pred.   : ")
  (print-if (tbox-entry-cpred-term r)        "Concrete Predicate Term         : ")
  (print-if (tbox-entry-internal   r)        "Internal Representation         : ")
  (print-if (if (tbox-entry-hierarchy-node   r) (node-value (tbox-entry-hierarchy-node   r)))  "Equivalent to                   : ")

  (terpri)
  (princ (format nil
"--------------------------------------------------------------------------"))
  (terpri)
  
  )

#|
(defun print-if (value name)
  (cond (value
           (princ (format nil "    ~A ~A" name value))
           (terpri))))
|#

(defun listing ()
   (terpri)
   (maphash #'(lambda (x y) (print-tbox-entry y y y))
            *tbox-table*)
    T)

(defvar *tbox-table* (make-hash-table))

(defun clear-tbox ()
  (setf *tbox-table*
	(make-hash-table))
  (setf deflist nil)	
  (setf subslist nil)
  (clear-new-cp)
  (clear-name<-->cnf)
  (setf *hierarchy-generators* NIL)
  (setf *classify-list* nil)
  (setf *filtered-graph* *subsumption-graph*)
  (setf *filtered-nodes* (make-hash-table))
  (switch-filtered-graph NIL)
  (tbox-insert 'top
	       :type 'conc
	       :definition '(top built-in)
	       :arity 1 :visible? T :output 'top
	       :hierarchy-node (graph-top *subsumption-graph*))
  (tbox-insert 'bottom
	       :type 'conc
	       :definition '(bottom built-in)
	       :arity 1 :visible? T :output 'bottom
	       :hierarchy-node (graph-bottom *subsumption-graph*))
  (cpred  =       (ro (x y) (= x y)))


  
  )


(defun get-tbox-entry (item)
   (gethash item *tbox-table*)
)

(defun tbox-insert (identifier  &key type definition arity disjoint shifted-not 
                                     cpred-domain cpred-varlist cpred-term internal
                                     pconc? members c-members
                                     negated family output hierarchy-node visible?)
   (if (tbox-member? identifier)
     (if (open-family? identifier)
         (progn 
            (terpri)
            (princ (format nil "Open Family ~a still exists -- it will be extended!" identifier))
            (terpri)
          )
       (progn
           (terpri)
           (princ (format nil "Warning -- second definition of ~a is ignored!" identifier))
           (terpri)
       )
       )
       (progn
       (setf (gethash identifier *tbox-table*)
             (make-tbox-entry :identifier identifier
                              :type type
                              :visible? visible?
                              :output output
                              :family family
                              :arity arity
                              :definition definition
                              :disjoint disjoint
                              :shifted-not shifted-not
                              :cpred-domain cpred-domain
                              :cpred-varlist cpred-varlist
                              :cpred-term cpred-term
                              :internal internal
                              :pconc? pconc?
                              :members members
			      :c-members c-members
                              :hierarchy-node  hierarchy-node
                              :negated negated
              )
        )
        (cond ((and (eq type 'conc) (not (eq identifier 'top)) (not (eq identifier 'bottom)))
               (setf *classify-list*  (cons identifier *classify-list*))
               (transform_concept->CNF identifier))
        )
        )
    )
)

(defun tbox-modify (identifier  &key type definition arity disjoint shifted-not 
                                     cpred-domain cpred-varlist cpred-term
                                     hierarchy-node pconc? members c-members
                                     internal negated family visible? output )
   
       (setf (gethash identifier *tbox-table*)
             (make-tbox-entry :identifier identifier
                              :type type
                              :family family
                              :arity arity
                              :definition definition
                              :output output
                              :disjoint disjoint
                              :shifted-not shifted-not
                              :cpred-domain cpred-domain
                              :cpred-varlist cpred-varlist
                              :cpred-term cpred-term
                              :internal internal
                              :negated negated
                              :members members
			      :c-members c-members
                              :hierarchy-node hierarchy-node
                              :pconc? pconc?
                              :visible? visible?
              )
        )
    
)

(defun tbox-insert-hierarchy-node (co node)
   (setf (tbox-entry-hierarchy-node (gethash co *tbox-table*))
         node)
)

(defun user-defined-concept? (item)
   (let ((entry (get-tbox-entry item)))
      (if entry
          (tbox-entry-visible? entry)))
)

         


(defun tbox-member? (identifier)
   (gethash identifier *tbox-table*)
)

(defun get-concept-term (identifier)
   (if (tbox-member? identifier)
       (tbox-entry-shifted-not (get-tbox-entry identifier))
       (progn (print (format nil "There is no TBox entry for ~A" identifier)) NIL)
    )
) 

(defun get-definition (identifier)
   (if (tbox-member? identifier)
       (tbox-entry-definition (get-tbox-entry identifier))
      ; (progn (print (format nil "There is no TBox entry for ~A" identifier)) NIL)
    )
)  

(defun get-def (item)
   (list (get-type-of-item item) (get-definition item)))

(defun get-type-of-item (item)
   (if (tbox-member? item)
       (tbox-entry-type (get-tbox-entry item))
   )
)

(defun get-family-type-of-item (item)
   (if (tbox-member? item)
       (tbox-entry-family (get-tbox-entry item))
   )
)

(defun get-output-of-item (item)
   (if (tbox-member? item)
       (tbox-entry-output (get-tbox-entry item))
   )
)

(defun get-arity-of-item (item)
   (if (tbox-member? item)
       (tbox-entry-arity (get-tbox-entry item))
   )
)

(defun get-arity-of-pred (item)
   (if (tbox-member? item)
       (tbox-entry-arity (get-tbox-entry item))
   )
)



(defun get-args-of-pred (predname)
   (if (tbox-member? predname)
       (tbox-entry-cpred-varlist (get-tbox-entry predname))
   )
)

(defun get-body-of-pred (predname)
   (if (tbox-member? predname)
       (tbox-entry-cpred-term (get-tbox-entry predname))
   )
)

(defun get-concrete-domain-of-pred (predname)
   (if (tbox-member? predname)
       (tbox-entry-cpred-domain (get-tbox-entry predname))
   )
)


(defun get-internal-rep-of-cpred (item)
   (if (tbox-member? item)
       (tbox-entry-internal (get-tbox-entry item))
   )
) 

(defun get-neg-internal-rep-of-cpred (item)
   (if (tbox-member? item)
       (tbox-entry-negated (get-tbox-entry item))
   )
)                 

(defun get-domain-of-cpred (item)
   (if (tbox-member? item)
       (tbox-entry-cpred-domain (get-tbox-entry item))
   )
) 

(defun get-pconc?-of-item (item)
   (if (tbox-member? item)
       (tbox-entry-pconc? (get-tbox-entry item))
   )
)                 

(defun get-node-of-item (item)
  (if *filtered-graph?
      (gethash item *filtered-nodes*)
      (if (tbox-member? item)
          (tbox-entry-hierarchy-node (get-tbox-entry item)))
   ))

(defun visible-item? (item)
   (if (tbox-member? item)
       (tbox-entry-visible? (get-tbox-entry item))
   )
) 

(defun get-family (item)
   (if (tbox-member? item)
       (first (tbox-entry-disjoint (get-tbox-entry item)))))
                


(defun conjunction-exists? (conj1 concname)
   (let ((conj2 (get-concept-term concname)))
     (let ((concset1 (get-concepts-of-conjunction conj1))
           (concset2 (if (conjunction? conj2) (get-concepts-of-conjunction conj2))))
        (and (null (set-difference concset1 concset2))
             (null (set-difference concset2 concset1)))
     )
   )
)

(defun role-entry? (entry)
   (eq 'role (tbox-entry-type entry)))

(defun attr-entry? (entry)
   (eq 'attr (tbox-entry-type entry)))


