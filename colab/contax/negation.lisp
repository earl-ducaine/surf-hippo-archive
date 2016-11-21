;;; -*- Package: CONTAX; Base: 10; Syntax: Common-Lisp -*-


( export '( def-negation
	    def-negation-fn ))


( defmacro def-negation ( constraint-type
			  &key ( name NIL neg-name-supplied-p ))
"Macro to define a constraint equivalent to the negation of the constraint
definition named CONSTRAINT-NAME. A name for the definition of the negation
can be supplied with the keyword argument :NAME, it defaults to NOT-<CONSTRAINT-NAME>."
  ( if neg-name-supplied-p
       `( def-negation-fn ',constraint-type :name ',name )
       `( def-negation-fn ',constraint-type )))


( defun def-negation-fn ( constraint-type
			  &key (( :name neg-c-name )
				( intern ( string-append "NOT-"	( symbol-name constraint-type ))))
			  &aux c-class )
"Function to define a constraint equivalent to the negation of the constraint
definition named CONSTRAINT-NAME. A name for the definition of the negation
can be supplied with the keyword argument :NAME, it defaults to NOT-<CONSTRAINT-NAME>."
  ( if ( constraint-definition-name-p constraint-type )
       ( setf c-class ( find-class constraint-type ))
       ;;; refuse, if CONSTRAINT-TYPE not a valid constraint definition
       ( error "This is not a constraint ~a." c-class ))
  ( let* (( c-class-precedence ( class-precedence-list c-class ))
	  ( parameters         ( eval ( class-slot-initform c-class 'parameters )))
	  ( domains            ( eval ( class-slot-initform c-class 'domains )))
;	  ( var-slots          ( compute-var-slots parameters '() ))
)
  ( cond ;;; primitive constraint
	 (( member ( find-class 'primitive-constraint )
		   c-class-precedence )
	  ( def-primitive-constraint-fn neg-c-name parameters domains
	    ( set-difference ( expand-tuples ( list domains ))
			     ( expand-tuples ( eval ( class-slot-initform c-class
									  'c-def )))
			     :test #'equal )))
	 ;;; lisp constraints
	 (( member ( find-class 'lisp-constraint )
		   c-class-precedence )
	  ( let* (( lisp-fun-name ( eval ( class-slot-initform c-class 'l-fun )))
		  ( neg-fun-name  ( intern ( string-append "NOT-" ( symbol-name lisp-fun-name )))))
		 ( eval `( defun ,neg-fun-name ( ,@parameters )
			   ( not ( ,lisp-fun-name ,@parameters ))))
		 ( def-lisp-constraint-fn neg-c-name parameters domains
		   neg-fun-name )))
	 ;;; compound constraints
	 (( member ( find-class 'compound-constraint )
		   c-class-precedence )
	  ;;; negate all components
	  ( loop for component in ( eval ( class-slot-initform c-class 'components ))
		 when ( constraint-definition-name-p ( first component ))
			collect ( cons ( class-name ( def-negation-fn ( first component )))
				       ( rest component ))
				into neg-components
	    finally ( return ( funcall ( cond (( member ( find-class 'conjunctive-constraint )
							c-class-precedence )
					       #'def-disjunctive-constraint-fn )
					      (( member ( find-class 'disjunctive-constraint )
							c-class-precedence )
					       #'def-conjunctive-constraint-fn )
					      ( T
					       ( error "Unknown compound type." )))
				       neg-c-name parameters domains neg-components ))))
	 ( T
	  ( error "Negation not defined for this kind of constraint: ~A" constraint-type )))))
