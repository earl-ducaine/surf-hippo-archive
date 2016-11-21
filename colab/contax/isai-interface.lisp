;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX -*-


;;;
;;; module: ISAI-INTERFACE
;;;
;;; This modules provides several functions of the programmer interface
;;; as macros for an easy use by an interactive user (mainly it spares
;;; him from quoting).
;;; The syntax is as specified in the paper "Using hierarchical constraint
;;; satisfaction for lathe-tool selection in a CIM environment" by Manfred
;;; Meyer (available as DFKI Research report RR-92-35)


;;; CREATED ON 21-10-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES:



( in-package "CONTAX" )


( export '( def-domain
	    def-int-domain

	    import-terminology

	    def-primitive-constraint
	    def-lisp-constraint
	    def-compound-constraint
	    def-conjunctive-constraint
	    def-disjunctive-constraint

	    make-variable
	    assign value
	    make-constraint
	    propagate ))

 
;;; BUILDING the HIERARCHY

( defmacro def-domain ( domain list-of-subdomains )
"Defines a new domain in terms of its immediate subdomains."
  `( def-concept-fn ',domain ',list-of-subdomains ))


( defmacro def-intdomain ( intdomain interval )
""
  ( declare ( ignore intdomain interval ))
  ( warn "You are using the macro DEF-INTDOMAIN, which is not supported by~
        ~%the current version of CONTAX." ))


;;; HIERACHY IMPORT FROM TAXON

( defmacro import-terminology ( concept )
"Imports the concept named CONCEPTS and all its subconcepts from TAXON."
  `( import-taxon-terminology ',concept ))


;;; VARIABLE CREATION and ASSIGNMENT and VALUE RETRIEVAL

( defmacro make-variable ( &key name
				domains
				( values  :unspecific )
				( type    :hierarchical ))
"Creates a variable of a given NAME and TYPE and initially assigns VALUES."
  `( make-variable-fn ',name
		      :domains ',( expand-domains-list domains )
		      :values  ',values
		      :type    ',type ))


( defmacro assign ( name new-val )
"Syntax: ( name value )
Assigns to variable named NAME the given VALUE."
  ( let (( var      ( gethash name *variable-table* :not-found )))
        ( if ( eq var :not-found )
	     ( error "Trying to set the value of variable ~a, which is not defined." name )
	     ( assign-fn var new-val ))
	`',new-val ))


( defmacro value ( name )
"Syntax: ( name )
Returns the value of the variable named NAME."
  ( let (( var ( gethash name *variable-table* :not-found )))
        ( if ( eq var :not-found )
	     ( error "Trying to get the value of variable ~a, which is not defined." name )
	     `',( values-fn var ))))


;;; CONTRAINT DEFINITION AND INSTANTIATION

( defmacro def-primitive-constraint ( &whole call
				      name
				      &key interface domains tuples
				      &allow-other-keys )
"Declares a primitive constraint of a given NAME with interface variables
INTERFACE, which is fullfilled by the tuples given in TUPLES."
  ( when ( member :relax1 call )
	 ( warn "You are using the keywords :RELAXn. Relaxation by inclusion of~
	       ~%of additional tuples is not supported at the time being." ))
  `( def-primitive-constraint-fn ',name
				 ',interface
				 ',( expand-domains-list domains )
				 ',domains ',tuples ))


( defmacro def-lisp-constraint ( &whole call
				 name
				 &key interface domains predicate )
"Declares a lisp-defined constraint of a given NAME with parameters
named PARAMS, which is defined by the LISP-function named L-FUN."
  ( when ( null domains )
	 ( cerror "Enter a list of domains."
		  "Your call ~a does not~
		 ~&contain a domain specification (via :DOMAIN)."
		  call )
	 ( setf domains ( read )))
  `( def-lisp-constraint-fn ',name
			    ',interface
			    ',( expand-domains-list domains )
			    ',( if ( symbolp predicate )
				  predicate
				  ( let (( function-name 'foo ))
					( eval `( defun ,function-name ,interface
						  ,@( rest ( rest predicate ))))))))


( defmacro def-compound-constraint ( &whole call
				     name
				     &key interface domains constraints )
"Declares a compound constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the conjunction of all
component constraints given in COMPS."
  `( progn
     ( warn "Your code contains a call to CONTAX:DEF-COMPOUND-CONSTRAINT~
             ~&~8t~a~
	     ~&which should be replaced by CONTAX:DEF-CONJUNCTIVE-CONSTRAINT."
	    ',call )
     ( def-conjunctive-constraint-fn ',name
				     ',interface
				     ',( expand-domains-list domains )
				     ',constraints )))


( defmacro def-conjunctive-constraint ( name
					&key interface domains constraints )
"Declares a compound constraint with a given NAME with parameters
named PARAMS, which is fullfilled by the conjunction of all
component constraints given in COMPS."
  `( def-conjunctive-constraint-fn ',name
				   ',interface
				   ',( expand-domains-list domains )
				   ',constraints ))


( defmacro def-disjunctive-constraint ( name
					&key interface domains constraints )
"Declares a compound constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the disjunction of all
component constraints given in COMPS."
  `( def-disjunctive-constraint-fn ',name
				   ',interface
				   ',( expand-domains-list domains )
				   ',constraints ))


( defmacro make-constraint ( &rest var-list
			     &key name
				  type
				  ( weight :hard )
			     &allow-other-keys )
"Creates an constraint object of a given NAME and TYPE. Actual
parameters can be specified in the keyword syntax with their
variable names."
  ( remf var-list :name )
  ( remf var-list :type )
  ( remf var-list :weight )
  `( make-constraint-fn ',name ',type :priority ,( symbol-to-keyword weight)
                                      ,@( mapcar #'( lambda ( x )
						     ( if ( keywordp x ) x `( gethash ',x *variable-table* )))
					         var-list )))


;;; PROPAGATION

( defmacro propagate ( &key ( consistency-mode :local )
			    ( up-to-weight :soft )
			    ( number-of-solutions :all )
			    ( errorp T ))
"This macro starts the propagation of the constraint net, trying to achieve
:CONSISTENCY-MODE with respect to the constraints having weight from :HARD to
:UP-TO-WEIGHT. The :NUMBER-OF-SOLUTIONS keyword controls how many solutions
will be computed in :GLOBAL mode."
  `( propagate-fn :consistency-mode ,consistency-mode
		  :minimum-priority ,up-to-weight
		  :number-of-solutions ,number-of-solutions
		  :errorp ,errorp ))


