;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX -*-


;;;
;;; module: USER-INTERFACE
;;;
;;; This modules provides several functions of the programmer interface
;;; as macros for easier use by an interactive user (mainly it spares
;;; him from quoting).


;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES:



( in-package "CONTAX" )


( export '( propagate
	    def-concept
	    push-var-states
	    restore-var-states
	    pop-var-states
	    make-variable
	    assign
	    value
	    def-primitive-constraint
	    def-domain-constraint
	    def-lisp-constraint
	    def-compound-constraint
	    def-conjunctive-constraint
	    def-disjunctive-constraint
	    make-constraint
	    spy unspy watch unwatch reset show contax-state ))

 
;;; BUILDING the HIERARCHY

( defmacro def-concept ( concept subconcepts )
"Syntax: ( concept-name subconcept-names )"
  `( def-concept-fn ',concept ',subconcepts ))


;( defmacro def-intervall ( intervall-name intervalls )
;"syntax: ( intervall-name interval-values )"
;  `( def-intervall-fn ',intervall-name ',( read-intervalls intervalls )))


;;; STATE SAVING and RETRIEVAL

;;; There are no macros in this section. All functions are only defined
;;; for consistency in the identifier names.

( setf ( symbol-function 'push-var-states-fn )    ( symbol-function 'push-var-states )
       ( symbol-function 'restore-var-states-fn ) ( symbol-function 'restore-var-states )
       ( symbol-function 'pop-var-states-fn )     ( symbol-function 'pop-var-states ))


;;; VARIABLE CREATION and ASSIGNMENT and VALUE RETRIEVAL

( defmacro make-variable ( name &key domains
			             ( values  :unspecific )
				     ( type    :hierarchical ))
"Syntax: ( name &key domains ( values :unspecific ) ( type :hierarchical ))
Creates a variable of a given NAME and TYPE and initially assigns
INIT-ASS."
  `( make-variable-fn ',name :domains ',domains :values ',values :type ',type ))


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

( defmacro def-primitive-constraint ( name params doms &body c-def )
"Syntax: ( name params &body c-def )
Declares a primitive constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the tuples given in C-DEF."
  `( def-primitive-constraint-fn ',name ',params ',doms ',c-def ))


( defmacro def-domain-constraint ( name params doms &body c-def )
  `( def-domain-constraint-fn ',name ',params ',doms ',c-def ))


( defmacro def-lisp-constraint ( name params doms l-fun )
"Syntax: ( name params l-fun )
Declares a lisp-defined constraint of a given NAME with parameters
named PARAMS, which is defined by the LISP-function named L-FUN."
  `( def-lisp-constraint-fn ',name ',params ',doms ',l-fun ))


( defmacro def-compound-constraint ( &whole expr name params doms &body comps )
"Syntax: ( name params &body comps )
Declares a compound constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the conjunction of all
component constraints given in COMPS."
  `( progn
     ( warn "Your code contains a call to CONTAX:DEF-COMPOUND-CONSTRAINT~
             ~&~8t~a~
	     ~&which should be replaced by CONTAX:DEF-CONJUNCTIVE-CONSTRAINT."
	    ',expr )
     ( def-conjunctive-constraint-fn ',name ',params ',doms ',comps )
))


( defmacro def-conjunctive-constraint ( name params doms &body comps )
"Syntax: ( name params doms &body comps )
Declares a compound constraint with a given NAME with parameters
named PARAMS, which is fullfilled by the conjunction of all
component constraints given in COMPS."
  `( def-conjunctive-constraint-fn ',name ',params ',doms ',comps ))


( defmacro def-disjunctive-constraint ( name params doms &body comps )
"Syntax: ( name params doms &body comps )
Declares a compound constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the disjunction of all
component constraints given in COMPS."
  `( def-disjunctive-constraint-fn ',name ',params ',doms ',comps ))


( defmacro make-constraint ( name type &rest var-list &key ( priority :hard ) &allow-other-keys )
"Syntax:  ( name type &rest var-list )
Creates an constraint object of a given NAME and TYPE. Actual
parameters can be specified in the keyword syntax with their
variable names."
;  ( remf var-list :priority )
  `(format t "name ~a type ~a var-list ~a" ',name ',type ',var-list)
  `( make-constraint-fn ',name ',type :priority ,priority
                                      ,@( mapcar #'( lambda ( x )
						     ( if ( keywordp x ) x `( gethash ',x *variable-table* )))
					         var-list )))


;;; PROPAGATION

( setf ( symbol-function 'propagate ) ( symbol-function 'propagate-fn ))


;;; GLOBAL CONTROL

( setf ( symbol-function 'reset ) ( symbol-function 'reset-fn ))
 

;;; TRACING & DEBUGGING

;;; those nice things have all been moved to the DEBUGGER module





