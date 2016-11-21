;;; -*- Syntax: Cltl; Base: 10; Mode: LISP; Package: CONTAX; -*-
;;;
;;; module: CONSTRAINTS
;;;
;;; providing the following
;;;   constraint classes
;;;   variables: *PARAMETERS*, *CONSTRAINT-TABLE*
;;;   methods  : PARAMETER-NAMES ( constraint )
;;;		 PARAMETER-NAMES ( symbol )
;;;              VARIABLE-NAMES ( constraint )
;;;		 VARIABLE-NAMES ( compound-constraint )
;;;		 INITIALIZE-INSTANCE :after ( constraint )
;;;		 SHARED-INITIALIZE :after ( constraint )
;;;		 SHARED-INITIALIZE :after ( compound-constraint )
;;;		 

;;; changed 10-09-92 fs
;;;   added some comments on the slot definition of the constraints
;;; changed 22-10-92 fs
;;;   PRINT-OBJECT now prints variable names
;;; changed 18-11-92 fs
;;;   moved DESRCIBE-CONSTRAINT and PRINT-OBJECT to DEBUGGER
;;; changed 25-11-92 fs
;;;   extended EXPORT list


( in-package "CONTAX" )


( export '( def-primitive-constraint-fn
	    def-lisp-constraint-fn
	    def-compound-constraint-fn
	    def-conjunctive-constraint-fn
	    def-disjunctive-constraint-fn

	    constraint-definition-name-p

	    make-constraint-fn

	    constraint-instance-name-p
	    lisp-constraint-p
	    primitive-constraint-p
	    conjunctive-constraint-p
	    disjunctive-constraint-p ))



;;;;; constraint CLASS-DEFINITIONS


( defclass constraint ()
  (( name       :reader        cname
                :initarg       :cname
	        :allocation    :instance
	        :documentation
		"Holds the print name. Every instance gets its unique name." )
   ( watched    :accessor cwatched-p
		:initform NIL )
   ( priority   :accessor   priority
                :initarg    :priority
	        :allocation :instance )
   ( parameters	:reader     parameter-names   ;added 18-2-92 fs
		:initarg    :parameters
		:allocation :class )
   ( domains    :reader     domains
	        :initarg    :domains
		:allocation :class ))
  ( :documentation "Top of the contraint type hierarchy." ))


( defclass primitive-constraint ( constraint )
  (( c-def      :accessor c-def
		:documentation
		"Contains the user's definition of the constraint." )
   ( comp-c-def :accessor comp-c-def
		:documentation
		"Contains a CONTAX manipulated defintion of the constraint." ))
  ( :documentation "The class of all primitive constraints." ))


( defclass lisp-constraint ( constraint )
  (( l-fun :accessor l-fun
	   :documentation
	   "Contains the name of a LISP function." ))
  ( :documentation "The class of all lisp defined constraints." ))


( defclass domain-constraint ( primitive-constraint )
  (( flag :accessor activated-p
          :initform nil ))
  ( :documentation "Domains restricting variable to be activated only once." ))


( defclass compound-constraint ( constraint )
  (( local-names   :reader     local-names
		   :documentation
		   "Contains a list of local variable names." )
   ( local-domains :reader     local-domains
		   :documentation
		   "Contains a list of the maximum domains of the local variables." )
   ( components    :accessor   components
		   :documentation
		   "Contains the component definitions (body) of the constraint." )
   ( constraints   :accessor   constraints
		   :initform   '()
		   :documentation
		   "Contains a list of the constraints constituting the compound." ))
  ( :documentation "The class of all compound constraints." ))


( defclass conjunctive-constraint ( compound-constraint )
  ()
  ( :documentation "Conjunction of constraints in one component." ))


( defclass disjunctive-constraint ( compound-constraint )
  (( parameter-mappings :accessor parameter-mappings
			:initform '()
			:documentation
			"Contains in an a-list for each component the matching of compound parameters to component parameters." )
   ( variable-sets      :accessor variable-sets
			:initform '()
			:documentation
			"Contains a list of the variables of each component constraint." ))
  ; with car being the parameter of the disjunction & cdr the parameter of the component
  ( :documentation "Disjunction of constraints." ))


;( defclass hac-mixin ()
;  (( hac-look-up :accessor hac-look-up )))


;;;;; VARIABLES


( defvar *constraint-table* ( make-hash-table )
  "Hashtable (by name) of all constraints within the system." )

( defun find-constraint ( symbol &optional default )
  ( gethash symbol *constraint-table* default ))

; 01-09-92 fs
; 12-10-92 fs : renamed
( defun list-all-constraints ( &aux ( clist '()) )
"Returns a list of all constraint objects."
  ( maphash #'( lambda ( name constraint )
	        ( declare ( ignore name ))
		( push constraint clist ))
	    *constraint-table* )
  ( values clist ))


;;;;; TESTING

( defun constraint-instance-name-p ( symbol )
"Returns T, iff SYMBOL is the name of a constraint instance, NIL otherwise."
  ( second ( multiple-value-list ( gethash symbol *constraint-table* ))))


; 20-09-92 fs : debugged
( defun constraint-definition-name-p ( symbol
				       &aux ( class ( find-class symbol NIL )))
"Returns T, iff SYMBOL ist the name of a constraint definition, NIL otherwise."
  ( if class
       ( if ( member ( find-class 'constraint )
		     ( class-precedence-list class ))
	    T
	    NIL )
       NIL ))


; 11-03-92 fs
( defun lisp-constraint-p ( constraint )
  ( typep constraint 'lisp-constraint ))

( defun primitive-constraint-p ( constraint )
  ( typep constraint 'primitive-constraint ))

( defun compound-constraint-p ( constraint )
  ( typep constraint 'compound-constraint ))

( defun conjunctive-constraint-p ( constraint )
  ( typep constraint 'conjunctive-constraint ))

( defun disjunctive-constraint-p ( constraint )
  ( typep constraint 'disjunctive-constraint ))


;;; CONTRAINT DEFINITION AND INSTANTIATION

( defun def-primitive-constraint-fn ( name params doms c-def )
"Declares a primitive constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the tuples given in C-DEF."
  ( eval `( defclass ,name ( primitive-constraint )
	             (( parameters :initform ',( copy-tree params ))
		      ( c-def      :initform ',( copy-tree  c-def ))
		      ( comp-c-def :initform ',( copy-tree ( expand-tuples c-def )))
		      ( domains    :initform ',( copy-tree doms ))
		      ,@( compute-var-slots params () )))))


( defun def-domain-constraint-fn ( name params doms c-def )
  ( eval `( defclass ,name ( domain-constraint )
		      (( parameters :initform ',( copy-tree params ))
		       ( c-def      :initform ',( copy-tree c-def ))
		       ( comp-c-def :initform ',( copy-tree ( expand-tuples c-def )))
		       ( domains    :initform ',( copy-tree doms ))
		       ,@( compute-var-slots params () )))))


( defun def-lisp-constraint-fn ( name params doms l-fun )
"Declares a lisp-defined constraint of a given NAME with parameters
named PARAMS, which is defined by the LISP-function named L-FUN."
  ( eval `( defclass ,name ( lisp-constraint )
                    (( parameters :initform ',( copy-tree params ))
		     ( l-fun :initform   ',l-fun
			     :allocation :class )
		    ( domains    :initform ',( copy-tree doms ))
	            ,@( compute-var-slots params () )))))


( defun def-compound-constraint-fn ( name params doms comps
				     &aux ( locals ( compute-local-vars params comps )))
"Declares a compound constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the conjunction of all
component constraints given in COMPS."
  ( warn "Your code contains a call to CONTAX:DEF-COMPOUND-CONSTRAINT-FN.~
	  ~&which should be replaced by CONTAX:DEF-CONJUNCTIVE-CONSTRAINT-FN." )
  ( eval `( defclass ,name ( compound-constraint )
		(( parameters    :initform ',( copy-tree params ))
		 ( domains       :initform ',( copy-tree doms ))
		 ( local-names   :initform ',locals )
		 ( local-domains :initform ',( compute-local-domains locals comps ))
		 ( components    :initform ',( copy-tree comps ))
		 ,@( compute-var-slots params locals )))))


( defun def-conjunctive-constraint-fn ( name params doms comps
					&aux ( locals ( compute-local-vars params comps )))
"Declares a compound constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the conjunction of all
component constraints given in COMPS."
  ( eval `( defclass ,name ( conjunctive-constraint )
		(( parameters    :initform ',( copy-tree params ))
		 ( domains       :initform ',( copy-tree doms ))
		 ( local-names   :initform ',locals )
		 ( local-domains :initform ',( compute-local-domains locals comps ))
		 ( components    :initform ',( copy-tree comps ))
		 ,@( compute-var-slots params locals )))))


( defun def-disjunctive-constraint-fn ( name params doms comps
					&aux ( locals ( compute-local-vars params comps )))
"Declares a compound constraint of a given NAME with parameters
named PARAMS, which is fullfilled by the disjunction of all
component constraints given in COMPS."
  ( eval `( defclass ,name ( disjunctive-constraint )
		(( parameters    :initform ',( copy-tree params ))
		 ( domains       :initform ',( copy-tree doms ))
		 ( local-names   :initform ',locals )
		 ( local-domains :initform ',( compute-local-domains locals comps ))
		 ( components    :initform ',( copy-tree comps ))
		 ,@( compute-var-slots params locals )))))


; 23-11-92 fs changed
( defun make-constraint-fn ( name type &rest var-list &key ( priority :hard ) &allow-other-keys )
"Creates an constraint object of a given NAME and TYPE. Actual
parameters can be specified in the keyword syntax."
  ( prog1 ( setf ( gethash name *constraint-table* )
		 ( eval `( make-instance ',type :cname ',name ,@var-list )))
	  ( when ( and *interactive-mode*
		       ( not ( eq ( getf *automatic-mode* :instantiation ) :OFF )))
		 ( propagate-locally ( getf *automatic-mode* :instantiation )))))


( defun delete-constraint ( name &key ( delete-unreferenced-variables T )
				      ( activate-neighbours T ))
"Deletes the named constraint including all component constraints. If
DELETE-UNREFERENCED-VARIABLES is T, all variables only connected to
this certain constraint are also deleted. If ACTIVATE-NEIGHBOURS is T,
all neighbouring constraints are also activated.
Two values are returned:
the first one T or NIL indicates, if a constraint named NAME was
found, the second one is a list of now unreferenced variables."
  ( if ( constraint-instance-name-p name )
       ( let* (( constraint  ( gethash name *constraint-table* ))
	       ( unrefd-vars ( local-variables constraint )))
	      ( dolist ( x ( constraints constraint ))
		       ( setf unrefd-vars ( append unrefd-vars
						   ( delete-constraint ( cname x )
								       :delete-unreferenced-variables
								          delete-unreferenced-variables
								       :activate-neighbours
								          activate-neighbours ))))
	      ( remhash name *constraint-table* )
	      ( constraint-queue-delete-any constraint *active-constraints* )
	      ( dolist ( var ( parameter-variables constraint ))
		       ( setf ( var-cons var ) ( delete constraint ( var-cons var )))
		       ( if ( zerop ( length ( var-cons var )))
			    ( pushnew var unrefd-vars )
			    ( when activate-neighbours
				   ( mapc #'activate ( var-cons var )))))
	      ( when delete-unreferenced-variables
		     ( dolist ( var unrefd-vars )
			      ( delete var *variables* )
			      ( remhash ( vname var ) *variable-table* )))
	      ( values T unrefd-vars ))
       ( values NIL NIL )))


;;;;; METHODS on constraints

;;; handling of VARIABLES and PARAMETER

( defmethod parameter-names (( cn symbol ))
"Returns the names of parameters of a constraint class named C."
  ( parameter-names ( find-class cn )))


; 18-2-92
( defmethod parameter-names (( constraint-class standard-class ))
  ( if ( member ( find-class 'constraint )
		( class-precedence-list constraint-class ))
       ( eval ( class-slot-initform constraint-class 'parameters ))
       ( error "This is not a constraint class: ~a" constraint-class )))


; changed 13-05-92 fs
( defun parameter-variables ( constraint )
"Returns the variable objects to which the CONSTRAINT is connected."
  ( mapcar #'( lambda ( vname )
	       ( funcall vname constraint ))
	   ( parameter-names constraint )))


; changed 13-05-92 fs
( defun local-variables ( constraint )
  ( etypecase constraint
	      (( primitive-constraint lisp-constraint ) '() )
	      ( compound-constraint ( mapcar #'( lambda ( vname )
						 ( funcall vname constraint ))
					     ( local-names constraint )))))


( defmethod variable-names (( c compound-constraint ))
"Returns the names of parameters and local variables of C."
  ( append ( parameter-names c )
	   ( local-names c )))


( defmethod variable-names (( c constraint ))
  ( parameter-names c ))


( defmethod variables (( c constraint ))
  ( mapcar #'( lambda ( x )
	       ( funcall x c ))
	   ( variable-names c )))


( defmethod domains (( constraint-name symbol ))
  ( domains ( find-constraint constraint-name )))


( defmethod comp-c-def (( constraint-name symbol ))
  ( comp-c-def ( find-constraint constraint-name )))


; 18-2-92
( defmethod domains (( constraint-class standard-class ))
  ( if ( member ( find-class 'constraint )
		( class-precedence-list constraint-class ))
       ( eval ( class-slot-initform constraint-class 'domains ))
       ( error "This is not a constraint class: ~a" constraint-class )))


; 11-03-92 fs
; 10-09-92 fs : debugged
( defun compute-local-domains ( locals components &aux position )
"Computes a list of domains for a list of local variables."
  ( flet (( one-local ( lv )
            ( apply #'hset-union
		    ( mapcap #'( lambda ( c )
				 ( unless ( constraint-definition-name-p ( first c ))
					  ( warn "You are using an previously undefined constraint type (~a)~
						 ~&as a component of a compound constraint.~
						 ~&(This may result in a lack of performance.)"
						 ( first c ))
					  ( return-from one-local '( TOP )))
				 ( if ( setf position ( position lv ( rest c )))
				      ( nth position
					    ( eval ( class-slot-initform ( find-class ( first c ))
									 'domains )))
				      ; local does not occur => no contribution
				      '() ))
		     components ))))
	 ( mapcar #'one-local locals )))


;;; DEFINITION of constraints


( defun compute-local-vars ( params body )
"Returns a list of symbols appearing at variable position inside BODY,
but do not occur inside the parameter list PARAMETER."
  ( set-difference ( remove-duplicates ( mapcap #'rest body ))
		   params ))


( defun compute-var-slots ( param-names local-names )
"Returns a list with slot specifiers for each element of param-names and
local-names."
    ( mapcar #'( lambda ( x )
                 ( list x :accessor x
			  :initarg  ( symbol-to-keyword x)))
	     ( append param-names local-names )))


;;; INSTANTIATION

; On initialization of constraints the following method are called in the order given below:
; ( initialize-instance )
; calling ( shared-initialize :around ( constraint )             - turn off interactive mode
;         ( shared-initialize ) - filling in the slots according to initargs & initforms
;         ( shared-initialize :after ( constraint ))             - check for parameter supply &
;                                                                  connect them
;         ( shared-initialize :after ( compound-constraint ))    - create local variables
;         ( shared-initialize :after ( disjunctive-constraint )) - create components with own variables
;         ( shared-initialize :after ( conjunctive-constraint )) - create component constraints
;         ( shared-initialize :after ( lisp-constraint ))        - expand variable values, if necessary
; ( initialize-instance :after ( constraint ))                   - activates constraint
; ( initialize-instance :after ( disjunctive-constraint ))       - deactivates constraints components
; definitions of method specializing on disjunctive constraints can be found in DISJUNCTION.LISP


; changed 23-03-92 - check parameter supply & connect them
( defmethod shared-initialize :after (( c constraint ) slots
				      &rest var-list
				      &aux temp-var
					   ( params ( parameter-names c )))
"Connect all variables of the constraint C specified in VAR-LIST to the
constraint. If any parameters are missing, an error is signaled, local
variables are created by need."
  ( declare ( ignore slots ))
  ( dolist ( par params )
           ( if ( setf temp-var ( getf var-list ( symbol-to-keyword par ))) ; variable supplied ?
                ( connect-var-to-cons temp-var c )
                ( error "No variable for parameter ~a supplied on initialization of constraint ~a." par ( cname c )))))


; changed 14-09-92 - lost and found fs
( defmethod shared-initialize :after (( cc compound-constraint ) slots
				      &rest var-list )
; create the local variable instances
  ( declare ( ignore slots var-list ))
  ( loop for local-name in ( eval ( class-slot-initform ( class-of cc ) 'local-names ))
	 and local-domains in ( eval ( class-slot-initform ( class-of cc ) 'local-domains ))
	   as new-local = ( make-local-variable local-domains )
	   do ( setf ( slot-value cc local-name ) new-local )
	      ( pushnew cc ( slot-value new-local 'constraints ))))


( defmethod shared-initialize :after (( cc conjunctive-constraint ) slots
				      &rest var-list
				      &aux  ( priority ( getf var-list :priority )))
  ( declare ( ignore slots ))
  #+:genera( remf var-list 'clos-internals:storage-area )
  ( remf var-list :cname )
  ( remf var-list :priority )
  ( unless priority ( setf priority :hard ))
  ; extend var-list by local keys and variables
  ( dolist ( local ( local-names cc ))
           ( push ( funcall local cc ) var-list )
           ( push ( symbol-to-keyword local ) var-list ))
  ; create component constraints
  ( dolist ( component ( components cc ))
                  ( let* (( comp-cc-keys ( mapcar #'symbol-to-keyword
                                                  ( rest component )))
                          ( comp-keys ( mapcar #'symbol-to-keyword
                                               ( parameter-names ( first component ))))
                          ( subst ( mapcar #'cons
                                           comp-cc-keys
                                           comp-keys )))
                         ( do* (( curr-key ( first var-list )
                                           ( first old-args ))
                                ( old-args ( rest ( rest var-list ))
                                           ( rest ( rest old-args )))
                                ( new-key  ( my-assoc curr-key subst )
                                           ( my-assoc curr-key subst ))
                                ( new-args ( if new-key
                                                ( list new-key ( getf var-list curr-key ))
                                                nil )
                                           ( if new-key
                                               ( nconc new-args
                                                       ( list new-key ( getf var-list curr-key )))
                                               new-args )))
                               (( endp old-args )
                                ( pushnew ( eval `( make-constraint-fn ',( gentemp ( string-append
( string ( first component )) "-" ))
                                                                       ',( first component )
                                                                       :priority ',priority
                                                                       ,@new-args ))
                                          ( constraints cc )))))))


; create constraint-components with their own variables

( defmethod shared-initialize :after (( dc disjunctive-constraint ) slots
				      &rest args-list
				      &aux  ( priority ( getf args-list :priority )))
  ( declare ( ignore slots )
	    ( special *variables* ))
  ( unless priority ( setf priority :hard ))
  ( dolist ( component ( reverse ( components dc )))
	   ( loop ; with pos = 1
		  with cname = ( first component )
		  with params = ( parameter-names cname )
		  with *variables* = '()
                  for arg in ( rest component )
		  and pos from 0
		    as par = ( nth pos params )
		    as var = ( make-local-variable ( nth ( position par params )
							 ( domains dc )))
		    collect ( cons arg par ) into mapping
		    collect ( symbol-to-keyword par ) into par-list
		    collect var into par-list
		    collect var into var-list
		    ; count T into pos
		  finally ( push mapping ( parameter-mappings dc ))
			  ( push var-list ( variable-sets dc ))
			  ( push ( apply #'make-constraint-fn ( gentemp ( string-append ( string cname ) "-" ))
							      ( first component )
							      :priority priority
							      par-list )
				 ( constraints dc ))))
;			  ( setf ( variables ( first ( constraints dc ))) *variables* ))
;  ( setf ( constraints dc ) ( nreverse ( constraints dc ))))
;  ( setf ( variable-sets dc ) ( nreverse ( variable-sets dc )))
)


( defmethod shared-initialize :after (( lc lisp-constraint ) slots-for-initform
				      &rest initargs )
  ( declare ( ignore slots-for-initform initargs ))
  ( dolist ( var ( remove-if #'var-expanded-p
			     ( remove-if-not #'hierarchical-variable-p
					     ( variables lc ))))
	   ( setf ( slot-value var 'values ) ( var-expanded-vals var )
		  ( var-expanded-p var ) T )))


( defun deactivate-subnet ( con )
  ( dolist ( subc ( constraints con ))
    ( deactivate subc )
    ( when ( compound-constraint-p subc )
	   ( deactivate-subnet subc ))))


; 03-12-92 fs debugged
( defmethod shared-initialize :around (( c constraint ) slots
				      &rest args-list )
;				      &aux  ( c-class ( class-of c )))
"Checks, if the domains of a constraint class have been already defined, an error is
signalled, if not."
  ( let* (( *interactive-mode* NIL )
	  ( c-class ( class-of c ))
	  ( undefined-domains
	    ( remove-if #'null
			( mapcar #'( lambda ( x )
				     ( remove-if #'constant-p x ))
			 ( eval ( class-slot-initform c-class 'domains ))))))
    ( if undefined-domains
	 ( error ; "define domains as primitive."
		   "You are trying to instatiate a constraint of type ~a, which is~
	          ~%defined using the following undefined domains: ~{~a ~}"
		   c-class
		   undefined-domains )
	 ( call-next-method ))))


( defmethod initialize-instance :after (( dc disjunctive-constraint ) &rest initargs )
  ( declare ( ignore initargs ))
  ( deactivate-subnet dc ))


( defmethod initialize-instance :after (( c constraint ) &rest initargs )
  "Activates newly created constraint."
  ( declare ( ignore initargs ))
  ( activate c ))






