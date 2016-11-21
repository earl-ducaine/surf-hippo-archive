;;; -*- Syntax: Cltl; Default-character-style: (:FIX :ROMAN :NORMAL); Base: 10; Mode: LISP; Package: CONTAX; -*-
;;;
;;; module: CONTROL
;;;

;;; 02-06-92 fs
;;; - all references to DOMAIN-CONSTRAINTS have been commented out
;;; changed : 03-09-92 mk  Adaptation to Lucid-Lisp and removal of CTRL-sequences
;;; 16-11-92 fs
;;; - added *automatic-mode*, a variable determinating automatic start of propagation on
;;;   variable assignment or constraint instatiation


( in-package "CONTAX" )

( export '( make-savepoint
	    back-to-savepoint ))


;;;;; VARIABLES

( defvar *active-constraints* NIL
"Set of all constraints, which have to be checked." )


( defvar *automatic-mode* '( :assignment :off :instantiation :off )
"Variable controlling the automatic start of propagation on various occassions.
Possible occassions are :ASSIGNMENT and :INSTATIATION ; controlling values are
:OFF and :HARD to :SOFT. :OFF turns automatic propagation off and any of the 
hardnesses starts local propagation trying to achieve the specified hardness." )


( defvar *interactive-mode* T
"Variable indicating, if CONTAX is currently in interactive mode or propagating." )


;( defvar *control-function* #'first 
;  "Function selecting the next constraint to deactivate from *ACTIVE-CONSTRAINTS*." )


;;;;; mapping of PRIORITIES to NUMBERS and VV

( defun priority-to-number ( priority )
  ( case priority
    ( :hard   1 )
    ( :strong 3 )
    ( :medium 5 )
    ( :weak   7 )
    ( :soft   9 )))


( defun number-to-priority ( number )
  ( case number
    ( 1 :hard   )
    ( 3 :strong )
    ( 5 :medium )
    ( 7 :weak   )
    ( 9 :soft   )))


;;;;; METHODS on constraints

( defgeneric activate ( constraint ))


;( defmethod activate :around (( dc domain-constraint ))
;  "Activate it only once."
;  ( declare ( inline activated-p ))
;  ( unless ( activated-p dc )
;           ( setf ( activated-p dc ) T )
;	   ( call-next-method )))


( defmethod activate :around (( cc conjunctive-constraint ))
  "Do nothing."
  ( values ))


( defmethod activate :around (( c constraint ))
  "Prevents further activity, if already active."
  ( unless ( constraint-queue-member-p c *active-constraints* )
	   ( when ( and *anything-watched-p*
			( member :on-activation ( cwatched-p c )))
		  ( funcall ( getf ( cwatched-p c ) :on-activation ) c ))
           ( call-next-method )))


( defmethod activate (( c constraint ))
  "Activates constraint C."
  ( set-constraint-queue-inserted c *active-constraints* :accept-duplicates NIL ))



( defgeneric deactivate ( constraint ))


( defmethod deactivate :around (( cc conjunctive-constraint ))
  "Do nothing."
  ( values ))


( defmethod deactivate :around (( c constraint ))
  "Prevents further activity, if already inactive."
  ( when ( constraint-queue-member-p c *active-constraints* )
	 ( when ( and *anything-watched-p*
		      ( member :on-deactivation ( cwatched-p c )))
		( funcall ( getf ( cwatched-p c ) :on-deactivation ) c ))
         ( call-next-method )))


( defmethod deactivate (( c constraint ))
  "Deactivates constraint C."
  ( constraint-queue-delete-any c *active-constraints* ))


;;; REMOVAL OF INCONSISTENCIES

( defmethod remove-inconsistencies :around (( c constraint ))
  "Do tracing."
  ( deactivate c )
  ( when ( and *anything-watched-p*
	       ( member :on-propagation ( cwatched-p c )))
	 ( incf *watch-indentation* 4 )
	 ( funcall ( getf ( cwatched-p c ) :on-propagation ) c ))
  ( call-next-method )
  ( when ( and *anything-watched-p*
	       ( member :on-propagation ( cwatched-p c )))
	 ( decf *watch-indentation* 4 )))


( defmethod remove-inconsistencies :around (( cc conjunctive-constraint ))
  "Do nothing."
  ( values ))


( defmethod remove-inconsistencies (( dc disjunctive-constraint ))
  ( loop for comp in ( constraints dc )
         and maps in ( parameter-mappings dc )
         and *variables* in ( variable-sets dc )
         ; update component variables from disjunctives vars
         do ( loop for map in maps
                     as disj-var = ( funcall ( car map ) dc )
                     as comp-var = ( funcall ( cdr map ) comp )
                     do ( setf ( slot-value comp-var 'values ) ( var-vals disj-var )))
         ; propagate each component
         do ( let (( *active-constraints* ( make-empty-constraint-queue ))
		   ( *consistency-states* ( initial-consistency-states )))
                  ( declare ( special *active-constraints*
                                      *consistency-mode*
				      *consistency-states* ))
                  ( activate comp )
                  ( propagate-fn :consistency-mode *consistency-mode* :errorp NIL )
		  ; ( format t "~%-> within let in r-i (dc): ~a" *active-constraints* )
		  ))
         ; update disjunctives vars from components
  ( loop for disj-par in ( variable-names dc )
           as disj-var = ( funcall disj-par dc )
           do ( assign-fn disj-var ( loop with union = '() and match = '()
                                          for comp in ( constraints dc )
                                          for map in ( parameter-mappings dc )
                                            when ( setf match
                                                        ( member disj-par map :key #'car ))
                                              do ( setf union ( union union
                                                                       ( var-vals ( funcall ( cdr ( first match )) comp ))))
                                          finally ( return union )))))
;  finally ; check for inconsistencies
;                          ( loop for var in ( variables dc )
;                                   when ( endp ( var-vals var ))
;                                     do ( throw *inconsistent-tag* `( :error ,var ))))))


( defmethod remove-inconsistencies (( pc primitive-constraint ))
  "Remove inconsistencies in variables appearing in the constraint PC."
;  ( hac-check pc )
    ( let* (( vars ( mapcar #'( lambda ( x ) ( funcall x pc ))
                            ( parameter-names pc )))
            ( res  ( fold ( nintersection ( compute-combinations ( mapcar #'var-expanded-vals vars ))
                                          ( comp-c-def pc )
                                          :test #'equal ))))
           ( when ( null res )
                  ( setf res ( make-list ( length vars ) :initial-element nil ))
)
           ( mapcar #'( lambda ( x y ) ( setf ( var-vals x ) y ))
                    vars
                    res ))
  ( values ))


( defmethod remove-inconsistencies (( lc lisp-constraint ))
  "Remove inconsistencies in variable appearing within LC."
  ( declare ( list vars res y )
            #+:symbolics
            ( inline ( setf var-vals )))
    ( let* (( vars ( mapcar #'( lambda ( x ) ( funcall x lc ))
		 	    ( parameter-names lc )))
;	    ( res ( fold ( remove-if-not ( symbol-function ( l-fun lc ))
;					 ( compute-combinations ( mapcar #'var-expanded-vals vars ))))))
	    ( res ( fold ( mapcan #'( lambda ( args )
				       ( when ( apply ( symbol-function ( l-fun lc )) args )
					      ( list args )))
	       			   ( compute-combinations ( mapcar #'var-expanded-vals vars ))))))
          ( when ( endp res )
	         ( setf res ( make-list ( length vars ) :initial-element nil )))
	  ( mapcar #'( lambda ( x y ) ( setf ( var-vals x ) y ))
		   vars
		   res )))


;( defmethod remove-inconsistencies :after (( c constraint ))
;  ( deactivate c ))


;;; STATE SAVING & RETRIEVAL

( defclass savepoint ()
  (( time-created         :reader   time-created
			  :initform ( multiple-value-list ( get-decoded-time )))
   ( active-variables     :reader  active-variables
			  :initarg :active-variables )
   ( variable-values      :reader  variable-values
			  :initarg :variable-values )
   ( variable-constraints :reader  variable-constraints
			  :initarg :variable-constraints )
   ( active-constraints   :reader  active-constraints
			  :initarg :active-constraints )))


( defun make-savepoint ()
"Internally save all important aspects of CONTAX and returns a symbol to identify
the savepoint. This symbol can later be used as an argument to BACK-TO-SAVEPOINT,
which then restores the state of the constraint solver to the one at the time of
the call to the corresponding MAKE-SAVEPOINT."
  ( make-instance 'savepoint
		  :active-variables *variables*
		  :variable-values ( mapcar #'( lambda ( var )
					        `( ,var ,( var-vals var )))
					    *variables* )
		  :variable-constraints ( mapcar #'( lambda ( var )
					        `( ,var ,( var-cons var )))
					    *variables* )
		  :active-constraints *active-constraints* ))


( defun back-to-savepoint ( savepoint )
  ( setf *variables*          ( active-variables savepoint )
	 *active-constraints* ( active-constraints savepoint ))
  ( dolist ( var-val-pair ( variable-values savepoint ))
	   ( setf ( slot-value ( first var-val-pair ) 'values ) ( second var-val-pair )))
  ( dolist ( var-con-pair ( variable-constraints savepoint ))
	   ( setf ( slot-value ( first var-con-pair ) 'constraints ) ( second var-con-pair ))))


( defun initial-consistency-states ()
  ( copy-list '( :hard   NIL
		 :strong NIL
		 :medium NIL
		 :weak   NIL
		 :soft   NIL
		 :start  NIL )))


( defvar *consistency-states* ( initial-consistency-states ))

( defun save-CONTAX-state ( &optional hardness )
 ( setf ( getf *consistency-states* hardness ) ( make-savepoint )))


( defun restore-CONTAX-state ( &optional hardness )
 ( back-to-savepoint ( getf *consistency-states* hardness )))


;;; LOCAL PROPAGATION

( defvar *current-constraint* NIL
"Holds the constraint currently being revised." )


( defun propagate-locally ( priority-to-achieve )
  ( let ( *current-constraint*
	 ( *interactive-mode* NIL ))
	( loop
	  ( when ( or ( constraint-queue-empty-p *active-constraints* )
		      ( < ( priority-to-number priority-to-achieve )
			  ( priority-to-number ( priority ( constraint-queue-first *active-constraints* )))))
		 ( return ))
	  ( setq *current-constraint* ( constraint-queue-first *active-constraints* ))
	  ( remove-inconsistencies *current-constraint* ))))


;;; GLOBAL PROPAGATION

( defun propagate-globally ( priority-to-achieve solutions-to-compute )
  ( declare ( list vars solutions vs )
            #+:symbolics 
	    ( inline eq first rest list var-vals ( setf var-vals )))
  ( let (( solutions '() )
	 ( number-of-solutions 0 ) 
	 ( *inconsistent-tag* :global-tag )
	 ( *interactive-mode* NIL ))
	( declare ( special *inconsistent-tag*
			    *interactive-mode* ))
        ( labels (( prop ( vs )
		    ( if ( endp vs )
			 ( prog1
			   ( push ( mapcap #'var-vals *variables* ) solutions )
			   ( when *spy-backtracking*
				  ( format *trace-output*
					   "~&Solution found: ~a."
					   ( first solutions )))
			   ( when ( and ( not ( eq :all solutions-to-compute ))
					( = ( incf number-of-solutions )
					    solutions-to-compute ))
				  ( throw 'et-reicht solutions )))
			 ( loop initially ( push-var-states vs )
			        with var = ( first vs )
			        for val in ( var-expanded-vals var )
				  do ( setf ( var-vals var ) ( list val ))
				     ( when *spy-backtracking*
					    ( format *trace-output*
						     "~&Choosing ~a for ~a."
						     val
						     ( vname var )))
				  unless ( eq ( first ( catch :global-tag
							      ( propagate-locally priority-to-achieve )))
					      :cerror )
				    do ( when *spy-backtracking*
					       ( format *trace-output* " Choice successful." ))
				       ( prop ( rest vs ))
				  do ( restore-var-states vs :notify-constraints-p NIL )
				     ( set-empty-constraint-queue *active-constraints* )
				finally ( pop-var-states vs )
				  ))))
; Sort all variables of the net in increasing order by the size of the current
; variable covering.
	 ( setf *variables* ( sort *variables* #'< :key #'( lambda ( v )
                                                            ( length ( var-expanded-vals v )))))
	 ( catch 'et-reicht ( prop *variables* ))
	 ( setf solutions ( cons ( mapcar #'vname *variables* )
			   solutions ))
	 ( mapc #'( lambda ( local )
		    ( setf solutions ( filter local solutions )))
		( mapcar #'vname ( remove-if-not #'local-variable-p *variables* )))
	 ( values ( delete-duplicates solutions :test #'equal )))))

; old version
;	( maphash #'( lambda ( x y )
;		      ( declare ( ignore x ))
;		      ( push ( list y ( length ( var-expanded-vals y ))) vars ))
;	          *variables* )
;	( setf vars ( mapcar #'first ( sort vars #'< :key #'second )))
;	( prop vars )
;	( cons ( mapcar #'vname vars )
;	       solutions ))))



;old version see above
