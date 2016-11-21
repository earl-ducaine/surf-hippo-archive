;; -*- Default-character-style: (:FIX :ROMAN :NORMAL); Syntax: Cltl; Base: 10; Package: CONTAX -*-
;;;
;;; module: PROGRAMMER-INTERFACE
;;;
;;; providing the following
;;;   functions: PROPAGATE-FN
;;;		 RESET-FN, SPY-FN, UNSPY-FN, SHOW-FN


;;; last changed : 17-09-91 fs

( in-package "CONTAX" )

( export '( propagate-fn
	    automatic-mode
	    reset-fn ))


;;; PROPAGATION

( defvar *inconsistent-tag* :local-tag
"Where to THROW on assignment of an empty value assignment." )

( defvar *consistency-mode* )

( defun propagate-fn ( &key (( :consistency-mode *consistency-mode* ) :local )
			    ( minimum-priority :soft )
			    ( number-of-solutions :all )
			    (( :errorp *throw-on-empty-set-assignment* ) T )
			    ( control-function 'not-provided control-function-supplied-p )
		       &aux res
			    ( *inconsistent-tag* :local-tag ))
"Starts propagation through the constraints of level :HARD down to the level given by the
:MINIMUM-PRIORITY keyword argument, trying to achieve :CONSISTENCY-MODE (either :LOCAL or
:GLOBAL). The function returns a maximum of :NUMBER-OF-SOLUTIONS (or :ALL). The :ERRORP
keyword controls, if an error should be signalled, if a variable covering is reduced to
the empty set during propagation."
  ( declare ( ignore control-function )
	    ( special *consistency-mode* *throw-on-empty-set-assignment* ))
  ( when control-function-supplied-p
	 ( warn "The :CONTROL-FUNCTION argument is no longer supported.~
		 ~%Instead, a function for a simple relaxation technique has been implemented.~
	         ~%(Please refer to the manual for more information.)" ))
  ( loop initially ( save-contax-state :start )
      for p2a from ( priority-to-number :hard ) to ( priority-to-number minimum-priority ) by 2
        as priority = ( number-to-priority p2a )
        as result = ( catch :local-tag ( propagate-locally priority ))
        do ( case ( first result )
		  ( :cerror ( cerror "Restore variable coverings"
				    "Inconsistency while propagating level: ~a"
				    ( number-to-priority p2a ))
			   ( restore-contax-state ( if ( = p2a ( priority-to-number :hard ))
						       :start
						       ( priority-to-number ( - p2a 2 ))))
			   ( return-from propagate-fn ( if ( = p2a ( priority-to-number :hard ))
							   NIL
							   ( number-to-priority ( - p2a 2 )))))
		  ( :abort ( return-from propagate-fn :abort ))
		  ( otherwise ( save-contax-state priority )))
      finally ( setf res ( number-to-priority ( - p2a 2 ))))
  ( if ( eq *consistency-mode*
	      :global )
       ( values ( propagate-globally minimum-priority number-of-solutions )
		res )))


;( defun old-propagate-fn ( &key ; ( c-net *active-constraints* )
;		         ( consistency-mode :local )
;			 ( priority :hard )
;		         ( control-function #'first control-function-supplied-p ))
;"args:  ( &key ( c-net *active-constraints* )
;	       ( consistency-mode :local )
;	       ( control-function #'first ))
;starts the propagation with all constraints in the set c-net, trying
;to achieve consistency-mode. the next inconsistent constraint from
;c-net ist selected by the control-function."
;  ( when control-function-supplied-p
;	 ( format *standard-output* "~%WARNING:~
;				     ~%The :CONTROL-FUNCTION argument is no longer supported.~
;				     ~%Instead, a function for a simple relaxation technique has been implemented.~
;		                     ~%(Please refer to the manual for more information.)" ))
;  ( when ( eq *inconsistent-tag* :local-tag )
;         ( push-var-states ))
;  ( do* ( res
;         ( current-constraint ( funcall control-function *active-constraints* )
;			      ( funcall control-function *active-constraints* )))
;        (( or ( endp *active-constraints* ))
;	 ( when ( eq *inconsistent-tag* :local-tag )
;	        ( maphash #'( lambda ( x y ) ( pop ( slot-value y 'value-stack ))) *variables* )))
;	 ( when ( eq ( first ( setf res ( catch :local-tag
;                                                ( remove-inconsistencies current-constraint ))))
;		     :error )
;	          ( cerror "restore variables states to prior the propagation."
;		           "inconsistent variable covering for variable ~a while propagating locally."
;			   ( vname ( second res )))
;		  ( pop-var-states )
;		  ( return )))
;    ( if ( eq consistency-mode :global )
;         ( if *spy-backtracking*
;	      ( traced-propagate-globally )
;	      ( propagate-globally ))
;	 ( values )))


( defun automatic-mode ( &rest new-mode )
"When called without any arguments, the function returns 2 values: the first one
being the automatic mode for value assignment, the second one being the on for
constraint instatiation. Arguments are keyword (:ASSIGNMENT or :INSTANTIATION)
value (:OFF, :SOFT ... :HARD) pairs, giving the new aim for local propagation."
  ( if ( null new-mode )
       ( values ( getf *automatic-mode* :assignment )
		( getf *automatic-mode* :instantiation ))
       ( progn ( when ( member :assignment new-mode )
		      ( setf ( getf *automatic-mode* :assignment )
			     ( getf new-mode :assignment )))
	       ( when ( member :instantiation new-mode )
		      ( setf ( getf *automatic-mode* :instantiation )
			     ( getf new-mode :instantiation )))
	       ( automatic-mode ))))

; bloody hack.



( defun reset-fn ( &key constraints variables concepts )
  "Resets certain aspects of the CONTAX system.
  :concepts     will clear the concept-tables, variables and constraints
  :variables    will clear constraints and variables, but leave the
  concept-tables untouched
  :constraints  will clear the constraints, but variables and concept-
  tables remain unchanged.
  If no parameters are specified, everything will be erased.
  However, the constraint definition will not be erased."
  (let ((args nil))
       ( when t
	      ( setf *active-constraints* '()
		     *watched-constraints* '() )
	      ( clrhash *constraint-table* )
	      ( clrhash *hac-look-up* )
	      ( maphash #'( lambda ( x y )
				   ( declare ( ignore x ))
				   ( setf ( slot-value y 'constraints ) '() ))
			*variable-table* )
	      ( format *standard-output* "~&CONTAX constraints reset." ))
       ( when ( or variables
		   concepts
		   (not (or constraints variables concepts)))
	      ( clrhash *variable-table* )
	      ( setf *variables* '()
		     *watched-variables* '() )
	      ( format *standard-output* "~&CONTAX variables reset." ))
       ( when ( or concepts
		   (not (or constraints variables concepts)))
	      ( reset-hierarchies ))
       ( setf *watch-indentation* 0
	      *consistency-states* ( initial-consistency-states ))
       ( values ))
  )




