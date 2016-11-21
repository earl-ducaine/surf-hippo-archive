;;; -*- Syntax: Cltl; Default-character-style: (:FIX :ROMAN :NORMAL); Base: 10; Mode: LISP; Package: CONTAX; -*-
;;;
;;; module: DEBUGGER
;;;
;;; This module contains routines for
;;; - standard IO via PRINT-OBJECT
;;; - tracing the actions taken by the constraint solver (activation, assignment...)
;;; - examination of the objects of the constraint solver (namely constraints and variables)


;;; 02-06-92 fs
;;; - module created

( in-package "CONTAX" )

( export '( show ))


;;; PRINTING OF Variable OBJECTS

( defmethod print-object (( v variable ) s )
  ( format s "#<~a ~a>"
             ( class-name ( class-of v ))
	     ( vname v )))


; 15-09-92 fs
( defmethod print-object (( lv local-hierarchical-variable ) stream )
  ( format stream "#<~a>" ( vname lv )))


;;; DESCRIBE VARIABLE

( defmethod describe-variable (( v variable ))
  ( format *standard-output*
	   "~&Variable ~a ranging over domains ~a. ~&~4tCurrent values: ~a"
	   ( vname v )
	   ( var-doms v )
	   ( var-vals v )))


;;; PRINTING OF Constraint OBJECTS

( defmethod print-object (( c constraint ) s )
  ( format s "#<constraint (should not be printed) ~a>"
	     ( class-name ( class-of c ))))


( defmethod print-object (( pc primitive-constraint ) s )
  ( format s "#<PRIMITIVE-CONSTRAINT ~a ~a>"
	     ( cname pc )
	     ( mapcar #'vname ( parameter-variables pc ))))


( defmethod print-object (( dc domain-constraint ) s )
  ( format s "#<DOMAIN-CONSTRAINT (should not be printed) ~a>"
	     ( cname dc )))


( defmethod print-object (( lc lisp-constraint ) s )
  ( format s "#<LISP-CONSTRAINT ~a ~a>"
	     ( cname lc )
	     ( mapcar #'vname ( parameter-variables lc ))))


( defmethod print-object (( cc compound-constraint ) s )
 ( format s "#<COMPOUND-CONSTRAINT (should not be printed) ~a>"
	    ( cname cc )))


( defmethod print-object (( cc conjunctive-constraint ) s )
 ( format s "#<CONJUNCTIVE-CONSTRAINT ~a ~a>"
	    ( cname cc )
	    ( mapcar #'vname ( parameter-variables cc ))))


( defmethod print-object (( dc disjunctive-constraint ) s )
 ( format s "#<DISJUNCTIVE-CONSTRAINT ~a ~a>"
	    ( cname dc )
	    ( mapcar #'vname ( parameter-variables dc ))))


( defmethod print-object (( sp cn::savepoint ) s )
 ( format s "#<CONTAX SAVEPOINT from ~2,'0d:~2,'0d:~2,'0d on ~a~[~;~st~;nd;~rd~:;th~] ~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~a>"
	    ( third ( time-created sp ))
	    ( second ( time-created sp ))
	    ( first ( time-created sp ))
	    ( fourth ( time-created sp ))
	    ( mod ( fourth ( time-created sp )) 10 )
	    ( fifth ( time-created sp ))
	    ( sixth ( time-created sp ))))


;;; DESCRIBE-CONSTRAINT

( defmethod describe-constraint :around (( c constraint ))
 ( let* (( pnames ( parameter-names c ))
	 ( plength ( length pnames ))
	 ( vars ( mapcar #'( lambda ( x ) ( funcall x c )) pnames ))
	 ( vnames ( mapcar #'vname vars )))
        ( format *standard-output*
	         "~%~d"
		 ( make-string 80 :initial-element #\* ))
	( format *standard-output*
	         "~%~a ~a is a "
		 ( cname c )
		 vnames )
	( call-next-method )
	( format *standard-output*
	         "~%Its parameter~p~:*, binding~p~:* and covering~p~:* ~[~;is~:;are~] as follows:~
                  ~{~%~5T~d:~(~6T~d~): ~d~}"
		 plength
		 ( mapcan #'list pnames vnames ( mapcar #'roots ( mapcar #'var-vals vars ))))
	( format *standard-output*
	         "~%~d"
		 ( make-string 80 :initial-element #\* ))
	( values )))

 
( defmethod describe-constraint (( pc primitive-constraint ))
  ( format *standard-output*
           "primitive constraint (of type ~a ~a).~%~
            It is defined by the following tuple~p:~{~%~5T~d~}"
	   ( class-name ( class-of pc ))
	   ( parameter-names pc )
	   ( length ( c-def pc ))
	   ( c-def pc )))


( defmethod describe-constraint (( lc lisp-constraint ))
  ( format *standard-output*
           "lisp defined contraint (of type ~a ~a).~
            ~%It is defined by the function named: ~%~5T~a"
	   ( class-name ( class-of lc ))
	   ( parameter-names lc )
	   ( l-fun lc )))


( defmethod describe-constraint (( cc compound-constraint ))
 ( let* (( comps ( components cc ))
	 ( clen  ( length comps )))
        ( format *standard-output*
                 "~a constraint (of type ~a ~a).~%~
                  It is defined as a combination of the following component~p:~{~%~5T~a ~a~}"
		 ( if ( conjunctive-constraint-p cc )
		      "conjunctive"
		      "disjunctive" )
		 ( class-name ( class-of cc ))
		 ( parameter-names cc )
		 clen
		 ( mapcan #'list
		          ( mapcar #'first comps )
			  ( mapcar #'( lambda ( x )
				       ( mapcar #'( lambda ( y )
						    ( vname ( funcall y cc )))
					        x ))
			           ( mapcar #'rest comps ))))))


;;; VARIABLES & METHODS supporting simple SPY

( defvar *spy-activation* NIL
  "Flag for tracing of activation/deactivation of constraints." )

( defvar *spy-propagation* NIL )

( defvar *spy-assignment* NIL )

( defvar *spy-backtracking* NIL )


( defun spy-fn ( &rest what-to-spy )
  ( setf *spy-activation*   ( or ( when ( member :activation  what-to-spy ) T )
			         ( null what-to-spy ))
	 *spy-propagation*  ( or ( when ( member :propagation what-to-spy ) T )
			         ( null what-to-spy ))
	 *spy-assignment*   ( or ( when ( member :assignment what-to-spy ) T )
			         ( null what-to-spy ))
	 *spy-backtracking* ( or ( when ( member :backtracking what-to-spy ) T )
			         ( null what-to-spy )))
  ( when *spy-activation*
	 ( format *trace-output* "~%Activation of constraints will be traced." ))
  ( when *spy-propagation*
	 ( format *trace-output* "~%Propagation of constraints will be traced." ))
  ( when *spy-assignment*
	 ( format *trace-output* "~%Assignment to variables will be traced." ))
  ( when *spy-assignment*
	 ( format *trace-output* "~%Backtracking will be traced." ))
  ( values ))


( defun unspy-fn ( &rest what-to-unspy )
  ( setf *spy-activation*   ( unless ( or ( member :activation  what-to-unspy )
					  ( null what-to-unspy )) T )
	 *spy-propagation*  ( unless ( or ( member :propagation what-to-unspy )
				          ( null what-to-unspy )) T )
	 *spy-assignment*   ( unless ( or ( member :assignment  what-to-unspy )
				          ( null what-to-unspy )) T )
	 *spy-backtracking* ( unless ( or ( member :assignment  what-to-unspy )
				          ( null what-to-unspy )) T ))
  ( when *spy-activation*
	 ( format *trace-output* "~%Activation of constraints will be traced." ))
  ( when *spy-propagation*
	 ( format *trace-output* "~%Propagation of constraints will be traced." ))
  ( when *spy-assignment*
	 ( format *trace-output* "~%Assignment to variables will be traced." ))
  ( when *spy-assignment*
	 ( format *trace-output* "~%Backtracking will be traced." ))
  ( values ))


( setf ( symbol-function 'spy )   ( symbol-function 'spy-fn )
       ( symbol-function 'unspy ) ( symbol-function 'unspy-fn ))


( defun show-fn ( object )
  "Function printing a description of a CONTAX object."
  ( cond (( variable-name-p object )
	  ( describe-variable ( find-variable object )))
	 (( variable-p object )
	  ( describe-variable object ))
	 (( constraint-instance-name-p object )
	  ( describe-constraint ( find-constraint object )))
	 ( T
	  ( format *error-output* "~&This is not a CONTAX object: ~a." object ))))


( defmacro show ( &rest object-names )
  "Macro for showing a brief decription of any number of CONTAX objects."
  ( mapc #'show-fn object-names )
  ( values ))


;;; TRACE OUTPUT

( defmethod activate :before (( c constraint ))
"Cares for spying output."
  ( when *spy-activation*
       	 ( format *trace-output*
		  "~%Queueing constraint ~a." c )))

( defmethod deactivate :before (( c constraint ))
"Just cares for spying output."
  ( when *spy-activation*
	 ( format *trace-output*
		  "~%Unqueueing constraint ~a." c )))

( defmethod remove-inconsistencies :before (( c constraint ))
"Cares for spying output."
  ( when *spy-propagation*
	 ( format *trace-output*
		  "~%Propagating through constraint ~a." c )))


;;; the WATCHer

( defvar *anything-watched-p* NIL
  "Flag indicating if anything is watched at all." )

( defvar *watched-constraints* '()
  "List of watched constraints." )

( defvar *watched-variables* '()
  "List of watched variables." )

( defvar *watch-indentation* 0
  "Tabulation, where to start tracing output." )


( defun print-on-activation ( c )
  "Print function on activation of a constraint."
  ( format *trace-output*
	   "~&~vtActivating constraint ~a [~a]."
	   *watch-indentation*
	   ( cname c )
	   ( class-name ( class-of c ))))


( defun print-on-deactivation ( c )
  "Print function on deactivation of a constraint."
  ( format *trace-output*
	   "~&~vtDeactivating constraint ~a [~a]."
	   *watch-indentation*
	   ( cname c )
	   ( class-name ( class-of c ))))


( defun print-on-propagation ( c )
  "Print function on propagation through a constraint."
  ( format *trace-output*
	   "~&~vtPropagating through constraint ~a [~a]."
	   *watch-indentation*
	   ( cname c )
	   ( class-name ( class-of c ))))


( defconstant +watch-constraint-aspects+ '( :on-activation   print-on-activation
					    :on-deactivation print-on-deactivation
					    :on-propagation  print-on-propagation )
  "Standard method for watching a constraint." )


( defun print-on-assignment ( var val )
  "Print function on variable assignment."
  ( format *trace-output*
	   "~&~vtSetting variable ~a [~a].~&~vtOld value: ~a -> New value: ~a."
	   *watch-indentation*
	   ( vname var )
	   ( class-name ( class-of var ))
	   ( + *watch-indentation* 4 )
	   ( var-vals var )
	   val ))


( defconstant +watch-variable-aspects+ '( :on-assignment print-on-assignment )
  "Standard method for watching a variable." )


( defun watch-fn ( what-to-watch
		   &aux ( extented-what-to-watch ( mapcar #'( lambda ( x )
							      ( if ( listp x )
								   x
								   ( list x T )))
							  what-to-watch ))
			obj-name
			obj-list )
  ( dolist ( obj extented-what-to-watch )
	   ( setf obj-name ( first obj )
		  obj-list ( rest obj ))
	   ( cond (( constraint-instance-name-p obj-name )
		   ( pushnew obj-name *watched-constraints* )
		   ( setf ( cwatched-p ( find-constraint obj-name ))
			  ( if ( equal obj-list '( T ))
			       +watch-constraint-aspects+
			       ; the following commands should do some parsing of
			       ; the aspects and be able to replace parts of them
			       obj-list )))
		  (( variable-name-p obj-name )
		   ( pushnew obj-name *watched-variables* )
		   ( setf ( vwatched-p ( find-variable obj-name ))
			  ( if ( equal obj-list '( T ))
			       +watch-variable-aspects+
			       ; as above
			       obj-list )))
		  ( T
		   ( format *error-output* "~& Unknown object name: ~a." obj-name ))))
  ( setf *anything-watched-p* ( or *watched-constraints* *watched-variables* ))
  ( format *trace-output*
	   "~&Watched constraints (by name):~%~2t~{~a  ~}."
	   *watched-constraints* )
  ( format *trace-output*
	   "~&Watched variables (by name):~%~2t~{~a  ~}."
	   *watched-variables* )
  ( values ))


( defmacro watch ( &rest things )
  `( watch-fn ',things ))


( defun unwatch-fn ( what-to-unwatch )
  ( dolist ( obj what-to-unwatch )
	   ( cond (( constraint-instance-name-p obj )
		   ( setf *watched-constraints* ( remove obj *watched-constraints*
							 :count 1 ))
		   ( setf ( cwatched-p ( find-constraint obj )) NIL ))
		  (( variable-name-p obj )
		   ( setf *watched-variables* ( remove-once obj *watched-variables* ))
		   ( setf ( vwatched-p ( find-variable obj )) NIL ))
		  ( T
		   ( format *error-output* "~& Unknown object name: ~a." obj ))))
  ( setf *anything-watched-p* ( or *watched-constraints* *watched-variables* ))
  ( format *trace-output*
	   "~&Watched constraints (by name):~{~%~4t~a~}."
	   *watched-constraints* )
  ( format *trace-output*
	   "~&Watched variables (by name):~{~%~4t~a~}."
	   *watched-variables* )
  ( values ))


( defmacro unwatch ( &rest things )
  `( unwatch-fn ',things ))


( defun contax-state ( &aux ( count 0 ) )
  ( format *trace-output*
           "~&CONTAX Constraint Solver - Report" )
  ( format *trace-output*
           "~&=================================" )
  ( format *trace-output*
           "~&#Variables  : ~a (~a are local)"
	   ( hash-table-count *variable-table* )
	   ( prog2 ( maphash #'( lambda ( k v )
				 ( declare ( ignore k ))
				 ( when ( local-variable-p v )
					( incf count )))
			     *variable-table* )
		   count ))
  ( format *trace-output*
           "~&#Constraints: ~a  (~a currently active) ~%Constraint queue: ~{~a  ~}"
	   ( hash-table-count *constraint-table* )
	   ( apply #'+ ( mapcar #'length ( mapcar #'cdr *active-constraints* )))
	   *active-constraints* )
  ( values ))

