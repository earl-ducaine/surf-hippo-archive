;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX; -*-


;;; module: CONTAX TO COLAB INTERFACE

;;; This module provides the interface of the COLAB shell towards CONTAX.
;;; For a description of the commands see the files ~contax/papers/manual
;;; and ~contax/papers/specification (LaTeX and DVI)


;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES:

;;; 29-06-92 fs : removed a quotation error and the symbol local from the import list
;;; 03-09-92 mk : Adaptation to Lucid-Lisp
;;; 21-10-92 fs : added LOAD-COLAB-FILE

( in-package "CONTAX" )

( export '( *contax-database*
	    load-colab-file ))

( export '( cn
	    destroy spy lispeval listing convert hacinit ctrace reconsult local global
	    dd di tt pc lc dc cc cv ci ))


( import '( user::*col-list-of-contax*
	    user::colab-consult-contax user::colab-destroy-contax user::colab-list-contax user::colab-consult
	    ))


;;; make contax commands known to COLAB shell
( setf *col-list-of-contax*
  '(( cn        ""  nil  colab-consult-contax     nil
     "Adds a knowledge item to the *CONTAX-DATABASE*." )
    ( destroy   ""  nil  colab-destroy-contax     nil
     "Totally resets *CONTAX-DATABASE*." )
    ( spy       ""  t    spy-fn                     t
     "Starts spying.")
    ( dd        ""  nil  apply-dd                 nil
     "." )
    ( tt        ""  nil  apply-tt                 nil
     "." )
    ( lispeval  ""  nil  eval-expr                nil
     "." )
    ( di        ""  nil  def-interval-fn          nil
     "." )
    ( pc        ""  nil  apply-pc                 nil
     "Defines a primitive constraint." )
    ( lc        ""  nil  apply-lc                 nil
     "Defines a LISP constraint." )
    ( dc        ""  nil  apply-dc                 nil
     "Defines a disjunctive constraint." )
    ( cc        ""  nil  apply-cc                 nil
     "Defines a conjunctive constraint." )
    ( listing   ""  nil  colab-list-contax        nil
     "Lists elements of *CONTAX-DATABASE*." )
    ( convert   ""  nil  colab-convert-hierarchy  nil
     "Converts hierarchy from TAXON to CONTAX." )
    ( hacinit   ""  nil  init-hac-fn              nil
     "Initializes the HAC data structures." )
;    ( reconsult ""  nil  contax-reconsult         nil
;     "Resets CONTAX and rereads definitions." )
    ( ctrace    ""  nil  ctrace-fn                nil
     "Traces CONTAX execution." )
    ( local     ""  nil  local-propagate          nil
     "Starts propagation achieving local consistency." )
    ( global    ""  nil  global-propagate         nil
     "Start propagation achieving global consistency." )))


( defun apply-pc ( args )
  ( colab-consult-contax ( cons 'pc args )))

( defun apply-lc ( args )
  ( colab-consult-contax ( cons 'lc args )))

( defun apply-dc ( args )
  ( colab-consult-contax ( cons 'dc args )))

( defun apply-cc ( args )
  ( colab-consult-contax ( cons 'cc args )))


( defvar *contax-database* '()
 "Alist holding all the users constraint contax-database by name." )


( defun add-definition ( cname definition )
  ( let (( data ( assoc cname *contax-database* )))
        ( if data
	     ( setf ( rest data ) definition )
	     ( setf *contax-database* ( acons cname definition *contax-database* )))))


( defun colab-destroy-contax ( userline )
"Arguments : ( userline )
Deletes all constraint and concept contax-database from the CONTAX
database as well as the state of the propagation."
  ( setf *contax-database* () )
  ( apply #'reset-fn ( mapcar #'symbol-to-keyword userline ))
  ( format *standard-output* "~&CONTAX has been reset." )
  ( values ))


( defun colab-list-contax ( item-names )
"Syntax: ( item-names )
Prints the contax-database of all the constraints with ITEM-NAMES
to *STANDARD-OUTPUT*. Undefinded names are omitted.
If no names are specified, all contax-database are printed."
 ( flet (( print-definition ( c )
	   ( format *standard-output*
	            "~%~a"
		    ( rest c ))))
         ( if ( null item-names )
	      ( mapc #'print-definition *contax-database* )
	      ( mapc #'( lambda ( x &aux ( y ( assoc x *contax-database* )))
			 ( when y
			        ( print-definition ( rest y ))))
	             item-names )))
 ( values ))


; changed 02-09-92 fs : no more introduction of domain constraints
( defun colab-consult-contax ( knowledge-item )
 "Syntax: ( knowledge-item )
Adds the KNOWLEDGE-ITEM to the CONTAX-database."
 ( let (( item-type    ( first knowledge-item ))
	( item-name    ( second knowledge-item ))
        ( item-qualify ( third knowledge-item ))
	( item-def     ( rest ( rest knowledge-item ))))
 
       ( add-definition item-name knowledge-item )

       ( case item-type
	 (( pc lc rc cc dc ) ( let* (( pars ( first item-def ))
				  ( doms ( expand-domains-list ( second item-def )))
				  ( body ( rest ( rest item-def )))
				  ( name item-name ))
				 ( case item-type
				   ( pc ( def-primitive-constraint-fn name pars doms body ))
				   ( lc ( def-lisp-constraint-fn name pars doms ( first body )))
				   ( rc ( error "RELFUN constraints not implemented." ))
				   ( cc ( def-conjunctive-constraint-fn name pars doms body ))
				   ( dc ( def-disjunctive-constraint-fn name pars doms body )))))
	 ( dd       ( def-concept-fn item-name ( first item-def )))
	 ( tt       ( import-taxon-terminology item-name ))
	 ( lispeval ( eval-expr item-name ))
;	 ( di       ( def-interval-fn item-name ( first item-def )))   
	 ( cv ( case item-qualify
		( hrcl ( make-variable-fn item-name :domains ( second item-def )
						    :type :hierarchical))
		( itvl ( make-variable-fn item-name :domains ( second item-def )
						    :type :interval))
		( otherwise ( format *trace-output* "Warning: Unknown variable type ~a.~%" item-qualify )
			    ( make-variable-fn item-name :domains ( second item-def ) 
							 :type :hierarchical ))))
	 ( ci ( eval `( make-constraint ,item-name ,( second item-def )
		 :priority ,( symbol-to-keyword ( first item-def ))
		 ,@(mapcan #'list
		    ( mapcar #'symbol-to-keyword ( parameter-names ( second item-def )))
		    ( rest ( rest item-def ))))))
   )))

( defun local-propagate ( userline &aux ( priority ( first userline ))
			                ( vars ( rest userline ))
			                var-list res )
  ( declare ( list userline vars var-list )
            ( symbol priority ))
  ( mapc #'( lambda ( x )
	     ( if ( listp x )
	          ( block init-ass-user-defined
		          ( push ( first x ) var-list )
			  ( assign-fn ( find-variable ( first x )) ( rest x )))
		  ( block init-ass-unconstrained
		          ( push x var-list )
; changed 04-09-92 fs:
; Variable symbols now no longer lead to their initialization with the whole domain.
; They are only mentioned in the solution.
;			  ( assign-fn  ( find-variable x ) ( var-doms ( find-variable x )))
)))
         vars )
  ( propagate-fn :consistency-mode :local
		 :minimum-priority ( symbol-to-keyword priority ))
  ( format *standard-output*
	   "~%~a"
	   ( setf res ( mapcar #'( lambda ( x )
				   ( cons x ( roots ( values-fn ( find-variable x )))))
			       var-list )))
  res )


( defun global-propagate ( userline &aux ( priority ( first userline ))
                                         ( vars     ( rest userline ))
					 var-list res pos )
  ( declare ( list userline vars var-list res )
            ( symbol priority )
	    ( integer pos ))
  ( mapc #'( lambda ( x )
	     ( if ( listp x )
	          ( block init-ass-user-defined
		          ( push ( first x ) var-list )
			  ( assign-fn ( find-variable ( first x )) ( rest x )))
		  ( block init-ass-unconstrained
		          ( push x var-list )
; same as above in LOCAL-PROPAGATE
;			  ( assign-fn  ( find-variable x ) ( var-doms ( find-variable x )))
)))
         vars )
  ( setf res ( propagate :consistency-mode :global
	                 :minimum-priority ( symbol-to-keyword priority )))
  ( format *standard-output* "~%~a"
			     ( setf res ( dolist ( kv
						   ( set-difference ( first res ) var-list )
						   ( remove-duplicates res :test #'equal ))
					  ( setf pos ( position kv ( first res ))
						 res ( mapcar #'( lambda ( x )
								  ( delete-nth pos x ))
							      res )))))
  res )


( defun apply-dd (l) (apply #'def-concept-fn l))

( defun apply-tt (l) (apply #'import-taxon-terminology l))

( defun eval-expr (l) (eval l))

( defun cn ( &optional cmd &rest args &aux ( userline ( cons cmd args)))
"Arguments : ( command-line )
The function CONTAX provides a complete interface to all of CONTAX's
features. The USERLINE should be a command to CONTAX which follows
the syntax in the specification."
  ( let (( *standard-output* nil )
	 ( *interactive-mode* nil) ; a bloody hack
	 )
        ( case cmd
	  ( spy                      ( spy-fn ) t )
	  ( reset                    ( reset-fn ) t )
	  ( reconsult                ( reset-fn ) ( colab-consult args ))
	  ( listing                  ( colab-list-contax args ) t )
	  ( show                     ( show-fn args ) t )
          ( hacinit    		     ( init-hac-fn ) t )
	  (( pc lc rc cc dc
	     tt dd cv ci di)         ( colab-consult-contax userline ) t )
	  ( lispeval		     ( apply #'eval-expr args ))
	  ( local                    ( local-propagate args ))
	  ( global                   ( global-propagate args ))
	  ( otherwise                ( error "Unknown command to contax: ~a." cmd )))))


(defun init-hac-fn (&rest userline)
  (declare (ignore userline))
  (init-hac))

(defun ctrace-fn (&rest userline)
  (declare (ignore userline))
  (spy-fn))

      
(defun import-taxon-terminology (concept-id)
  (cond (#+:symbolics
         (member :TAXON cl::*features*)
         #+:lucid
         (member :TAXON *features*)
	 (colab-convert-subhierarchy concept-id))
        (t (format *standard-output* "TAXON not loaded.~%")
	   nil)))


;;;; READING COLAB FILES FROM LISP

; 21-10-92 fs
( defun load-colab-file ( filename )
;  ( let (( *package* ( find-package "CONTAX" )))
  ( with-open-file ( *colab-input* filename
		     :direction :input )
    ( let (( eof-marker ( gensym ))
	   expression )
      ( loop
        ( setf expression ( read *colab-input* NIL eof-marker ))
	( when ( eq expression eof-marker )
	       ( return ))
	( apply #'cn expression ))))) ;)
