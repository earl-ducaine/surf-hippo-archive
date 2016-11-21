;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX -*-



;;; module: CONSTRAINT QUEUES

;;; This module provides a datastructure and various functions and macros
;;; that implement a queue for constraints of different weights.


;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES: 01-09-92 mk  adapted to usage with Lucid



( in-package "CONTAX" )


( export '() )


( defvar *priority-function* #'( lambda ( x ) 0 )
  "This is a function of one parameter giving the priority of its argument
as a positive number (0 stands for maximum priority)." )


( defun priority-function ( c )
  ( priority-to-number ( priority c )))


( setf *priority-function* #'priority-function )


;;; implementation of the priority queues is a list of the following form:
;;; ((<highest priority>.(<item-1>..<item-n>)) (<second highest priority>.(<item-n+1>..<item-n+m>)) .. )
;;; reader and writer functions are not supplied for external use, so don't use them !!!

#+:symbolics
( scl:defsubst class-priority ( cl )
  ( car cl ))

#+:symbolics
( scl:defsubst class-elements ( cl )
  ( cdr cl ))

#+:lucid
( user::defsubst class-priority ( cl )
  ( car cl ))


#+:lucid
( user::defsubst class-elements ( cl )
  ( cdr cl ))



( defun make-empty-constraint-queue ()
"Args: none
Returns the empty constraint queue."
  '() )


( defun constraint-queue-empty-p ( constraint-queue )
"Args: constraint-queue
Returns T, iff CONSTRAINT-QUEUE is empty. NIL otherwise."
  ( declare ( list constraint-queue )
            ( inline endp ))
  ( endp constraint-queue ))


( defun constraint-queue-member-p ( constraint constraint-queue )
"Args: constraint constraint-queue
Returns T, iff CONSTRAINT appears anywhere within CONSTRAINT-QUEUE.
NIL, otherwise."
  ( declare ( list constraint-queue )
            ( inline some member ))
  ( some #'( lambda ( class )
	     ( member constraint ( class-elements class )))
	 constraint-queue ))


( defun constraint-queue-insert ( element queue &key ( accept-duplicates T )
						&aux ( element-priority ( funcall *priority-function* element ))
						     ( priority-class   ( first queue ))
						     ( class-priority   ( class-priority priority-class )))
"Args: element constraint-queue &key ( accept-duplicates T )
Returns a constraint queue, with ELEMENT correctly inserted.
If ACCEPT-DUPLICATES is T, EQ elements having equal priorities
are rejected, but no error is signaled."
  ( declare ( integer element-priority class-priority )
	    ( list queue priority-class ))
  ( cond (( constraint-queue-empty-p queue )
	  `(( ,element-priority ,element )))
	 (( = element-priority class-priority )
	  ( if ( and accept-duplicates
		     ( member element priority-class :test #'equal ))
	       queue
	       `(( ,element-priority ,@( class-elements ( first queue )) ,element ) ,@( rest queue ))))
	 (( < element-priority class-priority )
	  `(( ,element-priority ,element ) ,@queue ))
	 ( T
	  ( cons ( first queue )
		 ( constraint-queue-insert element ( rest queue ))))))


( defun constraint-queue-first ( constraint-queue &key errorp )
"Args: constraint-queue &key errorp
Returns the first (highest ranked) element of a non empty CONSTRAINT-QUEUE.
If the queue is empty and ERRORP is non-NIL, an error is signaled, if it
is empty and ERRORP is NIL (which is the default), NIL is returned."
  ( declare ( list constraint-queue ))
  ( if ( constraint-queue-empty-p constraint-queue )
       ( if errorp
	    ( error "Trying to get the first element of an empty constraint queue." )
	    'NIL )
       ( second ( first constraint-queue ))))


( defun constraint-queue-remove-first ( constraint-queue )
"Args: constraint-queue
Returns two values: the first being the CONSTRAINT-QUEUE with its first element
deleted and the second value being the just deleted element.
If CONSTRAINT-QUEUE is empty prior to the removal, an error is signaled."
  ( declare ( list constraint-queue ))
  ( values ( if ( = 2 ( length ( first constraint-queue )))
		( rest constraint-queue )
		`(( ,( class-priority ( first constraint-queue ))
		    ,@( rest ( class-elements ( first constraint-queue ))))
		  ,@( rest constraint-queue )))
	   ( constraint-queue-first constraint-queue :errorp T )))


( defun constraint-queue-remove-any ( constraint c-queue )
"Args: constraint constraint-queue
Returns CONSTRAINT-QUEUE with all appearances of CONSTRAINT removed."
  ( declare ( list constraint-queue ))
  ( delete-if #'( lambda ( c ) ( endp ( rest c )))
	      ( mapcar #'( lambda ( p-class )
			   ( cons ( class-priority p-class )
				  ( remove constraint ( class-elements p-class )
					   :count 1 )))
		       c-queue )))


;;; MACRO definitions
;;; Constraint queues are automatically bound to symbols, which are taken as parameters.

( defmacro set-empty-constraint-queue ( q )
"Syntax: q
Binds the symbol Q to the empty constraint queue."
  `( setf ,q ( make-empty-constraint-queue )))


( defmacro set-constraint-queue-inserted ( e q &key ( accept-duplicates T ))
  `( setf ,q ( constraint-queue-insert ,e ,q :accept-duplicates ,accept-duplicates )))


( defmacro constraint-queue-delete-first ( q )
"Syntax: q
Removes the first (highest ranked) element from the constraint queue bound
to Q and rebinds Q to the resulting queue. The removed first element is
returned."
  `( multiple-value-bind ( new-queue former-first )
			 ( constraint-queue-remove-first ,q )
			 ( setf ,q new-queue )
			 former-first ))


( defmacro constraint-queue-delete-any ( el q )
"Syntax: constraint constraint-queue
Deletes all appearances of CONSTRAINT from CONSTRAINT-QUEUE."
  `( setf ,q ( constraint-queue-remove-any ,el ,q )))


