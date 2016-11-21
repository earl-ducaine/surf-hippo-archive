;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX -*-



;;; module: ASSISTANT FUNCTIONS

;;; This files provides various general functions to be used by CONTAX, which
;;; are not especially included in one of the various other modules.




;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES:


( in-package "CONTAX" )


( defun symbol-to-keyword ( sym )
"Args: ( symbol )
Returns a keyword made of the print-name of SYMBOL."
  ( declare ( symbol sym ))
  ( intern ( string-upcase ( symbol-name sym ))
	   "KEYWORD" ))


( defvar *local-variable-symbol-counter* 0 )

( defun local-variable-name ( &aux new-sym )
  ( prog1
    ( setf new-sym #+:symbolics 
                      ( sys:gensymbol "LOCAL-VARIABLE-"
				      ( incf *local-variable-symbol-counter* ))
                   #-:symbolics 
                      ( gensym "LOCAL-VARIABLE-" ) )
    ( import new-sym "CONTAX" )))


( defun compute-combinations ( sets )
"Args: ( sets )
Returns a list off all possible combinations of the sets in SETS."
  ( declare ( list sets u v x y )
            ( inline cons ))
  ( reduce #'( lambda ( u v )
	       ( mapcan #'( lambda ( x )
			    ( mapcar #'( lambda ( y )
					 ( cons x y ))
			             u ))
		        v ))
           ( reverse sets )
	   :initial-value ( make-list 1 )))


( defun fold ( tup-set &aux ( res '() ))
  ( declare ( list tup-set res x )
            ( integer i )
	    ( inline nth ))
  ( dotimes ( i ( length ( first tup-set )) ( nreverse res ))
	    ( push ( delete-duplicates ( mapcar #'( lambda ( x )
						    ( nth i x ))
						tup-set ))
				       res )))


( defun my-assoc ( key val )
  ( declare ( symbol key )
            ( list val ))
  ( let (( res ( assoc key val )))
        ( if res 
	     ( rest res )
	     nil )))


( defun delete-nth ( n list )
"Args: ( n list )
Destructively deletes the N-th element of LIST."
  #-:symbolics ( nconc ( subseq list 0 n ) ( subseq list ( 1+ n )))
  #+:symbolics ( setf ( nthcdr n list ) ( nthcdr ( 1+ n ) list ))
  list )




; 15-09-92 fs
( defun filter ( sym lst
		 &rest additional-args
		 &aux  ( n ( apply #'position sym ( first lst ) additional-args )) )
"Args: ( symbol list &rest keywords-to-position )
Given LIST is of structure
(( key-1 ... key-n )( value-1-1 ... value-1-n )...( value-m-1 ... value-m-n ))
and SYMBOL appears as one of the key-i (in the sense of the POSITION function,
key-i is deleted from the first list and value-j-i from the remaining lists."
  ( if n
       ( mapcar #'( lambda ( x )
		    ( delete-nth n x ))
		lst )
       lst ))


( defun class-slot-initform ( class slot-name )
"Args: class slot-name
This function returns the initform for the slot named SLOT-NAME of CLASS."
  ( slot-definition-initform ( find-if #'( lambda ( x )
					   ( eq ( slot-definition-name x )
					        slot-name ))
				       ( class-slots class ))))


( defun remove-once ( el li )
  ( declare ( list li )
            ( inline endp eq cons rest ))
  ( cond (( endp li )
	  '() )
         (( eq el ( first li ))
	  ( rest li ))
	 ( T
	  ( cons ( first li )
		 ( remove-once el ( rest li ))))))


; 07-10-92 fs
( defun find-minimum ( l
		       &key ( test #'< )
			    ( key  #'identity )
			    ( min ( first l ))
		       &aux ( min-val ( funcall key min )))
  ( dolist ( x l min )
    ( if ( funcall test ( funcall key x )
			 min-val )
      ( setf min     x
	     min-val ( funcall key x )))))

; 07-10-92 fs
( defun funcall-pairwise ( function set )
"Calls FUNCTION with every distinct pair of values out of SET."
  ( if ( = 1 ( length set ))
       ( first set )
       (progn ( reduce #'( lambda ( x y )
		             ( funcall function x y )
		           x )
	   	       set )
              ( funcall-pairwise function ( rest set )))))


( defun expand-domains-list ( domains-list )
  ( mapcar #'( lambda ( domain )
	       ( if ( symbolp domain )
		    `( ,domain )
		    domain ))
	   domains-list ))


;;; shortcuts

( defun load-all ()
  ( warn "~%This function (CN::LOAD-ALL) is no longer supported, please~
	  ~%remove all references to it (It might be deleted)." ))

( defun compile-all ()
  ( warn "~%This function (CN::COMPILE-ALL) is no longer supported, please~
	  ~%remove all references to it (It might be deleted)." ))

