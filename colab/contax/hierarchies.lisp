;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX; -*-


;;; module: HIERARCHIES

;;; This modules provides all functions to build, maintain and access the
;;; hierarchically structured domains of the CONTAX variables.


;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES:



( in-package "CONTAX" )

( export '( top

	    def-concept-fn
	    def-interval-fn
	    constant-p
	    subconcepts
	    superconcepts
	    leaves
	    expand
	    roots
	    includes-p
            hset-union
	    hset-intersection
	    reset-hierarchies
;	    print-hierarchies
))


;;; VARIABLES

( defvar *subconcepts* ( make-hash-table )
  "Hashtable holding the names of all all the subconcepts of a concept-name." )

( defvar *superconcepts* ( make-hash-table )
  "Hashtable holding the names of all the superconcepts of a concept-name." )


;;; BUILDING the HIERARCHY

( defun reset-hierarchies ()
"Resets the hierarchy structure, ie. removes all concept definitions."
  ( clrhash *subconcepts* )
  ( setf ( gethash 'TOP *subconcepts* ) '() )
  ( clrhash *superconcepts* )
  ( setf ( gethash 'TOP *superconcepts* ) '() )
  ( format *standard-output* "~&CONTAX concepts reset." ))


( defun subconcepts ( conc-name )
"Returns a list containing the names of all immediate subconcepts."
  ( gethash conc-name *subconcepts* :undefined ))


( defsetf subconcepts
  ( super )
  ( subs )
  `( setf ( gethash ,super *subconcepts* ) ,subs ))


( defun superconcepts ( conc-name )
"Returns a list containing the names of all immediate superconcepts."
  ( gethash conc-name *superconcepts* :undefined ))


( defsetf superconcepts
  ( sub )
  ( supers )
  `( setf ( gethash ,sub *superconcepts* ) ,supers ))


( defun def-concept-fn ( super subs )
"Defines a concept in terms of its direct subconcepts, which are
also introduced to the CONTAX system.
The hierarchy tree is set accordingly."
  ( if ( eq ( superconcepts super ) :undefined )
       ( block new-super
	       ( setf ( superconcepts super ) 'TOP
		      ( subconcepts super ) '() )
	       ( pushnew super ( subconcepts 'TOP ))))
  ( dolist ( sub ( set-difference ( subconcepts super )
				  subs )) ; these subconcepts become now obsolete
	   ( let (( old-supers ( superconcepts sub )))
		 ( if ( = 1 ( length old-supers ))
		      ( block set-super-to-TOP
			      ( setf ( superconcepts sub ) 'TOP )
			      ( pushnew super ( subconcepts 'TOP )))
		      ( setf ( superconcepts sub ) ( remove super old-supers )))))
  ( dolist ( sub ( set-difference subs
				  ( if ( listp ( superconcepts super ))
				       ( superconcepts super )
				       '() ))) ; theses subconcepts are new wrt. super
	   ( when ( eq ( subconcepts sub ) :undefined )
		  ( setf ( subconcepts sub ) '() ))
	   ( if ( listp ( superconcepts sub ))
		( pushnew super ( superconcepts sub ))
		( setf ( superconcepts sub ) ( list super )
		       ( subconcepts 'TOP ) ( remove sub ( subconcepts 'TOP )))))
  ( setf ( subconcepts super ) subs ))


;( defun def-interval-fn ( name i )
;  ( def-concept-fn namel ( read-intervalls i )))


;;; FUNCTIONS

( defun constant-p ( symbol )
"Returns T, iff SYMBOL is the name of a concept, NIL otherwise."
  ( second ( multiple-value-list ( gethash symbol *subconcepts* ))))


( defun leaves ( conc-names )
"Returns a list containing all the primitive subconcepts
of all concepts in CONCEPT-NAMES."
  ( labels (( leaves-of-one ( c &aux ( sc ( subconcepts c )))
	      ( cond (( null sc )
                      ( list c ))
		     ( T
		      ( delete-duplicates ( mapcap #'leaves-of-one sc ))))))
	   ( delete-duplicates ( mapcap #'leaves-of-one ( if ( listp conc-names )
							     conc-names
							     ( list conc-names ))))))


( defun includes-p ( conc-1 conc-2 &aux ( sub-concepts ( subconcepts conc-1 )))
"Returns T, iff concept-2 is a subconcepts of concept-1, NIL otherwise."
  ( cond (( endp sub-concepts )
	  NIL )
	 (( eq conc-1 conc-2 )
	  T )
	 (( member conc-2 sub-concepts )
	  T )
	 (( some #'( lambda ( x )
		     ( includes-p x conc-2 ))
		 sub-concepts )
	  T )))


( defun roots ( concept-list )
"Returns a list of the most general concepts, consisting of the concepts
given in CONCEPT-LIST."
  ( let ( success-p
	 ( result ( delete-duplicates ( copy-list concept-list ))))
	( flet (( expand ( x y )
		  ( when ( and y
			       ( not ( member x result ))
			       ( subsetp y result ))
		    ( setf success-p T )
		    ( push x result ))))
	       ( loop ( setf success-p NIL )
		      ( maphash #'expand *subconcepts* )
		      ( unless success-p ( return )))
	       ( loop for y in ( copy-list result )
		      do ( loop for x in ( subconcepts y )
				do ( delete x result )))
		      
	 result )))


( defun hset-union ( &rest sets )
  ( roots ( funcall #'append ( substitute '( TOP ) 'TOP ( remove-if #'null sets )))))


( defun hset-intersection-2 ( s1 s2 )
  ( roots ( intersection ( mapcap #'leaves s1 )
			 ( mapcap #'leaves s2 ))))


( defun hset-intersection ( &rest sets )
  ( case ( length sets )
	 ( 0 '() )
	 ( 1 sets )
	 ( 2 ( hset-intersection-2 ( first sets ) ( rest sets )))
	 ( T ( hset-intersection ( hset-intersection-2 ( first sets )
						       ( second sets ))
				 ( rest ( rest sets ))))))


( defun hset-difference ( s1 s2 )
  ( roots ( set-difference ( leaves s1 ) ( leaves s2 ))))



( defun expand-tuples ( tuple-list )
"Args: ( tuple-list )"
 ( remove-duplicates ( mapcap #'( lambda ( x )
				  ( compute-combinations ( mapcar #'leaves x )))
		              tuple-list )
                     :test #'equal ))

;;; OUTPUT

;( defun print-hierarchy ()
;"Args: none
;Creates a POSTSCRIPT file named HIERARCHY.PS which contains a
;presentation of the CONTAX hierarchy."
;  ( if *psgraph-loaded-p*
;       ( flet (( child ( x )
;		 ( if  ( eq ( subconcepts x ) 'TOP )
;		       '( TOP )
;		       ( subconcepts x )))
;	       ( info ( x )
;		 ( list ( princ-to-string x ))))
;	      ( with-open-file ( *standard-output* "hierarchy.ps" :direction :output )
;			       ( psgraph:psgraph t #'child #'info t )))
;       ( error "Sorry. You cannot create a postscript file, because PSGRAPH could not be loaded" )))


;;; INITIALIZATION

; #-:contax( reset-hierarchies )

