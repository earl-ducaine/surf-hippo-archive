;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX; -*-


;;; module: VARIABLES

;;; This module declares the variable class hierarchy (using CLOS), various
;;; special variables (namely *VARIABLE-TABLE*, *VARIABLES*) as well as a
;;; few standard and generic functions dealing with variables.


;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES:
;;;   13-05-92 fs added VAR-VALS and VAR-CONS methods for variable names (symbols)
;;;   28-09-92 fs removed method and functions deaking with interval valued variables


( in-package "CONTAX" )

( export '( find-variable
	    variable-name-p
	    variable-p
	    hierarchical-variable-p
	    interval-variable-p
	    local-variable-p
	    assign-fn
	    values-fn
	    make-variable-fn
	    push-var-states
	    restore-var-states
	    pop-var-states ))


;;;;; variables CLASS-DEFINITIONS


( defclass variable ()
  (( name :reader  vname
          :initarg :vname
          :initform 'unnamed-variable )
   ( watched :accessor vwatched-p
	     :initform NIL )
   ( domains :accessor var-doms
             :initarg  :domains )
   ( values :accessor var-vals
            :initarg  :values )
   ( value-stack :accessor var-stack
                 :initform '()
		 :type list )
   ( constraints :accessor var-cons
		 :initform nil ))
  ( :documentation "Top of the variable type hierarchy." ))


( defclass hierarchical-variable ( variable )
  (( expanded-p :accessor var-expanded-p
		:initform NIL ))
  ( :documentation "Variable subclass ranging over hierarchical structured domains." ))


;( defclass interval-variable ( variable )
;  (( expanded-p :accessor var-expanded-p
;		:initform NIL ))
;  ( :documentation "Variable subclass ranging over hierarchical structured domains." ))


; 15-09-92 fs
( defclass local-mixin ()
  ()
  ( :documentation
    "This mixin makes a variable local, leading to some exceptional treatment." ))

( defclass local-hierarchical-variable ( local-mixin hierarchical-variable )
  () )


;;; ADDITIONAL ACCESSOR METHODS

( defmethod var-vals (( var-name symbol ))
  ( var-vals ( find-variable var-name )))


( defmethod var-cons (( var-name symbol ))
  ( var-cons ( find-variable var-name )))


;;; VARIABLES

( defvar *variable-table* ( make-hash-table )
  "Hashtable holding all variable names and the associated objects." )


( defvar *variables* '()
  "List of all active variables." )


;;; FUNCTIONS


( defun find-variable ( symbol &optional ( default NIL ))
  ( gethash symbol *variable-table* default ))


( defun variable-name-p ( symbol )
  ( second ( multiple-value-list ( find-variable symbol ))))


( defun connect-var-to-cons ( var cons )
  ( pushnew cons ( var-cons var ))
  var )


( defun variable-p ( x )
  ( typep x 'variable ))


( defun hierarchical-variable-p ( x )
  ( typep x 'hierarchical-variable ))


( defun interval-variable-p ( x )
  ( declare ( ignore x ))
  'NIL )
;  ( typep x 'interval-variable ))


; 15-09-92 fs
( defun local-variable-p ( x )
  ( typep x 'local-hierarchical-variable ))


;;; VARIABLE CREATION and ASSIGNMENT and VALUE RETRIEVAL

( defun make-variable-fn ( name &key domains
			             ( values  :unspecific )
				     ( type    :hierarchical ))
"Args: ( name &key domains ( values :unspecific ) ( type :hierarchical ))
Creates a variable of a given NAME and TYPE and initially assigns
VALUES, if supplied, DOMAINS (which has to be supplied) otherwise."
  ( when ( equal  type '(hrcl) )
	 ( format T "~%WARNING: Please do not use variable type VAR any more.~
		     ~%         (Try using :HIERARCHICAL)." )
	 ( setq type :hierarchical ))
  ( let (( real-type ( case type
                            ( :hierarchical 'hierarchical-variable )
			    ( :hrcl         'hierarchical-variable )
			    ; ( :intervl      'interval-variable )
			    ; ( :interval     'interval-variable )
			    ( T ( error "This type of variable is not recognized: ~a " type )))))
	( first ( push ( setf ( gethash name *variable-table* )
			      ( make-instance real-type
					      :vname   name
					      :domains domains
					      :values  values ))
		       *variables* ))))

; 15-09-92 fs
( defun make-local-variable ( domains
			      &optional ( values :unspecific )
			      &aux      ( name ( local-variable-name )))
  ( first ( push ( setf ( gethash name *variable-table* )
			( make-instance 'local-hierarchical-variable
					:vname   name 
					:domains domains
					:values  values ))
		 *variables* )))


( defun assign-fn ( var new-val )
"Assigns to variable VAR the given VALUE."
  ( declare ( special *automatic-mode*
		      *interactive-mode* ))
  ( setf ( var-vals var ) ( if ( find-if #'lisp-constraint-p ( var-cons var ))
			       ( progn ( setf ( var-expanded-p var ) T )
				       ( leaves new-val ))
			       new-val ))
  ( when ( and *interactive-mode*
	       ( not ( eq ( getf *automatic-mode* :assignment ) :OFF )))
	 ( propagate-locally ( getf *automatic-mode* :assignment ))))


( defun values-fn ( var )
"Args: ( var )
Returns the value of the variable VAR."
  ( var-vals var ))


;;; METHODS on variables

( defmethod shared-initialize :after (( v variable ) slots-for-initform &rest initargs )
"Test for correct values of the DOMAIN slot."
  ( declare ( ignore slots-for-initform initargs ))
  ( loop
    ( unless ( null ( var-doms v ))
	     ( return ))
    ( cerror "Supply a list of domains."
	     "Trying to create variable ~a without domains."
	     ( vname v ))
    ( format *error-output* "Enter a list of domains: " )
    ( setf ( slot-value v 'domains ) ( read )))
  ( when ( eq ( var-vals v ) :unspecific )
	 ( setf ( var-vals v ) ( var-doms v ))))


( defmethod ( setf var-vals ) :around ( new-val ( v variable ))
  "Do tracing."
  ( when ( and *anything-watched-p*
	       ( member :on-assignment ( vwatched-p v )))
	 ( incf *watch-indentation* 4 )
	 ( funcall ( getf ( vwatched-p v ) :on-assignment ) v new-val ))
  ( call-next-method )
  ( when ( and *anything-watched-p*
	       ( member :on-assignment ( vwatched-p v )))
	 ( decf *watch-indentation* 4 )))

( defvar *throw-on-empty-set-assignment* T )

( defmethod ( setf var-vals ) :around ( new-vals ( v hierarchical-variable ))
"Prevents activation of constraints, if variable covering has not been
reduced."
;  ( format t "~%setf with ~A to ~a." new-vals v )
  ( when ( and *throw-on-empty-set-assignment*
	       ( null new-vals ))
;	 ( format t "~&THROWING to ~A: ~A" *inconsistent-tag* ( list :error v ))
;	 ( break ) 
         ( if ( eq *throw-on-empty-set-assignment* T )
	      ( throw *inconsistent-tag* ( list :cerror v ))
	      ( throw *inconsistent-tag* ( funcall *throw-on-empty-set-assignment* v new-vals ))))
  ( when ( eq new-vals :unspecific )
         ( setf new-vals ( var-doms v )))
  ( when ( eq ( var-vals v ) :unspecific )
	 ( setf ( slot-value v 'values ) ( var-doms v )))
  ( unless ( and ( subsetp ( var-vals v ) new-vals :test #'equal )
	         ( subsetp new-vals ( var-vals v ) :test #'equal))
           ( call-next-method ))
  ( values new-vals ))


;( defmethod ( setf var-vals ) :around ( new-vals ( v interval-variable ))
;"prevents activation of constraints, if variable covering has not been
;reduced."
;  ( when ( null new-vals )
;         ( throw *inconsistent-tag* ( list :error v )))
;  ( when ( eq new-vals :unspecific )
;         ( setf new-vals ( var-doms v )))
;  ( when ( eq ( var-vals v ) :unspecific )
;	 ( setf ( slot-value v 'values ) ( var-doms v )))
;  ( let (( new-vals ( write-intervalls new-vals )))
;	( unless ( and ( subsetp ( var-vals v ) new-vals :test #'equal )
;		       ( subsetp new-vals ( var-vals v ) :test #'equal))
;		 ( call-next-method ))
;	( values new-vals )))


( defmethod ( setf var-vals ) :before ( new-vals ( v variable ))
  "do spying."
  ( when *spy-assignment*
	 ( format *trace-output*
		  "~%assigning new value~p ~a to variable ~a (was ~a)."
		  ( and ( listp new-vals )
			( length new-vals ))
		  ( roots new-vals)
		  v
		  ( roots ( slot-value v 'values )))))


; neue werte in eine hierachische variable schreiben:
;  falls es unspecific ist, werden die allgemeinsten werte der hierachie
;                           genommen
;  falls es sonst die werte, wie sie sind

( defmethod ( setf var-vals ) ( new-vals ( v hierarchical-variable ))
  ( if ( eq new-vals :unspecific )
       ( setf ( slot-value v 'values ) ( var-doms v ))
       ( setf ( slot-value v 'values ) new-vals )))


; neue werte in eine interval variable schreiben:
;  falls es unspecific ist, werden die allgemeinsten werte der hierachie
;                           genommen
;  falls es sonst die werte, nachdem sie in intervall schreibweise
;                           umgeformt wurden, also (1 2 3) -> ((1 3))

;( defmethod ( setf var-vals ) ( new-vals ( v interval-variable ))
;  ( if ( eq new-vals :unspecific )
;       ( setf ( slot-value v 'values ) ( var-doms v ))
;       ( setf ( slot-value v 'values ) ( write-intervalls new-vals ))))


( defmethod ( setf var-vals ) :after ( new-vals ( v variable ))
  "Activates all constraints, connected to the variable."
  ( declare ( ignore new-vals ))
;  ( print 'setf-after)
  ( dolist ( c ( var-cons v ))
	   ( activate c )))


;( defmethod var-expanded-vals (( var interval-variable ))
;  ( write-intervalls ( var-vals var )))


( defmethod var-expanded-vals (( var hierarchical-variable ))
  ( leaves ( var-vals var )))


( defmethod push-state (( v variable ))
"Pushes variable covering onto local stack."
  ( push ( var-vals v ) ( var-stack v )))


( defmethod restore-state (( v variable ) &optional ( notify-constraints-p T ))
"Restores variable covering from local stack, which remains unchanged."
  ( if notify-constraints-p
       ( setf ( var-vals v ) ( first ( var-stack v )))
       ( setf ( slot-value v 'values ) ( first ( var-stack v )))))


( defmethod pop-state (( v variable ))
"Pops top of local stack. Variable values remain unchanged."
  ( pop ( var-stack v )))


( defun push-var-states ( &optional ( vars *variable-table* vars-supplied-p ))
"Args: ( &rest vars )
Saves the state of all variables in VARS in their internal stack. If VARS
is empty, all variables of the system will be saved."
  ( if vars-supplied-p
      ( mapc #'push-state vars )
      ( maphash #'( lambda ( x y )
		    ( declare ( ignore x ))
		    ( push-state y ))
		vars ))
  ( values "Variable coverings pushed." ))


( defun restore-var-states ( &optional ( vars *variable-table* vars-supplied-p )
			     &key      ( notify-constraints-p T ))
"Args: ( &rest vars )
Restores the last pushed state of all variables in VARS. If VARS is empty,
all variables of the system will have their values restored."
  ( if vars-supplied-p
      ( mapc #'restore-state vars ( make-list ( length vars )
					      :initial-element notify-constraints-p ))
      ( maphash #'( lambda ( x y )
		    ( declare ( ignore x ))
		    ( restore-state y notify-constraints-p ))
		vars ))
  ( values "Variable coverings restored." ))


( defun pop-var-states ( &optional ( vars *variable-table* vars-supplied-p ))
"Args: ( &rest vars )
Pops the state of all variables in VARS from their internal stack. If VARS
is empty, all variables of the system will be poped."
  ( if vars-supplied-p
      ( mapc #'pop-state vars )
      ( maphash #'( lambda ( x y )
		    ( declare ( ignore x ))
		    ( pop-state y ))
		vars ))
  ( values "Variable coverings poped." ))

