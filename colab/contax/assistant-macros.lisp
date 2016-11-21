;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX -*-




;;; module: ASSISTANT MACROS

;;; This files provides various general macros to be used by CONTAX, which
;;; are not especially included in one of the various other modules.




;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES: packages/function calls  changed for usage with Lucid-Lisp
;;;          01-09-92 mk 


( in-package "CONTAX" )


( export '( connect-pairwise ))


( defmacro mapcap ( function list )
"Syntax: function list
Applies function to all members of list and appends the results.
(This is the non-destructive counterpart to MAPCAN."
 `( apply #'append ( mapcar ,function ,list )))


#+:symbolics
( scl:defsubst string-append ( &rest strings )
"Args: &rest strings
Returns one string as a concatenation of all the argument strings."
  ( apply #'concatenate ( cons 'string strings )))


#+:lucid
( user::defsubst string-append ( &rest strings )
"Args: &rest strings
Returns one string as a concatenation of all the argument strings."
  ( apply #'concatenate ( cons 'string strings )))


( defmacro connect-pairwise ( constraint variable-list &optional ( priority :hard ))
"Installs CONSTRAINT between all pairs of variables out of VARIABLE-LIST."
  `( flet (( install-constraint ( var-1 var-2 )
	    ( make-constraint-fn ( gensym )
				 ',constraint
				 :x ( find-variable var-1 )
				 :y ( find-variable var-2 )
				 :priority ,priority )))
	  ( funcall-pairwise #'install-constraint ',variable-list )))
