;;; -*- Base: 10; Syntax: Common-Lisp; Default-character-style: (:FIX :ROMAN :NORMAL); Package: USER -*-

; This file declares a package CONTAX and a system CONTAX.

;;; date created : 11-11-92 fs
;;; 16-11-92 fs : last fixes (i hope)


( defvar *contax-system-defined* T
"Indicates, if the package and system definitions of CONTAX have been loaded already." )


;;; CONTAX package definition
;;; - works with SYMBOLICS and LUCID
;;; - needs packages CLOS, COMMON-LISP (or LISP)
( defpackage CONTAX
  ( :nicknames "CN" )
  #+:symbolics ( :prefix-name "CONTAX" )
  #-:symbolics ( :shadowing-import-from "CLOS" )
  #+:symbolics ( :shadowing-import clos:defgeneric
				   clos:defmethod
				   clos:setf
				   clos:make-instance
				   clos:documentation
				   clos:define-method-combination )
  ( :export reset )
  #-:symbolics ( :use "LISP" "CLOS" #+:colab "COLAB-KEYS" )
  #+:symbolics ( :use "COMMON-LISP" "CLOS" #+:colab "COLAB-KEYS" )
)


;;; CONTAX system definition
;;; - different version vor LUCID and SYMBOLICS
;;; - both require loading of OUR-FS beforehand

;;; LUCID defsystem
#-:symbolics
( pdefsys:defsystem CONTAX
  ( :default-package "CONTAX"
    :default-pathname ( our-fs:logdir-file :CONTAX "" )
    :default-binary-pathname ( our-fs:logdir-extend1 :CONTAX "contax-sbin" )
  )
  ; MODULES
  (  "assistant-macros")
  (  "assistant-functions")
  (  "constraint-queues")
  (  "hierarchies"  ) 
  ; component removed 28-09-92 fs
  ;  (  "intervals" )
  ; component removed 26-08-92 fs
  ;  (  "convert-hierarchy.lisp" )
  (  "variables" )  
  (  "constraints" )
  (  "predicates") 
  (  "revise" )
  (  "control")
  (  "programmer-interface" )
  (  "negation" )
  (  "colab-interface")
  (  "user-interface" )
  (  "debugger" )
)

;;; SYMBOLICS defsystem
#+:symbolics
( defsystem CONTAX
  ( :pretty-name "CONTAX Constraint Solver"
    :default-package "CONTAX"
    :default-pathname #.( our-fs:logdir-file :CONTAX "" )
    :default-destination-pathname #.( our-fs:logdir-extend1 :CONTAX "contax-ibin" )
    :default-module-type :lisp
    :maintain-journals NIL
    :patchable NIL
  )
  ( :serial "assistant-macros.lisp"
	    "assistant-functions.lisp"
	    "constraint-queues.lisp"
	    "hierarchies.lisp" 
	    ; component removed 28-09-92 fs
	    ;  (  "intervals" )
	    ; component removed 26-08-92 fs
	    ; "convert-hierarchy.lisp"
	    ( :parallel "variables.lisp" "constraints.lisp" )
	    ( :serial "predicates.lisp" "revise.lisp" )
	    "control.lisp"
	    "programmer-interface.lisp"
	    "negation.lisp"
	    "colab-interface.lisp"
	    "user-interface.lisp"
	    "debugger.lisp" )
)


( defun load-contax ( &rest arguments )
  #-:symbolics ( pdefsys:load-system 'CONTAX :source-if-newer T )
  #+:symbolics ( sct:load-system "CONTAX" :query :no-confirm :silent NIL :no-warn T )
  ( when #-:symbolics ( member :taxon *features* )
         #+:symbolics ( member :taxon cl::*features* )
    ( our-fs:use-file (our-fs:logdir-file :CONTAX "convert-hierarchy")))

  ; tell LISP, that CONTAX is available
  ( provide "CONTAX" )

  ; and reset it
  ( contax:reset )

  ; add :CONTAX to feature list
  #-:symbolics ( pushnew :contax *features* )
  #+:symbolics ( pushnew :contax cl::*features* )
)


( defun compile-contax ( &rest arguments )
  #-:symbolics ( cond (( member :speed arguments )
		   ( pdefsystem:with-compiler-options ( :speed 3 :safety 0 )
		     ( pdefsys:compile-system 'CONTAX
					      :recompile ( member :all arguments ))))
		  (( member :safety arguments )
                   ( pdefsystem:with-compiler-options ( :speed 0 :safety 3 )
                     ( pdefsys:compile-system 'CONTAX
                                              :recompile ( member :all arguments
 ))))
		  ( T
		   ( pdefsys:compile-system 'CONTAX
					    :recompile ( member :all arguments ))))
  #+:symbolics ( sct:compile-system "CONTAX" :query :no-confirm
					     :silent NIL
					     :no-warn T
					     :recompile ( member :all arguments ))
)
