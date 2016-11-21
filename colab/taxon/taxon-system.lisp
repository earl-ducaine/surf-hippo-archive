;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-


(defun load-taxon ()
  (let (#+:genera (tv:more-processing-global-enable nil)     ; suppress **MORE**
        #+:genera (sys:inhibit-fdefine-warnings :just-warn)) ; redefine but
                                                             ; show warnings
       (pdefsys:load-system 'TAXON :source-if-newer t)
       )
  (pushnew :taxon user::*features*)
  (taxon::destroy-taxon)
  (taxon::put-cnf-of-conceptname! 'bottom (taxon::make-cnf-of-bottom))
  (taxon::put-cnf-of-conceptname! 'top (taxon::make-cnf-of-top))
  t
  )

(defun compile-taxon ()
  (let (#+:genera (tv:more-processing-global-enable nil)     ; suppress **MORE**
        #+:genera (sys:inhibit-fdefine-warnings :just-warn)) ; redefine but
                                                             ; show warnings
       (pdefsys:compile-system 'TAXON :propagate t :reload t :recompile t)
       )
  )


#-(or :genera :kcl) (defpackage TAXON  (:nicknames TX))
#-(or :genera :kcl) (defpackage psgraph)

#+:kcl (in-package 'TAXON :nicknames '(TX))
#+:kcl (in-package 'psgraph)
(in-package 'user)

(pdefsys:defsystem TAXON
  (:default-pathname (our-fs:logdir-file :taxon "")
		     :default-binary-pathname (our-fs:logdir-extend1 :taxon "ibin")
		     :needed-systems (TAXON-front-end 
				      TAXON-print-fcts 
				      TAXON-representation 
				      TAXON-subsumption
				      TAXON-consistency
				      TAXON-incr-real-ord 
				      TAXON-concrete)
		     :default-package "TAXON"
		     )
  )

(pdefsys:defsystem TAXON-front-end
  (:default-pathname (our-fs:logdir-file :front-end "")
		     :default-binary-pathname (our-fs:logdir-extend1 :front-end "ibin")
		     :default-package "TAXON"
		     )
  ("geto")
  ("tx-signatur")
  ("graph")
  ("classify")
  
  ;;; These files are only for producing postscript output
  ("psgraph")
  ("draw-hierarchy")
  ("draw-on-boards")
  ("draw-objects-on-boards")
  ;
  
  ("abox-manager")
  ("tbox-manager")
  ("absynt")
  ("define")
  ("sprims")
  ("push-not")
  ("assertion")
  ("ro-predicates")
  ("predicates")
  ("exp-asse")
  ("subsumption")
  ("filter-graph")
  
  ("a-reasoning")
  ("cmds")
  ("listing")
  ("ind-reasoning")
  ("exp-pred")
  
  ("colab-access")
  ("retrieval")
  ("trace")  
  ("switch")
  ("tx")
  ("help-fu")
  
  ("save-load-graph")
  
  )

(pdefsys:defsystem TAXON-print-fcts
  (:default-pathname (our-fs:logdir-file :print-fcts "")
		     :default-binary-pathname (our-fs:logdir-extend1 :print-fcts "ibin")
		     :default-package "TAXON"
		     )
  ("print-fcts")
  
  )

(pdefsys:defsystem TAXON-representation
  (:default-pathname (our-fs:logdir-file :representation "")
		     :default-binary-pathname (our-fs:logdir-extend1 :representation "ibin")
		     :default-package "TAXON"
		     )
  ("cnf-subsumption")
  ("cnf-representation")
  ("generate-cn")
  ("print-cnf")
  ("tr2knf")
  ("constructors2fkts")
  ("get-function")
  ("object")
  ("abstract")
  ("print-abstracts")
  ("concrete")
  ("unknown")
  ("deref")
  )

(pdefsys:defsystem TAXON-subsumption
  (:default-pathname (our-fs:logdir-file :subsumption "")
		     :default-binary-pathname (our-fs:logdir-extend1 :subsumption "ibin")
		     :default-package "TAXON"
		     )
  ("make-subs-rules")
  ("subsumes-weakly")
  ("subs-rules")
  )


(pdefsys:defsystem TAXON-consistency
  (:default-pathname (our-fs:logdir-file :consistency "")
		     :default-binary-pathname (our-fs:logdir-extend1 :consistency "ibin")
		     :default-package "TAXON"
		     )
  ;;; contains macros
  ("print-rule")
  ;;; these files contain proclaim
  ("stacks")
  ("predicates")
  ("choice-points")
  ;;; this file contains a macro which is used in control
  ("clash")
  ;;; rest
  ("abstract-predicates")
  ("agreements")
  ("backtracking")
  ("concrete-predicates")
  ("cond-fcts")
  ("control")
  ("init")
  ("make-assertions")
  ("make-rules")
  ("pathrules-for-concs")
  ("pathrules-for-preds")
  
  ("process-stacks")
  ("restrictor-rules")
  ("rules")
  ("types")
  )

(pdefsys:defsystem TAXON-incr-real-ord
  (:default-pathname (our-fs:logdir-file :incr-real-ord "")
		     :default-binary-pathname (our-fs:logdir-extend1 :incr-real-ord "ibin")
		     :default-package "TAXON"
		     )
  ("make-predicates")
  ("print-RO")
  ("propagate-number-rest")
  ("propagate-number")
  ("propagate-real")
  ("propagate-symbolic")
  ("representation")
  ("symbolic-clash")
  ("propagate-new")
  ("rules")
  )

(pdefsys:defsystem TAXON-concrete
  (:default-pathname (our-fs:logdir-file :concrete "")
		     :default-binary-pathname (our-fs:logdir-extend1 :concrete "ibin")
		     :default-package "TAXON"
		     )
  ("get-rules")
  )

#|
(progn
  ;(pdefsys:compile-system 'TAXON-front-end :recompile t :reload t)
  (pdefsys:load-system 'TAXON-front-end))

(progn
  ;(pdefsys:compile-system 'TAXON-print-fcts :recompile t :reload t)
  (pdefsys:load-system 'TAXON-print-fcts))

(progn
  ;(pdefsys:compile-system 'TAXON-representation :recompile t :reload t)
  (pdefsys:load-system 'TAXON-representation))

(progn
  ;(pdefsys:compile-system 'TAXON-subsumption :recompile t :reload t)
  (pdefsys:load-system 'TAXON-subsumption))

(progn
  ;(pdefsys:compile-system 'TAXON-consistency :recompile t :reload t)
  (pdefsys:load-system 'TAXON-consistency))

(progn
  ;(pdefsys:compile-system 'TAXON-incr-real-ord :reload t)
  (pdefsys:load-system 'TAXON-incr-real-ord))

(progn
  ;(pdefsys:compile-system 'TAXON-concrete :recompile t :reload t)
  (pdefsys:load-system 'TAXON-concrete))


(progn
  ;(pdefsys:compile-system 'TAXON)
  (pdefsys:load-system 'TAXON))

|#

