;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-


;;; RFM-system.lisp
;;;;;;;;;;;;;;;;;;;


(our-fs:logdir-define :Aux 
  (our-fs:logdir-extend1 :RFM "aux"))
(our-fs:logdir-define :classifier 
  (our-fs:logdir-extend1 :RFM "classifier"))
(our-fs:logdir-define :codegenerator 
  (our-fs:logdir-extend1 :RFM "codegenerator"))
(our-fs:logdir-define :IDX 
  (our-fs:logdir-extend1 :RFM "index"))
(our-fs:logdir-define :emulator 
  (our-fs:logdir-extend1 :RFM "gama"))
(our-fs:logdir-define :mode-interpreter 
  (our-fs:logdir-extend1 :RFM "mode-interpreter"))
(our-fs:logdir-define :normalizer 
  (our-fs:logdir-extend1 :RFM "normalizer"))
(our-fs:logdir-define :RFI
  (our-fs:logdir-extend1 :RFM "relfun"))
(our-fs:logdir-define :RF-Help
  (our-fs:logdir-extend :RFM "docu/help"))


(pdefsys:defsystem RELFUN
  (:default-pathname (our-fs:logdir-dir :RFM)
   :default-binary-pathname (our-fs:logdir-dir :RFM)
   :needed-systems (RELFUN-aux RELFUN-rfi RELFUN-codegenerator
			       RELFUN-normalizer RELFUN-mode-interpreter
			       RELFUN-emulator RELFUN-classifier
			       RELFUN-index)
   :load-before-compile (RELFUN-aux RELFUN-rfi RELFUN-codegenerator
			       RELFUN-normalizer RELFUN-mode-interpreter
			       RELFUN-emulator RELFUN-classifier
			       RELFUN-index)
   :default-package USER)
  ;("relfun-patches")
  )

(pdefsys:defsystem RELFUN-aux
  (:default-pathname (our-fs:logdir-dir :Aux)
   :default-binary-pathname (our-fs:logdir-dir :Aux)
   :default-package USER)
  ("aux"))

(pdefsys:defsystem RELFUN-rfi
  (:default-pathname (our-fs:logdir-dir :RFI)
   :default-binary-pathname (our-fs:logdir-dir :RFI)
   :default-package USER)
  ("rfi")
  ("lisp2pro")
  ("pro2lisp")
  ("comment")
  ("patches"))

(pdefsys:defsystem RELFUN-codegenerator
  (:default-pathname (our-fs:logdir-dir :codegenerator)
   :default-binary-pathname (our-fs:logdir-dir :codegenerator)
   :default-package USER)
  ("absynt")
  ("assocf")
  ("instr")
  ("cg5")
  ("cgis")
  ("misc"))

(pdefsys:defsystem RELFUN-normalizer
  (:default-pathname (our-fs:logdir-dir :normalizer)
   :default-binary-pathname (our-fs:logdir-dir :normalizer)
   :default-package USER)
  ("normalizer")
  ("debug"))

(pdefsys:defsystem RELFUN-mode-interpreter
  (:default-pathname (our-fs:logdir-dir :mode-interpreter)
   :default-binary-pathname (our-fs:logdir-dir :mode-interpreter)
   :default-package USER)
  ("mode-interpreter")
  ("mode-rfi-interface"))

(pdefsys:defsystem RELFUN-emulator
  (:default-pathname (our-fs:logdir-dir :emulator)
   :default-binary-pathname (our-fs:logdir-dir :emulator)
   :default-package USER)
  ("gaux")
  ("gcla")
  ("gmem")
  ("gmht")
  ("gasm")
  ("ginit")
  ("gwam" :load-before-compile ("gwam")))

(pdefsys:defsystem RELFUN-classifier
  (:default-pathname (our-fs:logdir-dir :classifier)
   :default-binary-pathname (our-fs:logdir-dir :classifier)
   :default-package USER)
  ("absyn-macro")
  ("absyn1")
  ("absyn2")
  ("simplify")
  ("clasPas1")
  ("clasPas2")
  ("classify"))

(pdefsys:defsystem RELFUN-index
  (:default-pathname (our-fs:logdir-dir :IDX)
   :default-binary-pathname (our-fs:logdir-dir :IDX)
   :default-package USER)
  ("idx")
  ("icl")
  ("icg")
  ("iif")
  ("linear"))



(defun load-rfm ()
  (let (#+:genera (tv:more-processing-global-enable nil)     ; suppress **MORE**
        #+:genera (sys:inhibit-fdefine-warnings :just-warn)) ; redefine but
                                                             ; show warnings
       (pdefsys:load-system 'RELFUN :source-if-newer t)
       (defun normalize-database (db) db) ; remove normalizer !
       )
  )

(defun compile-rfm ()
  (let (#+:genera (tv:more-processing-global-enable nil)     ; suppress **MORE**
        #+:genera (sys:inhibit-fdefine-warnings :just-warn)) ; redefine but
                                                             ; show warnings
       (pdefsys:compile-system 'RELFUN :propagate t :reload t :recompile t)
       )
  )


