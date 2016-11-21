
(use-package 'our-fs)

(setq *compile-in-use* t)


; Start-up file for the FORWARD-System in C the COLAB-shell

;;;(logdir-define :Forward (logdir-extend1 :colab "forward"))   ;; <-- :Forward is defined in start-colab

;;; used to generate the prototype in /home/forward/colab/forward
;(logdir-define :Forward (pathname "/home/forward/colab/forward/xxx")) 

#|
;needed for new our-fs.lsp
(logdir-undef :fw-rf)
(logdir-undef :fw-demo)
(logdir-undef :fw-docu)
(logdir-undef :fw-help)
(logdir-undef :features)
(logdir-undef :fw-semi)
(logdir-undef :fw-strategies)
(logdir-undef :fw-rfi)
(logdir-undef :fw-rfe)
(logdir-undef :meta)
(logdir-undef :tx-inter)
(logdir-undef :magic)
(logdir-undef :semi)
(logdir-undef :fam)
|#


;the subdirectories of the system
;assign the extended pathnames to new keywords
(logdir-define :fw-rf (logdir-extend1 :Forward "fw-relfun"))
(logdir-define :fw-demo (logdir-extend1 :Forward "demo"))
(logdir-define :fw-docu (logdir-extend1 :Forward "docu"))
(logdir-define :fw-help (logdir-extend1 :fw-docu "help"))
(logdir-define :features (logdir-extend1 :Forward "features"))
(logdir-define :fw-semi (logdir-extend1 :Forward "fw-bottomup"))
(logdir-define :fw-strategies (logdir-extend1 :fw-rf "strategies"))
(logdir-define :fw-rfi (logdir-extend1 :fw-rf "fw-rfi"))
(logdir-define :fw-rfe (logdir-extend1 :fw-rf "fw-rfe"))
(logdir-define :meta (logdir-extend1 :fw-rf "meta"))
(logdir-define :tx-inter (logdir-extend1 :fw-rf "taxon-interface"))
(logdir-define :magic (logdir-extend1 :fw-semi "magic"))
(logdir-define :semi (logdir-extend1 :fw-semi "semi-naive"))

(logdir-define :fam (logdir-extend1 :Forward "fam"))



#+Genera (progn
	   (dbg:com-set-stack-size :control 500000))

(defun load-forward ()
  (let (#+:genera(tv:more-processing-global-enable nil)     ; suppress **MORE**
        #+:genera(sys:inhibit-fdefine-warnings :just-warn)) ; redefine but
                                                 ; show warnings
    (pdefsys:load-system 'forward :source-if-newer t))  
  (pushnew :forward *features*)
)


(defun compile-forward ()
  (let (#+:genera(tv:more-processing-global-enable nil)     ; suppress **MORE**
        #+:genera(sys:inhibit-fdefine-warnings :just-warn)) ; redefine but
                                                 ; show warnings
  (pdefsys:compile-system 'forward :propagate t :reload t)))




(pdefsys:defsystem forward
		   (:default-pathname (our-fs::logdir-dir :Forward)
		    :default-binary-pathname (our-fs::logdir-dir :Forward)
		    :needed-systems
		    (fw-basic fw-semi fw-meta fw-magic fw-inter fw-emul fw-fam fw-tx)
		    :load-before-compile
		    (fw-basic fw-semi fw-meta fw-magic fw-inter fw-emul fw-fam fw-tx)
		    :default-package USER
		    )
 
    )


(pdefsys:defsystem fw-basic
		   (:default-pathname (our-fs::logdir-dir :Forward)
		    :default-binary-pathname (our-fs::logdir-dir :Forward)
		    :needed-systems
		    ()
		    :load-before-compile
		    ()
		    :default-package USER
		    )
    ("fw-colab-top")
    ("fw-access")
 )


(pdefsys:defsystem fw-semi
		   (:default-pathname (our-fs::logdir-dir :semi)
		    :default-binary-pathname (our-fs::logdir-dir :semi)
		    :needed-systems
		    (fw-basic)
		    :load-before-compile
		    (fw-basic)
		    :default-package USER
		    )
   ("semi-naive")
 )


(pdefsys:defsystem fw-magic
		   (:default-pathname (our-fs::logdir-dir :magic)
		    :default-binary-pathname (our-fs::logdir-dir :magic)
		    :needed-systems
		    (fw-basic)
		    :load-before-compile
		    (fw-basic)
		    :default-package USER
		    )
   ("magic-sets")
 )


(pdefsys:defsystem fw-meta
		   (:default-pathname (our-fs::logdir-dir :meta)
		    :default-binary-pathname (our-fs::logdir-dir :meta)
		    :needed-systems
		    (fw-basic)
		    :load-before-compile
		    (fw-basic)
		    :default-package USER
		    )
   ("part-eval")
 )


(pdefsys:defsystem fw-tx
		   (:default-pathname (our-fs::logdir-dir :tx-inter)
		    :default-binary-pathname (our-fs::logdir-dir :tx-inter)
		    :needed-systems
		    (fw-basic)
		    :load-before-compile
		    (fw-basic)
		    :default-package USER
		    )
  ("get-wp")
  ("instance-name")
  ("tx-transformer")
)



(pdefsys:defsystem fw-inter
		   (:default-pathname (our-fs::logdir-dir :fw-rfi)
		    :default-binary-pathname (our-fs::logdir-dir :fw-rfi)
		    :needed-systems
		    (fw-basic)
		    :load-before-compile
		    (fw-basic)
		    :default-package USER
		    )
   ("fw-retain-admin")
   ("fw-horizon-comp")

 )



(pdefsys:defsystem fw-emul
		   (:default-pathname (our-fs::logdir-dir :fw-rfe)
		    :default-binary-pathname (our-fs::logdir-dir :fw-rfe)
		    :needed-systems
		    (fw-gasm fw-basic)
		    :load-before-compile
		    (fw-basic)
		    :default-package USER
		    )
    ("fw-gassem-startup")
 )


(pdefsys:defsystem fw-gasm
		   (:default-pathname (our-fs::logdir-dir :fw-rfe)
		    :default-binary-pathname (our-fs::logdir-dir :fw-rfe)
		    :needed-systems
		    (fw-basic)
		    :load-before-compile
		    (fw-basic)
		    :default-package USER
		    )
    ("changes")
    ("tools")
    ("retain")
    ("absyn2.patch")
    ("procedures")

 )


(pdefsys:defsystem fw-fam
		   (:default-pathname (our-fs::logdir-dir :fam)
		    :default-binary-pathname (our-fs::logdir-dir :fam)
		    :needed-systems
		    (fw-basic)
		    :load-before-compile
		    (fw-basic)
		    :default-package USER
		    )
  ("compiler")
  ("emu")
  ("interface")
 )


