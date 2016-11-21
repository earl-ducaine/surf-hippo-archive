; This file must be loaded by 'start.lsp' because
; *our-fs-path* and *my-tree-path* are defined there.

(setf *load-verbose* t)
(princ (format nil "loading start-colab~%"))

(load *our-fs-path*)
(use-package 'our-fs)
(setf our-fs:*compile-in-use* nil) ; enable (our-fs::compile-all)


(let (#+genera (tv:more-processing-global-enable nil)	 ; suppress **MORE**
      #+genera (sys:inhibit-fdefine-warnings :just-warn)); redefine but
						         ; show warnings
;build a key for *my-tree-path*
(our-fs:logdir-define :Colab *my-tree-path*)

; ':Colab' is the top Directory
; ':top' is a new Key
;define main directory keywords for subsystems and system
(our-fs:logdir-define :top (our-fs:logdir-extend1 :Colab "top"))
(our-fs:logdir-define :Rfm (our-fs:logdir-extend1 :Colab "RFM"))
(our-fs:logdir-define :Forward (our-fs:logdir-extend1 :Colab "forward"))
(our-fs:logdir-define :Taxon (our-fs:logdir-extend1 :Colab "taxon"))
(our-fs:logdir-define :Contax (our-fs:logdir-extend1 :Colab "contax"))
;define directory keywords for docu, demo and help directories
(our-fs:logdir-define :col-docu (our-fs:logdir-extend1 :top "docu"))
(our-fs:logdir-define :col-demo (our-fs:logdir-extend1 :top "demo"))
(our-fs:logdir-define :col-help (our-fs:logdir-extend1 :col-docu "help"))

(defvar *colab-directory*	nil "Default pathname for merging")
(defvar *colab-colab-startup*	(our-fs:logdir-file :top "colab" :force nil) "Filename of colab startup file")
(defvar *colab-relfun-startup*  (our-fs:logdir-file :rfm  "start-RFM"
:force nil) "Filename of relfun startup file")
(defvar *colab-forward-startup* (our-fs:logdir-file :Forward "colab-fw-startup" :force nil) "Filename of forward startup file")
(defvar *colab-contax-startup*	(our-fs:logdir-file :contax "start-contax"
:force nil) "Filename of contax startup file")
(defvar *colab-taxon-startup*	(our-fs:logdir-file :taxon "col-rest-tx"
:force nil) "Filename of taxon startup file")



(our-fs:use-file (our-fs:logdir-file :top "col-keys" :force nil))
(use-package 'colab-keys)
(our-fs:use-file (our-fs:logdir-file :taxon "col-init-tx" :force nil))


(setq our-fs:*compile-in-use* t)
(setq our-fs::compile-list nil)
(setq our-fs::compile-relfun-list our-fs::compile-list)
(setq our-fs::compile-list nil)
(setq our-fs:*compile-in-use* nil)


(our-fs:use-file *colab-colab-startup*)
; (load *colab-colab-startup*)


#-:our-defsystem (our-fs:use-file (our-fs:logdir-file :top "defsystem" :force nil))

(our-fs:use-file (our-fs:logdir-file :Rfm "RFM-system" :force nil))
(our-fs:use-file (our-fs:logdir-file :taxon "taxon-system" :force nil))
(our-fs:use-file (our-fs:logdir-file :forward "forward-system" :force nil))
#-:kcl (our-fs:use-file (our-fs:logdir-file :contax "contax-system" :force nil))

(load-rfm)

(defun compile-colab ()
  ;Rfm is already loaded
  #-:kcl (load-contax)
  (load-taxon)
  (load-forward)

  (compile-rfm)
  (compile-taxon)
  #-:kcl (compile-contax)
  (compile-forward)
  )


#+Genera (progn

	(setq *load-set-default-pathname* nil)
	(setq *compile-file-set-default-pathname* nil)
	(dbg:com-set-stack-size :control 500000)

	(defmacro demo (&body body)
	  `(with-character-style ('(:fix :bold :very-large))
				 ,@body))

	(defvar *demo* nil)

	( cp:define-command
	  ( com-colab :name "Colab"
		      :command-table "global"
		      :provide-output-destination-keyword NIL
		      :values NIL )
	  ()
	  ( colab-start ))

	( cp:define-command
	  ( com-demo1 :name "Demo"
		      :command-table "global"
		      :provide-output-destination-keyword NIL
		      :values NIL )
	  ()
	  (setq *demo* T ))

	( cp:define-command
	  ( com-demo0 :name "NoDemo"
		      :command-table "global"
		      :provide-output-destination-keyword NIL
		      :values NIL )
	  ()
	  (setq *demo* nil))

	( cp:define-command
	  ( com-back  :name "Back"
		      :command-table "global"
		      :provide-output-destination-keyword NIL
		      :values NIL )
	  ()
	  ( colab-start ))


	(defun colab-start ()
	  (if *demo* (demo (colab)) (colab)))
	)


(our-fs:use-file (our-fs:logdir-file :rfm "colab-interface" :force nil)) ; for New-RFM
(our-fs:use-file (our-fs:logdir-file :top "demo-break" :force nil)) ; for (New-)RFM

) ; let (more etc.)
