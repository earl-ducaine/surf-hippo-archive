; start-RFM.lisp

; This file must be loaded by 'init.lsp' because
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

(defvar *colab-directory*	nil "Default pathname for merging")
(defvar *colab-relfun-startup*  (our-fs:logdir-file :rfm  "start-RFM"
:force nil) "Filename of relfun startup file")


(setq our-fs:*compile-in-use* t)
(setq our-fs::compile-list nil)
(setq our-fs::compile-relfun-list our-fs::compile-list)
(setq our-fs::compile-list nil)
(setq our-fs:*compile-in-use* nil)

(our-fs:use-file (our-fs:logdir-file :top "defsystem" :force nil))

(our-fs:use-file (our-fs:logdir-file :Rfm "RFM-system" :force nil))

(load-rfm)

#+Genera (progn
	  (setq *load-set-default-pathname* nil)
	  (setq *compile-file-set-default-pathname* nil)
	  (dbg:com-set-stack-size :control 500000)))

