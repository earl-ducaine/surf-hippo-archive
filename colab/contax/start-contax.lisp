;;; 08-04-92 fs
;;; 01-09-92 mk : adapted to Lucid-Lisp 
;;; 28-09-92 fs : parameterized local pathname 
;;; 11-11-92 fs,mk,mm : using OUR-FS
;;; 25-11-92 mk : extension of "our-fs typelist"  

( setf *load-verbose* t )

; set the local path for CONTAX
( setf *my-contax-path* "/home/ctec/rcs_colab/contax/" )

; set path for LISP
( setf *my-lisp-path* "/usr/local/test/lisp/" )

; set the path for the file system module
( setq *my-our-fs-path* 
       (string-append *my-contax-path* "our-fs.lisp"))

; CTEC dir: "/home/ctec/rcs_colab/contax/"
; Frank's dir: "/home/steinle/lisp/Colab/contax/"
; Michael's dir: "/home/kreinbih/colab/contax/"
; Manfred's dir: "/home/meyer/colab/contax/"

; load OUR-FS, if not already done
( require "OUR-FS" *my-our-fs-path* )


; ( use-package 'our-fs ) ; don't use it - only qualified

; define pathnames within OUR-FS
( our-fs:logdir-define :CONTAX *my-contax-path* )
( our-fs:logdir-define :LISP *my-lisp-path* )  

;;; (load "/home/ctec/rcs_colab/top/col-keys.lisp")
;;; (use-package 'colab-keys)


;;; special LUCID setup
#+:lucid ( progn

          ( our-fs:logdir-push-typelist-front '("sbin") )

	  ;;; load DEFSYSTEM, if necessary
	  ( unless ( member :DEFSYSTEM *features* )
	    ( our-fs:use-file ( our-fs:logdir-file :CONTAX "defsystem" )))

	  ;;; load CLOS, if necessary
	  ( unless ( member :CLOS *features* )
	    ( our-fs:use-file ( our-fs:logdir-file :LISP "clos" )))

          ( our-fs:logdir-pop-typelist ))


;;; load CONTAX package & system definition, if previously unknown

( unless ( and ( boundp '*contax-system-defined* )
	       *contax-system-defined* )
	 ( format t "~&Loading CONTAX system definition..." )
         ( our-fs:use-file ( our-fs:logdir-file :contax "contax-system" ))
	 ( format t "~&CONTAX system definition loaded..." )
)


;;; load CONTAX system

( load-contax )


#+:lucid
( pdefsystem:show-system 'CONTAX )

( contax:reset )


