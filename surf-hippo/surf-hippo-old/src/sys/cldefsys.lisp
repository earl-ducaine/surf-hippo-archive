;;; ;;; -*- Mode: lisp; package: user; base: 10;  Syntax: Common-lisp; -*-

;; Yes, it seems that we need to be this explicit with regard to package dependencies.
(make-package 'surf :use '("COMMON-LISP-USER" "COMMON-LISP"))

(pushnew :neuron *features*)		; Include neuron model
(pushnew :off-diag *features*)		; Use tri-diagonal matrices.
(pushnew :sun *features*)		; You are on a sun

;; renamed filename to clmisc.lisp 4/92 lbg
(load "clmisc.lisp")

(defun setsurf () 
  (in-package 'SURF))

(setsurf)

#-cmu (user::compile-and-load '("loop"))
(compile-system-and-load
 '("macros"
   "declare"
   "subs"
   ;; just for parallel -> "devices"
   "init"
   "sim"
   "node"
   ;; step.lisp needs to know about structure type 'node, so put it here
   "step"
   "tri-ge"				;don't need this for Hines method
   "soma"
   "segment"
   "res"
   "cap"
   "vsource"
   "isource"
   "currents"
;;   "new-currents"
   "conc-int"
   "channel"
   "conc-part"
   "particle"
   "synapse"
   "misc"
   "cell"
   "fix"
   "read"
   "print"
   "main"
   "plot"
   "output"
   #+ackl "input"
   #+garnet "garnet-tools"
   #+garnet "cell-graphics"
   )
 #+akcl :suffix #+akcl "-akcl"
 )


;;; Go ahead and load some examples
(compile-system-and-load
 '("/rabbit/star-amacrine"
   "/rabbit/star-amacrine-functions"
   "/hippocampus/hippos"
   "/sun/sys/read-me")
 :prefix *Surfdir*)


