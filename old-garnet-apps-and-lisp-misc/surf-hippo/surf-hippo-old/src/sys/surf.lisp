;;; Master file for surf system under sun common lisp.

;;; Sigh.  Ok, you have to change this as well as the flag parallel below.
(in-package "SURF")
;(in-package "*SURF")

;;; Flags.

;(pushnew :parallel *features*)		; CM system
;(pushnew :mos1 *features*)		; Include Level 1 mosfet model
(pushnew :neuron *features*)		; Include neuron model
(pushnew :off-diag *features*)		; Use tri-diagonal matrices.
(pushnew :sun *features*)		; You are on a sun; don't use interface

;;; Provide declaration.

#-parallel (provide 'surf)
#+parallel (provide '*surf)

;;; Set package name

(setq *surf-package-name* #-parallel "SURF" #+parallel "*SURF")
(export '*surf-package-name*)

;;; Create a list of the files involved in the program.  This will 
;;; eventually (?) turn into a defsys, but for right now I want to be
;;; able to compile this thing easily.

(export '*surf-package-file-names*)

(setq *surf-package-file-names*
      '(
	"macros"			; Just what you'd think.
	"declare"			; Variables
	"subs"				; Subroutines
	"devices"			; Other common needed stuff
	"struct"
	"misc"				; From misc interface file on lispms.

					; Hippo:
	"variables"			; Variables.
	"currents"			; Membrane current params.

	"error"				; The System
	"init"
	"sim"
	"return"
#+mos1  "circuit"
        "node"
	"step"
	"res"
	"cap"
	"vsource"
	"isource"
#+mos1  "mos1"

#+neuron "memb"				; Neuron Model
#+neuron "conc-int"
#+neuron "conc-part"
#+neuron "channel"
#+neuron "particle"
#+neuron "synapse"
#+neuron "segment"
#+neuron "cell"

        "fix"				; Other stuff.
	"read"
	"print"

#+sun	"sun"				; Stuff that needs to be here 
					; only on suns.

#+(and off-diag (not parallel))  "tri-ge"
#+(and off-diag parallel)        "cr11.lisp"
))

(dolist (x *surf-package-file-names*) (load x))

;;; Load additional files that may not really be part of the distribution.
(load "../sample-input.lisp")
(load "../deal-with-sim.lisp")

;;; Allow easy reloading of sample-input.lisp (for changing parameters).
;;; Actually, I should have all of the parameters I want to change
;;; easily accesible in the form of variables and have each run of the
;;; simulator print out the values it is using.
(defun lf () (load "../sample-input.lisp"))
(export 'lf)

(defun *compile-all-surf-files* ()
  (dolist (x *surf-package-file-names*)
	  (compile-file x)))
(export '*compile-all-surf-files*)

(export 'sim)
(export 'resim)
