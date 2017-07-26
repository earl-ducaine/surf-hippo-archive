;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10; -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


;;; SURF - This is the main program which calls both "SIM" and the interface routines.
(defun surf (&optional circuit automatic)
  (if (not (or *circuit-name* circuit)) (sim-error "There is no circuit to simulate!!"))
  (if automatic (setq *automatic-run t))
  #-sun  (if (not *automatic-run) (main-menu))	;Main-menu calls most of the parameter menus.
  (without-floating-underflow-traps
    (sim circuit))
  #-sun (progn (beep)(beep)))


;;; SURF-STARTUP Setup up the simulation frame with 6 plot panes for the relevant output lists and one lisp listener
;;; pane for input and parameter printing. The flavor "plot:plot-hack" comes from PAO's plotting software, which
;;; is assumed to be loaded. (Requires Symbolics environment)
#-sun
(progn
  (send *surf-interaction-pane :set-default-character-style '(:fix :roman :small))
  (send *surf-interaction-pane :select)
  (zl:pkg-goto #-PARALLEL 'SURF #+PARALLEL '*SURF t)
  (format *surf-interaction-pane "Ok, start simulating.....~%~%"))

