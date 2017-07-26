;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10; -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 6/09/85 15:18:14
;
; Reads in a circuit description
;

#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


;; 6/17/92 lbg
;; added this since functionp in cmucl 16d doesn't seem to work right
;;
#+cmu
(defvar *assume-function-input* t)

(defun readin (circuit)
  "Reads in a description of a circuit."
  (if (not circuit)
      (progn
	(format t "What circuit? ")
	(setq circuit (read))
	))
  (setq *circuit-name* circuit)
  (cond
#+cmu    (*assume-function-input* (funcall circuit))
    ((functionp circuit)     (funcall circuit))
    ((stringp circuit) (load (pathname (string circuit))))
    (t (sim-error "~a is not a function or a string, I don't know what to do with it."
		  circuit)))
  )

