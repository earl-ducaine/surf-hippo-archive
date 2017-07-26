;;; -*- Mode: lisp; package: user; base: 10;  Syntax: Common-lisp; -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; main-compiler.lisp
;;;
;; This loads the Garnet and Surf-Hippo systems, according to the Unix
;; environment variables GARNETHOME and SURFHOME.
;; LBG 9/30/92


;; This is loaded into a fresh CMUCL Ilisp environment, or one with GARNET added.
;; LBG 10/0/94

(defvar surf-hippo-compile t)

(defvar *SURF-HOME*)
(setq *SURF-HOME*
      (let ((surfhome (cdr (assoc :SURFHOME lisp::*environment-list*)))
	    (home (cdr (assoc :HOME lisp::*environment-list*))))
	(if surfhome
	    (concatenate 'string (string-right-trim '(#\/) surfhome) "/")
	    (concatenate 'string (string-right-trim '(#\/) home) "/surf-hippo/"))))

(load (concatenate 'string *surf-home* "misc/loaders/main-loader") :if-source-newer :compile)






