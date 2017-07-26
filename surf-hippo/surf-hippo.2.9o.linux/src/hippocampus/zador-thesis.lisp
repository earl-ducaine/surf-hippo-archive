; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

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


(in-package "SURF-HIPPO")

#|

Derived from -

@PHDTHESIS{Zad-93,
	AUTHOR = {Zador, A. M.},
	TITLE = {Biophysics of computation in single hippocampal neurons},
	SCHOOL = {Yale University},
	YEAR = {1993}
}

see also surf-hippo/src/parameters/zador-chs.lisp

|#



(create-cell-type "amaral-ca1" ; "zador-ca1"
		  :rm 150e3		; also 15e3
		  :ri 200			
		  :spcap 1
		  :vl -70.0)

(setq *nts-cell-type* "amaral-ca1"
      *nts-cell-name* "tz_c12861.ca1")
		  
(read-in-circuit (concatenate 'string *surf-user-dir* "/" "anatomy/misc/" "c12861.ca1.lisp"))
	 

(create-channel *soma* 'dr-tz-ca1-hh)
(create-channel *soma* 'na-tz-ca1-hh)