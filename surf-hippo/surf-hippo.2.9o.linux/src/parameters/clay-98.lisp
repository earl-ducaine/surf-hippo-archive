;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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
Taken from a preprint version of:


@ARTICLE{Cla-98,
	AUTHOR = {Clay, J. R.},
	TITLE = {Excitability of the squid giant axon revisited},
	JOURNAL = {Journal of Neurophysiology},
	YEAR = {1998},
	VOLUME = {80},
	NUMBER = {2},
	PAGES = {903-913},
	MONTH = {August}
}

Note that the parameters in the published version are different.

This file requires channels defined in 

    surf-hippo/src/parameters/clay-98-na
    surf-hippo/src/parameters/clay-98-k



|#
;; Phi is the width of the periaxonal space
(defvar *clay-98-FARADAY-phi* (/ 1 0.009))

					; value for Fig. 4c
					; but phi = 13.8nm => (/ 1.0  (* 13.8 faraday 1.0e-4)) = 0.0075107557

					; (/ 1 0.0135) Fig.5c, 6c, 8,
					; but phi = 9.2nM => (/ 1.0  (* 9.2 faraday 1.0e-4)) = 0.011266134

					; (/ 1 0.0108) Fig 3
					; mA ms / M cm2 (?)

					; but phi = 11.5nm => (/ 1.0  (* 11.5 faraday 1.0e-4)) 0.009012907
#|

;; I / F phi =

;; coloumbs
;; ---------------------
;; sec cm2 96480 coloumbs mole-1 cm

;; mole
;; ----
;; sec cm3


* (/ 1 (* faraday 11.5 1.0e-4)) ; 11.5 1.0e-4 cm


0.009012907 mole coloumb-1 cm-1


11.5nM = 11.5 1.0e-9 m = 11.5 1.0e-9 1.0e2 cm = 11.5 1.0e-7 cm


coloumb sec-1 cm-2 mole coloumb-1 cm-1 = mole sec-1 cm-3 

= 0.001 M msec-1


coloumbs-1 mole cm-1 1.0e-3 couloumbs sec-1 cm-2

1.0e-3 mole cm-3 sec-1

1.0e-3 mole cm-3 1.0e3  sec-1 1.0e-3

|#

(defvar *clay-98-Kd* 0.002)		;Molar, dissociation constant for removal of excess K by glia

(defvar *clay-98-tau* 12.0)		;ms, time constant for clearance of excess K from the space

(defvar *clay-98-gamma* 5.0)		; 1/ms (?)

(setq *k-conc-extra* 10.0)		; mM, Figs 4c 5c 6c 7c 8
					; 0 Figs 3 7d

(setq *k-conc-intra* 300.0)		; mM



;; Generic conc-ints call a function for determining the dcdt with the arg CINT, returning mM/ms.
(defun clay-98-dkdt-extra (cint)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((total-current (* 1.0e-6 (CONC-INT-MEMBRANE-CURRENT-COMPONENT cint))) ; mA
	 (total-current-per-unit-area (/ total-current (* 1.0e-8 (element-area cint)))) ; mA/cm2
	 (accumulation-term (/ total-current-per-unit-area (the sf *clay-98-FARADAY-phi*))) ; M/ms
	 (conc-shell-extra (conc-int-shell-1-free-conc-n cint)) ; mM
	 (difference-conc (* 0.001 (- conc-shell-extra (the sf *k-conc-extra*)))) ; M
	 )
    (* 1000.0				; M to mM
       (+ accumulation-term		; M/ms
	  (- (/ difference-conc (the sf *clay-98-tau*))) ; M/ms
	  ))))


;; Implicit calculation of concentration at tn+1. Returns mM.
(defun clay-98-k-n+1-extra (cint)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (let* ((delta-t (*real-time-step*))
	 (total-current (* 1.0e-6 (CONC-INT-MEMBRANE-CURRENT-COMPONENT cint))) ; mA
	 (total-current-per-unit-area (/ total-current (* 1.0e-8 (element-area cint)))) ; mA/cm2
	 (accumulation-term (/ total-current-per-unit-area (the sf *clay-98-FARADAY-phi*))) ; M/ms
	 (conc-shell-extra (conc-int-shell-1-free-conc-n cint)) ; mM
	 (difference-conc (* 0.001 (- conc-shell-extra (the sf *k-conc-extra*)))) ; M
	 (f-of-cn (/ 1 (the sf *clay-98-tau*))))
    (/ (+ (* 1000 accumulation-term)	;mM/ms
	  (* conc-shell-extra
	     (- (/ 1 (the sf delta-t))
		(/ 1 (* 2 (the sf *clay-98-tau*)))))
	  (* (the sf *k-conc-extra*)
	     f-of-cn))
       (+ (/ 1 (the sf delta-t))
	  (/ 1 (* 2 (the sf *clay-98-tau*)))))))



;; Returns millimole/ms
(defun clay-98-pump (pump conc-shell-extra)
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0))
	   (double-float conc-shell-extra))
  (let* ((difference-conc (* 0.001 (- conc-shell-extra (the sf *k-conc-extra*)))) ; M
	 (result
	  (* 1000.0			; mM/M
	     (* (the sf *clay-98-gamma*) ; 1/ms
		(/ difference-conc	; M
		   (the df (expt (+ 1 (/ difference-conc (the sf *clay-98-Kd*))) 3)))))))
    (* 0.001				; L/mL
       (pump-conc-int-compartment-volume pump) ; mL
       result)				; millimole / L ms
    ))
    



(pump-type-def
 '(k-clay-98
   (species . k)
   (class . :generic)
   (pump-function . clay-98-pump)	; For :generic pumps, this function is called with args PUMP and concentration
					; (mM), and returns pump current in millimole/ms.
   ))

	
(conc-int-type-def
 `(clay-98-k-ex
   (class . :generic)

   ;; For :generic conc-ints, use either c-n+1-function or explicit integration using
   ;; dcdt-function.
;   (user-spec-eval . t)
   
   (c-n+1-function . clay-98-k-n+1-extra)
;   (dcdt-function . clay-98-dkdt-extra)

   (core-conc . ,*k-conc-extra*)
   (PUMP-TYPE-PARAMS . ((k-clay-98 1)))
   (species . k)
   (valence . 1)
   (intra-p . nil)))


(defun clay-98-squid-membrane ()
  (setq *temp-celcius* 8.0)
  (create-soma :cell (create-cell "clay-98-squid-membrane"
				  :cell-type
				  (create-cell-type "clay-98-squid-membrane"
						    :membrane-resistivity
						    (/ 1
						       (* 0.2 ; mS/cm2
							  0.001 ; S/mS
							  ))
						    :v-leak -59
						    :spcap 1.0))
	       :diameter (sphere-diameter-from-area 1.0e4) ; total area is 0.0001cm2 (1e4um2)
	       )
  (create-element *soma* 'na-clay98 'k-clay98)
  (setq *user-stop-time* 20)
  (add-pulse-list (add-isource *soma*) '(1 100 1))
  (enable-element-plot (somas))
  (enable-element-plot (pumps))
  (enable-element-plot "clay-98-squid-membrane-soma-K-CLAY98" 'reversal-potential)
  (enable-element-plot "clay-98-squid-membrane-soma-K-CLAY98")
  (enable-element-plot (conc-ints)))

#|
(progn (profile::profile CLAY98-EXPONENTIAL-RATE)
       (goferit)
       (profile::report-time)
       (profile::unprofile))

|#