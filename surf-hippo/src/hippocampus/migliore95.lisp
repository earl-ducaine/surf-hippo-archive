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

;; From -
;;   Computer simulations of a morphologically reconstructed
;;   CA3 hippocampal neuron
;;   M.Migliore, EP. Cook, DB. Jaffe, DA. Turner, D. Johnston
;;   J. Neurophysiol, March 1995, vol. 73, no. 3, 1157-1168
;;

;; See also parameters/migliore95-chs.lisp

(defvar mig95-k-ca-channels
  '(ca-t-mig95 ca-n-mig95 ca-l-mig95
    ka-mig95 km-mig95 kahp-mig95
    kdr-mig95 kc-Mig95))
 

(defun mig95-set-k-e-revs-to-cell-type ()
  (loop for k-ch-type in (CHANNEL-TYPES-OF-ION-TYPE 'k t t)
	do (setf (channel-type-use-defined-e-rev k-ch-type) nil))
  (loop for na-ch-type in (CHANNEL-TYPES-OF-ION-TYPE 'na t t)
	do (setf (channel-type-use-defined-e-rev na-ch-type) nil)))

(defmacro mig95-cell-type ()
  `(create-cell-type "mig95" :rm 60e3 :ri 200 :spcap 1 :vl -65.0
    :e-k -91 :e-na 50
    :e-na-dependence :fixed
    :e-k-dependence :fixed))

(defun mig95-hippo ()
  (let ((cell (dead-hippo "mig95-hippo" (mig95-cell-type))))
    (process-circuit-structure)
    (let* ((apical-segments (segments-out (car (trunk-segments))))
	   (na-segs (loop for seg in apical-segments
			  when (< (distance-to-soma seg) 500 ; apical-extent-of-na-k
				  )
			  collect seg)))
      (add-isource *soma*)
      (create-element *soma* na-segs 'na-Mig95)
					;      (mig95-plot-settings-hippo)
      (create-element (cell-elements) mig95-k-ca-channels)
      (mig95-set-k-e-revs-to-cell-type))))



(defun mig95-nak-soma (&optional (apical-extent-of-na-k 150))
  (let ((cell (ca1-max-red :cell-type (mig95-cell-type)
			   :name "mig95-c12861-ca1")))

    (process-circuit-structure)
    (let* ((apical-segments (segments-out (element "46"))) ; By eye.
	   (synapse-segments (loop for seg in apical-segments
				   when (< 100 (distance-to-soma seg) 250)
				   collect seg))
	   (na-segs (loop for seg in apical-segments
			  when (< (distance-to-soma seg) apical-extent-of-na-k)
			  collect seg)))
      (add-isource *soma*)
      (create-element *soma* na-segs 'na-Mig95)
      (create-element (cell-elements) mig95-k-ca-channels)

      (create-element synapse-segments 'mig95-syn)
      (element-type-param 'mig95-syn 'gbar-ref
			  ;; 5nS total, spread out over all the syns.
			  (/ 5.0e-3 (length synapse-segments)) t)

      (let ((events (loop for time from 10.0 to 520 by 2 collect time)))
	(add-events (SYNAPSEs) EVENTS))))
  (mig95-set-k-e-revs-to-cell-type)
  (mig95-plot-settings))


  
(defun mig95-n120 (&optional (apical-extent-of-na-k 150))
  (setq *plot-channels-by-major-ion* t)
  (let ((cell (n120-max-red)))
					;    (setf (cell-name cell) "mig95-n120")
    (setf (cell-type cell) (mig95-cell-type))
    (process-circuit-structure)
    (let* ((apical-segments (segments-out (element "17"))) ; By eye.
	   (na-segs (loop for seg in apical-segments
			  when (< (distance-to-soma seg) apical-extent-of-na-k)
			  collect seg)))
      (add-isource *soma*)
      (create-element (or (soma-segments) *soma*) 'na-Mig95)
      (create-element na-segs 'na-Mig95)
      (create-element (cell-elements) mig95-k-ca-channels)))
  (mig95-set-k-e-revs-to-cell-type))


(defun mig95-plot-settings ()
  (setq *plot-channels-by-major-ion* t)

  (setq *PLOT-NODE-VOLTAGES-P T)
  (setq *PLOT-NODE-VOLTAGE-DERIVATIVES-P NIL)
  (setq *PLOT-PATH-NODE-VOLTAGES-P NIL)
  (setq *PLOT-AXON-VOLTAGES-P NIL)
  (setq *PLOT-AXON-EVENTS-P NIL)
  (setq *PLOT-SYNAPSE-EVENTS-P NIL)
  (setq *PLOT-SOMA-VOLTAGE-P T)
  (setq *PLOT-SOMA-VOLTAGE-DERIVATIVE-P NIL)
  (setq *PLOT-SOMA-DENDRITE-CURRENTS-P NIL)
  (setq *PLOT-CHANNEL-CURRENTS-P T)
  (setq *PLOT-CHANNEL-CONDUCTANCES-P NIL)
  (setq *PLOT-CHANNEL-REVERSAL-POTENTIALS-P NIL)
  (setq *PLOT-SYNAPSE-CURRENTS-P NIL)
  (setq *PLOT-SYNAPSE-CONDUCTANCES-P NIL)
  (setq *PLOT-SHELL-1-CONCENTRATIONS-P T)
  (setq *PLOT-SHELL-2-CONCENTRATIONS-P NIL)
  (setq *PLOT-SHELL-3-CONCENTRATIONS-P NIL)
  (setq *PLOT-CONCENTRATIONS-P NIL)
  (setq *PLOT-BUFFER-CONCENTRATIONS-P NIL)
  (setq *PLOT-CONC-PARTICLES-P NIL)
  (setq *PLOT-PARTICLES-P NIL)
  (setq *PLOT-PUMP-CURRENTS-P NIL)
  (setq *PLOT-ISOURCE-CURRENTS-P NIL)
  (setq *PLOT-VSOURCE-CURRENTS-P NIL))



(defun mig95-plot-settings-hippo ()
  (setq *plot-channels-by-major-ion* t)
  (setq *PLOT-NODES* 
	`( "Hippo-5"  )
	)

  (setq *PLOT-SOMA-NODES* 
	`( "Hippo-soma"  )
	)

  (setq *PLOT-CHANNEL-CURRENTS* 
	`( "Hippo-soma-KC-MIG95"  "Hippo-soma-kdr-mig95"  "Hippo-soma-CA-L-JAFFE"
	  "Hippo-soma-CA-N-JAFFE"  "Hippo-soma-CA-T-JAFFE"  "Hippo-soma-na-Mig95"  ) )

  (setq *PLOT-CHANNEL-CONDUCTANCES* 
	`( "Hippo-soma-KC-MIG95"  "Hippo-soma-kdr-mig95"  "Hippo-soma-CA-L-JAFFE"
	  "Hippo-soma-CA-N-JAFFE"  "Hippo-soma-CA-T-JAFFE"  "Hippo-soma-na-Mig95"  ) 	)

  (setq *PLOT-CHANNEL-REVERSAL-POTENTIALS* 
	`( "Hippo-soma-KC-MIG95"  "Hippo-soma-kdr-mig95"  "Hippo-soma-CA-L-JAFFE"
	  "Hippo-soma-CA-N-JAFFE"  "Hippo-soma-CA-T-JAFFE"  "Hippo-soma-na-Mig95"  ) )

  (setq *PLOT-CONC-1-INTS* 
	`( "Hippo-soma-CA-IN-jaffe-94"  )
	)

  (setq *PLOT-ISOURCE-CURRENTS* 
	`( "Hippo-soma-isrc"  )
	)

  (setq *PLOT-NODE-VOLTAGES-P T)
  (setq *PLOT-NODE-VOLTAGE-DERIVATIVES-P NIL)
  (setq *PLOT-PATH-NODE-VOLTAGES-P NIL)
  (setq *PLOT-AXON-VOLTAGES-P NIL)
  (setq *PLOT-AXON-EVENTS-P NIL)
  (setq *PLOT-SYNAPSE-EVENTS-P NIL)
  (setq *PLOT-SOMA-VOLTAGE-P T)
  (setq *PLOT-SOMA-VOLTAGE-DERIVATIVE-P NIL)
  (setq *PLOT-SOMA-DENDRITE-CURRENTS-P NIL)
  (setq *PLOT-CHANNEL-CURRENTS-P T)
  (setq *PLOT-CHANNEL-CONDUCTANCES-P NIL)
  (setq *PLOT-CHANNEL-REVERSAL-POTENTIALS-P NIL)
  (setq *PLOT-SYNAPSE-CURRENTS-P NIL)
  (setq *PLOT-SYNAPSE-CONDUCTANCES-P NIL)
  (setq *PLOT-SHELL-1-CONCENTRATIONS-P T)
  (setq *PLOT-SHELL-2-CONCENTRATIONS-P NIL)
  (setq *PLOT-SHELL-3-CONCENTRATIONS-P NIL)
  (setq *PLOT-CONCENTRATIONS-P NIL)
  (setq *PLOT-BUFFER-CONCENTRATIONS-P NIL)
  (setq *PLOT-CONC-PARTICLES-P NIL)
  (setq *PLOT-PARTICLES-P NIL)
  (setq *PLOT-PUMP-CURRENTS-P NIL)
  (setq *PLOT-ISOURCE-CURRENTS-P NIL)
  (setq *PLOT-VSOURCE-CURRENTS-P NIL))



  
#|
(defun test-mig (&optional (include-control t))
  (unblock-all-channel-types)
  (let ((user-stop-time 160) *BEEP-AFTER-SURF* *BEEP-AFTER-gc*
	*automatic-voltage-plot-scaling
	(*soma-voltage-plot-min -80)(*soma-voltage-plot-max 20)
	(*voltage-plot-min -80)(*voltage-plot-max 20)
	)
    (reset-non-unity-channel-type-gbar-modulation)
    (when include-control
      (cc-steps 0.2 0.4 .2 :individual-plots t
		:timeit t :current-start-time 10 :current-stop-time 100
		:extra-comment
		(loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
		      collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
		      into out
		      finally (return (concatenate-string-list (cons "" out) :string-count-to-add-linefeed 1)))))
    (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95) do
	  (reset-non-unity-channel-type-gbar-modulation)
	  (loop for gbar-mod in '(2.0 5.0) do
		(element-parameter type 'gbar-modulation gbar-mod t)
		(cc-steps 0.2 0.4 .2 :individual-plots t
			  :timeit t :current-start-time 10 :current-stop-time 100
			  :extra-comment
			  (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
				collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
				into out
				finally (return
					  (concatenate-string-list (cons "" out) :string-count-to-add-linefeed 1))))))))


(defun test-mig (&optional (include-control t))
  (unblock-all-channel-types)
  (let ((user-stop-time 50) *BEEP-AFTER-SURF* *BEEP-AFTER-gc*
	(*CREATE-NEW-SIMULATION-PLOTS* t)
	*automatic-voltage-plot-scaling
	(*soma-voltage-plot-min -80)(*soma-voltage-plot-max 20) (*voltage-plot-min -80)(*voltage-plot-max 20))
    (element-parameter '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95) 'gbar-modulation 1.0 t)
    (when include-control
      (let ((*simulation-plot-window-comment*
	     (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
		   collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
		   into out
		   finally (return (concatenate-string-list out :string-count-to-add-linefeed 1)))))
	(goferit)(setq *CREATE-NEW-SIMULATION-PLOTS* t)))
    (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95) do
	  (element-parameter '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95) 'gbar-modulation 1.0 t))
	  (loop for gbar-mod in '(2.0 5.0) do
		(element-parameter type 'gbar-modulation gbar-mod t)
		(let ((*simulation-plot-window-comment*
		       (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
			     collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
			     into out
			     finally (return (concatenate-string-list out :string-count-to-add-linefeed 1)))))
		  (setq *CREATE-NEW-SIMULATION-PLOTS* t)
		  (goferit))))))

(defun test-mig-syns ()
  (unblock-all-channel-types)
  (let ((user-stop-time 900) *BEEP-AFTER-SURF* *BEEP-AFTER-gc*
	(*CREATE-NEW-SIMULATION-PLOTS* t)
	*automatic-voltage-plot-scaling
	(*soma-voltage-plot-min -80)(*soma-voltage-plot-max 20) (*voltage-plot-min -80)(*voltage-plot-max 20))

    (reset-non-unity-channel-type-gbar-modulation)

    (loop for gbar-mod in '(1.0 2.0 5.0) do
	  (element-parameter 'km-mig95 'gbar-modulation gbar-mod t)
	  (loop for gbar-mod in '(1.0 2.0 5.0) do
		(element-parameter 'ka-mig95 'gbar-modulation gbar-mod t)
		(loop for gbar-mod in '(1.0 2.0 5.0) do
		      (element-parameter 'kdr-mig95 'gbar-modulation gbar-mod t)

		      (core-mig-syn))))))
		      


(defun core-mig-syn ()
  (let ((*simulation-plot-window-comment*
	 (loop for type in '(ka-mig95 km-mig95 kahp-mig95 kdr-mig95 kc-Mig95)
	       collect (format nil "~A ~,2f" type (or (element-parameter type 'gbar-modulation) 1))
	       into out
	       finally (return (concatenate-string-list out :string-count-to-add-linefeed 1)))))
    (setq *CREATE-NEW-SIMULATION-PLOTS* t)
    (goferit)))

;; useful functions (ARRANGE-PLOT-WINDOWS t) (default-window-font-menu)




|#
