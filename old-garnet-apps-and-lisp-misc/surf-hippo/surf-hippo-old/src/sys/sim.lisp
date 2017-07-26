;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10 ; -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 6/09/85 12:15:40
;
; Main routine for circuit simulator, by Don Webber

; Note that the function SURF (defined in file MAIN) actually calls SIM.


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defun sim (&optional circuit)
  (introduce)
  (initialize-for-simulation)
  (if circuit
      (progn
	(initialize-for-circuit)
	(display-message (format nil "Reading in circuit ~a..." circuit)) (readin circuit)
	(display-message (format nil "Collecting circuit objects...")) (collect-circuit-objects)
	(display-message (format nil "Locating nodes..."))	(locate-all-nodes)))

  ;; May want to do this somewhere else.
  (setq *time-stamp (get-universal-time))
  (setq *simulation-name* (format nil "~a-~a" *circuit-name* *time-stamp))
  (setq *display-time-increment (max 1 (floor (/ user-stop-time 18))))
  #-sun (if (and (not *automatic-run) *modify-globals) (globals-menu))
  #+sun (if *modify-globals (user-set-globals))
  (set-circuit-source-values)
  (set-constants)
  (set-circuit-elements-parameters)	; We have to do these two functions now because we
					;have to know the dimensions of the cell segments,
					;which was found by LOCATE-ALL-NODES.
  #-sun (choose-plot-data)		;This is menu-driven. See notes in SYMBOLICS-MISC file
					;for Common LISP version comments. 
  #+(or garnet sun) (if *draw-cells (draw-cells))
  (fix-circuit circuit)
  ;; Tell the appropriate devices to save their values during the simulation.
  (set-up-output-flags)
  (print-circuit)
  #+parallel (remove-empty-models)
  (if *pseudo-transient-requested*
      (without-floating-underflow-traps	(dc-solution)))
  (if (and *surf-interactive*
	   #+parallel (not
		       (y-or-n-p
			(format t "Do you want to measure cm-busy time? (this slows simulation) "))))
      (time (simulate))
      (simulate))
  (simulation-output))

#+parallel
(defun remove-empty-models ()
  (loop for mod in (eval '*models*)
	when (loop for model-instance in (model-template-instances mod)
		 when (model-instance-elements model-instance)
		   return t
		   finally (return nil))
	  collect mod into new-models
	finally (setq *models* new-models)))

(defun introduce ()
  (format t "~%        -*-  Surf-Hippo  -*-~%")
  #-parallel
  (format t "~%MIT CBIP Neuron Simulator -- Serial Version~%")
  #+parallel
  (format t "~%MIT CBIP/TMC Neuron Simulator -- Parallel Version~%"))

(defun simulate ()
  (without-floating-underflow-traps
    (do-time-control)))

(defun set-circuit-elements-parameters ()
  #-sun  (change-cell-types-parameters)	;For the sun version these parameters will have to be changed 
						;from the circuit file's values someway else.
  (set-segments-membrane-parameters)		
  (set-somas-membrane-parameters)		
  (set-conc-integrators-parameters)	
  (set-channels-parameters)
  (set-synapses-parameters)
)

(defun fix-circuit (circuit)
  (cond (circuit
	 "Creates the core nodes and elements and any other processing that has to
   be done to the data structure before simulation."
	 (mark-vsources)		; marks nodes that are vsources 
;	 (add-cmin)			; adds the required minimum capacitance
	 (if (or *use-Hines* *use-tridiagonal*)
	     (reorder-circuit))		; tries to put the large matrix terms on the tri-diagonal
	 (fix-nodes)))			; makes the core nodes
  (setup-light-synapses)
  (set-initial-node-voltages)		;node-hash-table is needed at this point.
  )

(defun fix-elements ()
  "Creates the core elements"
  (dolist (mod *models*)
    (dolist (inst (model-template-instances mod))
      (dolist (elt (model-instance-elements inst))
	(funcall (model-template-create-core-routine mod) elt)))))
