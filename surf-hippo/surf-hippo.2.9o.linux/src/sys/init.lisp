;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: init.lisp

;;; Initialization routines.


(in-package "SURF-HIPPO")

;; Called at the beginning of every simulation by INITIALIZE-SIMULATION, and
;; also by INITIALIZE-GLOBALS-FOR-CIRCUIT and BLANKET-INITIALIZE.
(defun initialize-simulation-vars ()
  ;; For backward compatibility.
  (when nil;; (xor *overlay-simulations-last *overlay-simulations)
    (setq *OVERLAY-ALL-PLOTS* *overlay-simulations
	  *overlay-simulations-last *overlay-simulations))

  ;; If *USE-TIME-LIST* is set, then check a few things.
  (when *use-time-list*
    ;; There has to have been times stored from a prior simulation, with the same stop time.
    (unless (and *last-sim-reverse-time-list* (= (car *last-sim-reverse-time-list*) *user-stop-time*))
      ;; Other, see if the time steps from the last simulation can be used.
      (if (and *sim-reverse-time-list* (= (car *sim-reverse-time-list*) *user-stop-time*))
	  ;; If so, then set flag to transfer the last simulation's time steps.
	  (setq *auto-refresh-last-sim-reverse-time-list* t)
	  ;; Can't do it, so cancel the use of a time list for this simulation.
	  (setq *use-time-list* nil))))
		
  ;; Transfer the time steps from the last simulation for use in this simulation.
  (when *auto-refresh-last-sim-reverse-time-list*
    (setf *last-sim-reverse-time-list* (delete-duplicates *sim-reverse-time-list*))
    (setf *last-sim-reverse-time-step-list* *sim-reverse-time-step-list*)
    (setq *auto-refresh-last-sim-reverse-time-list* nil))
  
  (setf

   ; *CURRENT-SIMULATION-FINISHED* nil
   
   *relative-voltage-lte* 0.0
   *relative-particle-lte* 0.0
   *relative-conc-int-lte* 0.0

   *ARE-THERE-SYNAPSES*  (ARE-THERE-SYNAPSES)
   *particles-are-working* (working-particles-p)
   *conc-particles-are-working* (WORKING-CONC-PARTICLES-P)
   *calculate-particle-error* (and *consider-particle-error* (not (or *use-time-list* *use-fixed-step*))
				   (or *conc-particles-are-working* *particles-are-working*)
				   *active*)

   *calculate-conc-int-error* (and *consider-conc-int-error* (not (or *use-time-list* *use-fixed-step*))
				   *conc-int-type-list*)


					;   *twice-relative-conc-int-error*  (* 2 *relative-conc-int-error*)
					;   *twice-relative-particle-error* (* 2 *relative-particle-error*)

   *vclamp-default-magnitude-double (coerce *vclamp-default-magnitude* 'double-float)
   *synapse-evaluation-times* nil
   *first-time-step* t
    *reverse-sparse-data-times* nil
   *sim-reverse-time-list* '()
   *sim-reverse-time-step-list* '()
   *sim-reverse-plot-time-list* '()
   *FIXED-TIME-STEPS* '()

   *breakpoint-list* *user-breakpoint-list*
   *dynamic-breakpoint-generation* (and *enable-dynamic-breakpoint-generation*
					(not (or *use-time-list* *use-fixed-step*)))

   *simulation-initialized* nil

   ;;   *dc-solution-computed* nil
   *total-num-iterations* 0
   *total-num-time-points* 0
   *particle-w-max-error* ""
   *conc-int-w-max-error* ""
   *particle-ERROR-STEP-less-than-min-step* '()
   *particle-error-step-changes* '()
   *conc-int-ERROR-STEP-CHANGES* '()
   *voltage-error-step-changes* '()
   *lte-was-punted* nil


   *particle-error-max-time-step* 0
   *vsources* (vsources))
  
  (set-stepping-constants)		; Set constants that control time stepping.

  (initialize-time-step)
					;   (update-time-step-variables)
  (clear-element-output-data)

  (setf (*input-time*) 0.0)		;maybe this is a good idea for the first step.

  (setq *simulation-actual-time* (get-universal-time))
  (setq *last-show-time-remaining-time* *simulation-actual-time*)

  (when *show-time-remaining* (init-timer-window)))

;; Called by INITIALIZE-SIMULATION-VARS.
(defun initialize-time-step ()
  (cond
    (*use-fixed-step* (setq *time-step* (round (/ *user-step* *mrt*))
			    *last-time-step* (round (/ *user-step* *mrt*))))
    (t (setf *time-step* *min-step*
	     *last-time-step* *min-step*))))

;; Called by INITIALIZE-SIMULATION-VARS.
(defun set-stepping-constants ()
  (setq *user-start-time* (coerce *user-start-time* 'single-float)) ; just in case...

  (setf (*t[n+1]*) *user-start-time*
	(*t[n]*) *user-start-time*
	(*t-prime[n+1]*) *user-start-time*
	(*t-prime[n]*) *user-start-time*
	(*t-prime[n-prime-1]*) *user-start-time*
	(*t-prime[n-prime]*) *user-start-time*)

  (setf (*delta-t[n]*) (coerce (- (*t[n+1]*) (*t[n]*)) 'double-float))
  (setf (*delta-t[n-1]*) (*delta-t[n]*))

  (setf (*delta-t-prime[n]*) (coerce (- (*t-prime[n+1]*) (*t-prime[n]*)) 'double-float))
  (setf (*delta-t-prime[n-1]*) (*delta-t-prime[n]*))

  (setq *user-stop-time* (coerce *user-stop-time* 'single-float))
  (when (<= *user-stop-time* *user-start-time*)
    (sim-error "The *user-stop-time* must be greater than the *user-start-time*."))
  (setf *mrt* (/ (float (+ *extra-time-after-stop-time* *user-stop-time*)) max-integer-time))
  (setf *stop-time* (truncate (/ *user-stop-time* *mrt*))
	*start-time* (truncate (/ *user-start-time* *mrt*)))
  (setf *INT-USER-STOP-TIME* (truncate *user-stop-time*))
  ;; Make sure that *MAX-STEP* and *MIN-STEP* are FIXNUMs.
  (setf	*max-step*
	(min most-positive-fixnum (truncate (/ (if (> *user-max-step* 0.0) *user-max-step* *user-stop-time*) *mrt*))))
  (setf *min-step* (min most-positive-fixnum (max (truncate (/ *user-min-step* *mrt*)) *MIN-STEP-MRTS)))
  (setf *USER-MIN-STEP-DOUBLE* (d-flt *USER-MIN-STEP*))
  ;; This ensures that SAVE-DATA will save the data the first time it is called.
  (setq *save-data-step-count* (1- *save-data-step*))
  (setq *sim-time-n+1* *start-time*			
	*sim-time-n* *start-time*
	*sim-time-n-1* *start-time*
	*sim-time-n-2* *start-time*))
 
(defun initialize-globals-for-circuit ()
  "Initialize simulator to accept a new circuit definition."
  (if (not *model-hash-table*)
      (create-models)			; create-models clears out all the old elements and element types.
      (clear-instance-model-instances))
  (when *always-clear-models* (clear-model-instances))
  (when *always-clear-types*
    (clear-model-instances)
    (make-new-type-hash-tables))	; If we keep the element type hash tables intact between
					; circuits, then circuits can directly share the current
					; edition of a type.
  (make-new-hash-tables)
  (global-initialize-time-variables)
  (initialize-plotting-variables)
  (initialize-histology-variables)
  (initialize-simulation-vars)
  (clean-up-parameter-lists)		; just to clean things up
  (setq *particle-w-max-error* ""
	*conc-int-w-max-error* ""
	*colorized-windows* nil
	*colorize-simulation* nil
	*enable-sparse-data* nil
	*eval-all-synapses-every-step* nil
					;	*synapse-waveforms nil
	*enable-light* nil
	*simulation-finished* nil
	*simulation-in-progress* nil
	; *CURRENT-SIMULATION-FINISHED* nil
	*DOCUMENTED-USER-VARIABLES* nil
	*LIGHT-INPUT-WAVEFORM* nil)

  (setq *particle-look-up-table-length*
	(round (/ *particle-look-up-table-voltage-range* *particle-look-up-table-precision*)))

  (unless *LAST-SIMULATION-TEMPERATURE* (setq *update-temperature-dependent-parameters* t))
  (setq *LAST-SIMULATION-TEMPERATURE* *TEMPERATURE*
	*LAST-SIMULATION-TEMP-celcius* *TEMP-celcius*)

  (setf *last-lte-node-criterium* nil
	*node-voltage-initializations* '()
	*conc-int-initializations* '()
	*buffer-initializations* '()
	*pump-initializations* '()

	*include-simulation-annotation* nil
	*simulation-annotation* ""
	
	*create-new-histology-window* t
	*circuit-drawn* nil

	*CELL-NAME-SUFFIX* nil
	
	*recheck-circuit-elements-parameters* t
	*LOADED-CIRCUIT-PARTS* nil
	*multiple-source-circuit* nil
;	*ADD-CELL-NAME-TO-SEGS* nil

	*cell-element-simple-name-counter* 0
	*synapse-simple-name-counter* 0
	*channel-simple-name-counter* 0
	*particle-simple-name-counter* 0
	*conc-particle-simple-name-counter* 0
	*axon-simple-name-counter* 0
	*pump-simple-name-counter* 0
	*buffer-simple-name-counter* 0

	
	*circuit* ""
	*simulation-name* ""
	*circuit-filename* ""
	*circuit-file* ""
	*circuit-file-type* nil
	*input-is-function* nil
	*circuit-parts* nil
	
	*circuit-loaded* nil
	*circuit-processed* nil

;;	*synapse-names-to-do-first* nil

	*CALCULATE-ABSOLUTE-LOCATIONS* t 
	*active* t
					;	*only-load-passive* nil


	
					; *pseudo-transient-requested* t

	*ARE-THERE-SYNAPSES* nil
	*neuron-tree-consolidated* nil

	*make-node-w/elements-array* t
	*make-needed-v-particle-arrays* nil
	*make-needed-v-pump-arrays* nil
	*make-needed-v-buffer-arrays* nil


	*make-segment-lists* t

	*setup-voltage-synapses* t
	*setup-light-synapses* t
	*setup-auto-synapses* t

	*SOMA* nil
	*Segment* nil
	*cell* nil
	*cell-type* nil
	*vsource* nil
	*isource* nil
	*axon* nil
	*axon-type* nil
	*synapse* nil
	*synapse-type* nil
	*conc-particle* nil
	*conc-particle-type* nil
	*particle* nil
	*particle-type* nil
	*channel* nil
	*channel-type* nil
	*conc-int* nil
	*conc-int-type* nil
	*pump* nil
	*pump-type* nil
	*buffer* nil
	*buffer-type* nil
	*electrode* nil

	*use-node-voltage-initializations* nil
	*find-steady-state* nil
	*PLOT-NODE-ELEMENTS* nil
	)
  
  (setq *ENABLE-SEGMENT-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-SOMA-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-CONC-INTEGRATOR-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-BUFFER-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-PUMP-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-CHANNEL-TYPE-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-CHANNEL-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-AXON-MEMBRANE-PARAMETER-UPDATE* t
	*ENABLE-SYNAPSE-MEMBRANE-PARAMETER-UPDATE* t)
  
  (clear-node-and-matrix-arrays)

  (setq *ANALYSIS-NODES* nil
	*ANALYSIS-NODES-structures* nil

	*all-save-voltage-nodes* nil
	*all-save-dvdt-nodes* nil)

  (setq *file-output-variable-list* nil)

  ;; Clear working arrays and lengths.
  (clear-working-arrays-and-lengths)

  (declare-ground "Ground"))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun clear-instance-model-instances ()
  (loop for name in *type-model-names*
	when (model-hash-table (gethash name *model-hash-table*))
	do (loop for instance being the hash-value of (model-hash-table (gethash name *model-hash-table*))
		 do (typecase instance
		      (channel-type (clear-channels-of-type instance)
				    (remove-channel-type-lists instance))
		      (synapse-type (clear-synapses-of-type instance)
				    ; (remove-synapse-type-lists instance)
				    )
		      (particle-type (clear-particles-of-type instance))

		      (conc-particle-type (clear-conc-particles-of-type instance))

		      (conc-int-type (setf (conc-int-type-conc-ints instance) nil)
				     (element-parameter instance 'conc-int-array nil))
		      (buffer-type (setf (buffer-type-buffers instance) nil)
				   (element-parameter instance 'buffer-array nil))
		      (pump-type (setf (pump-type-pumps instance) nil)
				 (element-parameter instance 'pump-array nil))
		      (axon-type (setf (axon-type-axons instance) nil)
				 (element-parameter instance 'axon-array nil))
		      (cell-type (setf (cell-type-cells instance) nil)
				 (element-parameter instance 'cell-array nil)))))
		      
  (loop for name in *instance-model-names*
	do (setf (model-hash-table (gethash name *model-hash-table*))
		 (make-hash-table :test #'equal))))

;; Not called by anybody.
(defun clear-type-model-instances ()
  (loop for name in *type-model-names*
	do (setf (model-hash-table (gethash name *model-hash-table*))
		 (make-hash-table :test #'equal))))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun clear-model-instances ()
  (loop for model being the hash-value of *model-hash-table* do
	(setf (model-hash-table model) (make-hash-table :test #'equal))))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun global-initialize-time-variables ()
  (setq *user-breakpoint-list* '()
	*user-start-time* 0.0
	*breakpoint-list* '()
	*last-breakpoint-list* '()
	*last-user-stop-time* 0
	*sparse-data-times* nil
	*reverse-sparse-data-times* nil
  	*SIM-REVERSE-PLOT-TIME-LIST* nil))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun initialize-plotting-variables ()
  (setq *create-new-plot-windows* nil
	*CREATE-NEW-SIMULATION-PLOTS* t
	*overlay-simulations nil *OVERLAY-ALL-PLOTS* nil)
  (clear-all-plot-lists))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun initialize-histology-variables ()
  (dolist (win (clean-up-*output-windows*))
    (dolist (slot '(:CHANNEL-TYPE-GRAPHICS-PARAMETERS
		    :synapse-type-graphics-parameters
		    :marked-branches-and-colors
		    :marked-segments-and-colors
		    :restrict-axons-to-synapse-types
		    :cell-types
		    :cells
		    :chosen-one
		    :chosen-ones))
      (s-value win slot nil)))

  (setq *histology-comment* ""
	*label-nodes nil
	*label-plotted-nodes t
	;; nts stuff
	*soma-outline* nil
	*soma-points* nil))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun clear-node-and-matrix-arrays ()
  (setq *branch-list* '()  
	*branch-array* nil
	*last-seg-branch-array* nil
	*reverse-branch-array* nil
	*branch-array-limit* nil
	*num-nodes* 0
	*num-unknowns* 0

	*diag-double* (make-array '(0) :element-type 'double-float)
	*lower-diag-double* (make-array '(0) :element-type 'double-float)
	*upper-diag-double* (make-array '(0) :element-type 'double-float)
	*v-at-half-step-double* (make-array '(0) :element-type 'double-float)
	*rhs-double* (make-array '(0) :element-type 'double-float)
	*core-node-array* nil
	*ALL-NODE-ARRAY* nil
	*node-w/elements-array* nil
	*node-w/elements-array-length* 0))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun clear-working-arrays-and-lengths ()
  (setq 
	*soma-array* nil

	*segment-guts-list* nil
	*segment-node-2-floats-list*  nil    
	
	*channel-type-list* nil
	*synapse-type-list* nil
;; 	*USE-PARTICLE-ARRAY* t

	*particle-type-list* nil

	*CONC-INT-TYPE-LIST* nil

	*pump-type-list* nil

	*conc-particle-type-list* nil

	*axon-type-list* nil

	*vsource-array* nil
	*fixed-vsource-array* nil
	*isource-array* nil

	*non-ideal-vsource-list* nil
	*fixed-vsource-list* nil
	*isource-list* nil

	*vsource-array-length* 0
;;	*fixed-vsource-array-length* 0
	*isource-array-length* 0))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun clean-up-parameter-lists ()
  (loop for model being the hash-value of *model-hash-table* do
	(setf (model-parameter-type-library model)
	      (delete-duplicates (model-parameter-type-library model) :from-end t :key 'car :test 'eql))))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun make-new-hash-tables ()
  (loop for model-name in (cons "electrode" (cons "node" *instance-model-names*)) do
	(setf (model-hash-table (type-symbol-model model-name)) (make-hash-table :test #'equal))))

;; Called by INITIALIZE-GLOBALS-FOR-CIRCUIT.
(defun make-new-type-hash-tables ()
  (loop for model-name in *type-model-names* do
	(setf (model-hash-table (type-symbol-model model-name)) (make-hash-table :test #'equal))))


;; Creates the model for each circuit model. Called by INITIALIZE-GLOBALS-FOR-CIRCUIT and below when
;; file is loaded.
(defun create-models ()
    (setf *model-hash-table* (make-hash-table :test #'equal))
    (create-soma-model)

    ;; Parent type model creation must occur before the child models.
    (create-channel-type-model)
    (create-channel-model)

    ;; This must come after (create-channel-model) here
    (create-conc-int-type-model)
    (create-conc-int-model)		

    (create-buffer-type-model)
    (create-buffer-model)

    (create-pump-type-model)
    (create-pump-model)		
  
    (create-particle-type-model)
    (create-particle-model)

    (create-conc-particle-type-model)
    (create-conc-particle-model)

    (create-synapse-type-model)
    (create-synapse-model)
    
    (create-axon-type-model)
    (create-axon-model)
    
    (create-segment-model)
    (CREATE-ELECTRODE-MODEL)
    (CREATE-extracellular-ELECTRODE-MODEL)

    (create-cell-type-model)
    (create-cell-model)
    
    (create-isource-model)
    (create-vsource-model)

    (create-node-model)			; Make this last so that it is final option with the ELEMENT
					; functions.
)

(create-models)



;; Not typically used by (SURF) based simulations. Used on initial startup by INIT-SURF (surf-hippo-loader.lisp).
(defun blanket-initialize (&optional (kill-the-hippo t))
  (let ((*always-clear-models* t))
    (initialize-window-system-variables)
    (unless kill-the-hippo (update-cool-hippo-window))
  
    ;;    (create-models)

    (initialize-globals-for-circuit)
    (make-new-type-hash-tables)
    (initialize-simulation-vars)	; For variables that always have to be initialized.
    (get-original-surf-variable-symbols)
    (setq *plot-soma-voltage-p t *plot-node-voltages-p t)
    nil))



;; Called by BLANKET-INITIALIZE.
(defun update-cool-hippo-window ()
  (if *cool-hippo-window-p*
      (unless (opal-obj-exists *cool-hippo-window*)
	(SHOW-IMAGE (concatenate-strings *SURF-HOME* "lib/pix/cool-hippo.half.bmp" )
	 :crop t :left :right :top :bottom :title "Surf's Up")
	(setq *cool-hippo-window*
	      (SHOW-IMAGE (concatenate-strings *SURF-HOME* "lib/pix/cool-hippo.half.bmp" )
			  :crop t :left :right :top :bottom :title "Surf's Up")))
    (when *cool-hippo-window*
      (clear-window *cool-hippo-window*)
      (setq *cool-hippo-window* nil)))
  nil)


(defun clear-all ()
  (blanket-initialize))

;; Called by BLANKET-INITIALIZE.
(defun get-original-surf-variable-symbols ()
  (let ((surf (find-package 'surf)))
    (or *original-surf-variable-symbols*
	(do-symbols (sym surf *original-surf-variable-symbols*)
	  (when (and (eq surf (symbol-package sym))
		     (not (fboundp sym))
		     (not (member sym *original-surf-variable-symbols*)))
	    (push sym *original-surf-variable-symbols*))))))

;; Called by PRINT-DOCUMENTED-USER-VARIABLES.
(defun get-new-surf-variable-symbols ()
  (let ((surf (find-package 'surf))
	new-symbols)
    (do-symbols (sym surf new-symbols)
      (when (and (eq surf (symbol-package sym))
		 (not (fboundp sym))
		 (boundp sym)
		 (not (member sym *original-surf-variable-symbols*)))
	(push sym new-symbols)))))


