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


;;; SYS Source file: store-plot-data.lisp
(in-package "SURF-HIPPO")


;; For setting up output flags and saving out data during simulation.


;;; SAVE-DATA During the simulation, saves the data and time onto the appropriate lists, every
;;; *SAVE-DATA-STEP* time steps.
(defun save-data (&optional always)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when *save-simulation-data*
    (save-time)
    (when (or always (= 0 (mod (the fn *total-num-time-points*) (the (unsigned-byte 29) *save-data-step*))))
      (save-plot-time)
      (save-models-output)
      (save-total-conductances)
      (user-save-data))))

(defun setup-models-output-data-enabled ()
  (loop for model being the hash-value of *model-hash-table*
	when (model-save-output-data-routine model)
	do (setf (model-output-data-enabled model)
		 (loop for sym in (model-output-data-structure-variables model)
		       when (symbol-value sym)
		       do (return t)))))

(defun save-models-output ()
  (loop for model being the hash-value of *model-hash-table*
	when (model-output-data-enabled model)
	do (funcall (model-save-output-data-routine model))))


(defvar *user-save-data-functions* '() "This is a list of user-defined functions, each of which is
called after plot data is saved.")

#| For example, 
 
(defvar *tuning* (make-array '(16 2)))

;; initialize number of cells per orientation
(loop for column from 0 to 15 do (setf (aref *tuning* column 1) 0))
(loop for soma in (somas)
      do (setf (aref *tuning* (soma-column soma) 1)
	       (1+ (aref *tuning* (soma-column soma) 1) )))

(defun record-column-output ()
  (loop for soma in (somas)
	do (setf (aref *tuning* (soma-column soma) 0)
		 (+ (aref *tuning* (soma-column soma) 0)
		    (* *last-time-step* (soma-voltage soma))))))

(setq *user-save-data-functions* (list 'record-column-output))

;; normalize output
(loop for column from 0 to 15 do
      (setf (aref *tuning* column 0)
	    (/ (aref *tuning* column 0) (* *user-stop-time* (aref *tuning* column 1)))))

(progn (gotimed) (save-array-to-file *tuning*))
|#

(defun user-save-data ()
  (loop for fun in *user-save-data-functions* do (funcall fun)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;     Total Conductance Plotting    ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun plot-total-conductances-plot-labels ()
  (loop for spec in *plot-total-conductance-specifications*
	collect
	(typecase spec
	  (atom (element-name spec))
	  (cons (format nil "~A~{-~A~}" (element-name (car spec))
			(coerce-to-list (if (eq :all (cadr spec)) 'all-elts (element-name (cdr spec)))))))))

(defun parse-*plot-total-conductances* ()
  (setq *plot-total-conductance-specifications*
	(no-nils
	 (loop for preliminary-spec in (coerce-to-list *plot-total-conductances*)
	       nconc
	       (if (eq :all preliminary-spec)
		   (loop for cell in (cells) collect (list cell :all))
		   (list (typecase preliminary-spec
			   (atom (element preliminary-spec))
			   (cons
			    (let ((cell-or-cell-type (element (car preliminary-spec))))
			      (when (or (cell-p cell-or-cell-type)
					(cell-type-p cell-or-cell-type))
				(if (eq (nth 1 preliminary-spec) :all)
				    (list cell-or-cell-type :all)
				    (flatten-list (list cell-or-cell-type (element-type (cdr preliminary-spec)))))))))))))))

(proclaim '(inline channels-total-conductance))
(defun channels-total-conductance (cell-or-chs)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (typecase cell-or-chs
    (cell
     (loop for ch being the hash-value of (channel-hash-table)
	   when (eq cell-or-chs (channel-cell ch))
	   sum (channel-conductance ch) into total double-float
	   finally (return total)))
    (t (do ((chs cell-or-chs (cdr chs))
	    (sum 0.0d0 (the df (+ sum (channel-conductance (car chs))))))
	   ((null chs) sum)))))

(proclaim '(inline channel-type-total-conductance))
(defun channel-type-total-conductance (type &optional cell)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((sum 0.0d0))
    (declare (double-float sum))
    (channel-type-iterator
     (ch type)
     when (or (not cell) (eq (channel-cell ch) cell))  
     do (setq sum (the df (+ sum (channel-conductance ch)))))
    sum))

(proclaim '(inline synapses-total-conductance))
(defun synapses-total-conductance (cell-or-syns)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (typecase cell-or-syns
    (cell (loop for syn being the hash-value of (synapse-hash-table)
		when (eq cell-or-syns (synapse-cell syn))
		sum (synapse-conductance syn) into total double-float
		finally (return total)))
    (t (do ((syns cell-or-syns (cdr syns))
	    (sum 0.0d0 (the df (+ sum (synapse-conductance (car syns))))))
	   ((null syns) sum)))))

(proclaim '(inline synapse-type-total-conductance))
(defun synapse-type-total-conductance (type &optional cell)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((sum 0.0d0))
    (declare (double-float sum))
    (synapse-type-iterator
     (syn type)
     when (or (not cell) (eq (synapse-cell syn) cell))  
     do (setq sum (the df (+ sum (synapse-conductance syn)))))
    sum))

(proclaim '(inline total-conductance))
(defun total-conductance (specification)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((specification (coerce-to-list specification)))
    (s-flt
     (typecase (car specification)
       (synapse-type (the df (synapse-type-total-conductance (car specification))))
       (channel-type (the df (channel-type-total-conductance (car specification))))
       (t (loop for cell in (typecase (car specification)
			      (cell-type (cell-type-cells (car specification)))
			      (cell (list (car specification))))
		summing (the sf (cell-max-g-in cell)) into sf-total single-float
		summing (the df (if (eq :all (cadr specification))
				    (+ (the df (channels-total-conductance cell))
				       (the df (synapses-total-conductance cell)))
				    (loop for type in (cdr specification) summing
					  (the df (typecase type
						    (synapse-type (synapse-type-total-conductance type cell))
						    (channel-type (channel-type-total-conductance type cell))
						    (t 0.0d0)))
					  into df-subtotal double-float
					  finally (return df-subtotal))))
		into df-total double-float
		finally (return (+ sf-total df-total))))))))

(defun save-total-conductances ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when *plot-total-conductance-specifications* ; *PLOT-TOTAL-CONDUCTANCES-P
    (setq *total-conductances-data*      
	  (if *total-conductances-data*
	      (loop for type in *plot-total-conductance-specifications*
		    for data-list in *total-conductances-data*
		    collect (push (the sf (total-conductance type)) data-list))

	      (loop for type in *plot-total-conductance-specifications*
		    collect (list (the sf (total-conductance type)))))))
  nil)
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-time ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (push *real-time* *sim-reverse-time-list*)
  (push *last-time-step* *sim-reverse-time-step-list*)
  nil)

(defun save-plot-time ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (push *real-time* *sim-reverse-plot-time-list*)
  nil)


(defun recorded-node-voltage (node voltage)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (the sf
       (if *isource*			; Shunte le test s'il n'y a aucune source de cree. NG 31/10/97
	   (let ((isrcs (element-parameter-fast :isources (node-parameters node))))
	     (if isrcs
		 (let* ((single-isource-on-this-node (= (length isrcs) 1))
			(isrc (element (car isrcs) 'isource))
			(isrc-params (isource-parameters isrc))
			(isrc-current (isource-current-value isrc))
			(isource-drop
			 (if (and single-isource-on-this-node
				  (element-parameter-fast :enable-isource-drop isrc-params))
			     (* (isource-resistance isrc) isrc-current)
			     0.0))
			(bridge-balance (element-parameter-fast :bridge-balance isrc-params))
			(bridge-drop
			 (if (and single-isource-on-this-node
				  (element-parameter-fast :enable-bridge-balance isrc-params)
				  (numberp bridge-balance))						    
			     (* (the sf bridge-balance) isrc-current)
			     0.0)))
		   (+ voltage (the sf isource-drop) (- (the sf bridge-drop))))
		 voltage))
	   voltage)))


(defun recorded-element-voltage (element)
  (let* ((node (typecase element
		 (segment (segment-node-2 element))
		 (soma (soma-node element))
		 (node element)	
		 (t (element-physical-node element))))
	 (voltage (coerce (node-voltage-n+1 node) 'single-float)))
    (recorded-node-voltage node voltage)))

(defun element-dvdt (element)
  (let ((node (typecase element
		(node element)
		(t (element-physical-node element)))))
    (when node
      (s-flt (if (or *use-fixed-step* (not (THING-IN-ARRAY-P node *NODE-W/ELEMENTS-ARRAY*)))
		 (get-node-dvdt node)
		 (node-dvdt-n node))))))
    
#|
(defun save-node-data ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for element in *all-save-voltage-nodes* do
	(let* ((node (element-physical-node element))
	       (voltage (coerce (node-voltage-n+1 node) 'single-float))
	       (data (the sf (recorded-node-voltage node voltage))))
	  (typecase element
		    (soma (push-soma-voltage-data element data))
		    (segment (push-segment-voltage-data element data)))))
  (loop for element in *all-save-dvdt-nodes* do
	(let ((data (element-dvdt element)))
	  (typecase element
		    (soma (push-soma-voltage-derivative-data element data))
		    (segment (push-segment-voltage-derivative-data element data))))))
|#


(defun SAVE-NODE-DATA ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for element in *ALL-SAVE-VOLTAGE-NODES* do
	(let ((data (recorded-element-voltage element)))
          (typecase element
            (soma (push-soma-voltage-data element data))
            (segment (push-segment-voltage-data element data)))))
  ;; NG, 31/10/97 : je rajoute le test WHEN pour aller plus vite les 3/4 du temps.
  (when *ALL-SAVE-DVDT-NODES*		; ?? does this really help ???
    (loop for element in *ALL-SAVE-DVDT-NODES* do
	  (let ((data (element-dvdt element)))
	    (typecase element
	      (soma (push-soma-voltage-derivative-data element data))
	      (segment (push-segment-voltage-derivative-data element data)))))))






(defun save-soma-data ()
  (save-node-data)
  (loop for soma in *plot-soma-dendrite-currents-structures* do
	(push-soma-dendrite-current-data soma (GET-SOMA-DENDRITE-CURRENT soma))))


;; Handled by SAVE-SOMA-DATA
(defun save-segment-data ())

(defun axon-current-value (axon)
  (let ((axon (element axon 'axon)))
    (when axon (node-voltage-n+1 (axon-node axon)))))


(defun axon-voltage-value (axon)
  (axon-current-value axon))

(defun save-axon-data ()
  (loop for axon in *plot-axons-structures* do
	(push-axon-voltage-data axon (axon-voltage-value axon))))


(defun PARTICLE-STATE-VALUE (particle)
  (let ((prt (element particle 'particle)))
    (when prt (particle-state-n+1-double prt))))

(defun save-particle-data ()
  (loop for particle in *plot-particles-structures* do
	(push-particle-state-data particle (PARTICLE-STATE-VALUE particle)))
  (loop for particle in *plot-markov-particles-structures* do
	(when (and (eq (particle-type-class (particle-type particle)) :markov)
		   t ; (element-parameter particle 'plot-markov-states)
		   )
	  (loop for state-label in (element-parameter (particle-type particle) 'markov-state-labels)
		for state-index fixnum from 0 to (nb-states-1 particle)
		do (push-element-parameter particle
					   state-label
					   (s-flt (particle-aref-state-n+1 (prt-state particle state-index))))))))


(defun pump-current-value (pump)
  (let ((pump (element pump 'pump)))
    (when pump (pump-concentration-current pump))))

(defun save-pump-data ()
  (loop for pump in *plot-pumps-structures* do
	(push-pump-current-data pump (pump-current-value pump))))

(defun buffer-concentration-value (buffer)
  (let ((buffer (element buffer 'buffer)))
    (when buffer (buffer-conc-n+1 buffer))))
	
(defun save-buffer-data ()
  (loop for buffer in *plot-buffers-structures* do
	(push-buffer-concentration-data buffer (buffer-concentration-value buffer))))


(defun CONC-PARTICLE-STATE-VALUE (conc-particle)
  (let ((prt (element conc-particle 'conc-particle)))
    (when prt (conc-particle-state-n+1-double prt))))

(defun save-conc-particle-data ()
  (loop for particle in *plot-conc-particles-structures* do
	(push-conc-particle-state-data particle (conc-particle-state-value particle))))

(defun conc-int-total-value (conc-int)
  (let ((cint (element conc-int 'conc-int)))
    (when cint (conc-int-total-free-conc-n+1 cint))))

(defun conc-int-SHELL-1-VALUE (conc-int)
  (let ((cint (element conc-int 'conc-int)))
    (when cint (conc-int-shell-1-free-conc-n+1 cint))))

(defun conc-int-SHELL-2-VALUE (conc-int)
  (let ((cint (element conc-int 'conc-int)))
    (when cint (conc-int-shell-2-free-conc-n+1 cint))))

(defun conc-int-SHELL-3-VALUE (conc-int)
  (let ((cint (element conc-int 'conc-int)))
    (when cint (conc-int-shell-3-free-conc-n+1 cint))))

(defun save-conc-int-data ()
  (loop for cint in *plot-conc-1-ints-structures* do
	(push-conc-int-shell-1-data cint (conc-int-shell-1-value cint)))
  (loop for cint in *plot-conc-2-ints-structures* do
	(push-conc-int-shell-2-data cint (conc-int-shell-2-value cint)))
  (loop for cint in *plot-conc-3-ints-structures* do
	(push-conc-int-shell-3-data cint (conc-int-shell-3-value cint)))
  (loop for cint in *plot-conc-ints-structures* do
	(push-conc-int-total-data cint (conc-int-total-value cint))))


(defun get-channel-current-not-inline (ch)
  (if (save-ch-current-during-eval-p (channel-type ch)) (channel-current ch) (get-channel-current-n+1 ch)))

(defun CHANNEL-CURRENT-VALUE (channel)
  (let ((ch (element channel 'channel)))
    (when ch (if (channel-active-p ch) (get-channel-current-not-inline ch) 0.0d0))))

(defun CHANNEL-REVERSAL-POTENTIAL-VALUE (channel)
  (let ((ch (element channel 'channel)))
    (when ch (if (channel-active-p ch) (get-channel-e-rev ch) 0.0d0))))

(defun CHANNEL-CONDUCTANCE-VALUE (channel)
  (let ((ch (element channel 'channel)))
    (when ch (if (channel-active-p ch)
		 (if *SAVE-CONDUCTANCES-normalized*
		     (/ (channel-conductance ch) (channel-gbar ch))
		     (channel-conductance ch))
		 0.0d0))))

(defun save-channel-data ()
  (loop for channel in *plot-channel-currents-structures* do
	(push-channel-current-data channel (channel-current-value channel)))
  (loop for channel in *plot-channel-reversal-potentials-structures* do
	(push-channel-reversal-potential-data channel (CHANNEL-REVERSAL-POTENTIAL-VALUE channel)))
  (loop for channel in *plot-channel-conductances-structures* do
	(push-channel-conductance-data channel (CHANNEL-CONDUCTANCE-VALUE channel))))

#|
(defun save-synapse-data () 
  (loop for synapse in *plot-synapse-currents-structures* do
	(push-synapse-current-data
	 synapse
	 (if (synapse-use-reduced-model synapse)
	     (let* ((reduced-synapse-parameters-array (get-synapse-param synapse 'reduced-model-parameters))
		    (conductance (synapse-conductance synapse)))
	       (s-flt (get-reduced-synapse-current conductance reduced-synapse-parameters-array)))
	     (if (synapse-block synapse) 0.0 (get-synapse-current synapse)))))


  ;;  (loop for synapse in *plot-synapse-reversal-potentials-structures* do
  ;;	  (push (synapse-e-rev synapse) (synapse-reversal-potential-data synapse)))

  (loop for synapse in *plot-synapse-conductances-structures* do
	(push-synapse-conductance-data
	 synapse
	 (if (synapse-use-reduced-model synapse)
	     (let* ((reduced-synapse-parameters-array (get-synapse-param synapse 'reduced-model-parameters))
		    (conductance (synapse-conductance synapse)))
	       (s-flt (get-reduced-synapse-conductance conductance reduced-synapse-parameters-array)))
	     (if (synapse-block synapse) 0.0
		 (if *SAVE-CONDUCTANCES-normalized*
		     (/ (synapse-conductance synapse) (synapse-gbar synapse))
		     (synapse-conductance synapse)))))))
|#


(defun SYNAPSE-CURRENT-VALUE (synapse)
  (let ((syn (element synapse 'synapse)))
    (when syn (if (synapse-active-p syn) (get-synapse-current syn) 0.0d0))))



(defun SYNAPSE-REVERSAL-POTENTIAL-VALUE (synapse)
  (let ((syn (element synapse 'synapse)))
    (when syn (if (synapse-active-p syn) (get-synapse-e-rev syn) 0.0d0))))

(defun SYNAPSE-CONDUCTANCE-VALUE (synapse)
  (let ((syn (element synapse 'synapse)))
    (when syn (if (synapse-active-p syn)
		 (if *SAVE-CONDUCTANCES-normalized*
		     (/ (synapse-conductance syn) (synapse-gbar syn))
		     (synapse-conductance syn))
		 0.0d0))))

(defun save-synapse-data ()
  (loop for synapse in *plot-synapse-currents-structures* do
	(push-synapse-current-data synapse (synapse-current-value synapse)))
  (loop for synapse in *plot-synapse-reversal-potentials-structures* do
	(push-synapse-reversal-potential-data synapse (SYNAPSE-REVERSAL-POTENTIAL-VALUE synapse)))
  (loop for synapse in *plot-synapse-conductances-structures* do
	(push-synapse-conductance-data synapse (SYNAPSE-CONDUCTANCE-VALUE synapse))))


;; Isources

(defun save-isource-data ()
  (loop for isource in *plot-isource-currents-structures* do
	(push-isource-current-data isource (get-isource-current isource))))

;; Vsources

(defun save-vsource-data ()
  (loop for vsource in *plot-vsource-currents-structures* do
	(push-vsource-current-data vsource (vsource-current-value vsource)))
    (loop for vsource in *plot-vsource-voltages-structures* do
	(push-vsource-voltage-data vsource (vsource-voltage-value vsource))))


;;; CLEAR-ELEMENT-OUTPUT-DATA Initialize all the plot data lists to '().
(defun clear-element-output-data ()
  (when *enable-sparse-data* (clear-sparse-data))
  (clear-total-conductances)
  (clear-markov-particles-output-data)
  (setq *vsrvolt* nil
	*vsrnodevolt* nil)
  (loop for model being the hash-value of *model-hash-table*
	when (model-output-data-keys model)
	do (loop for instance being the hash-value of (model-hash-table model)
		 do (remove-element-parameters instance (model-output-data-keys model)))))


(defun clear-total-conductances ()
  (parse-*plot-total-conductances*)
  (setq *total-conductances-data* nil))


(defun clear-markov-particles-output-data ()
  (loop for type in (particle-types)
	when (element-parameter type 'markov-state-labels) do
	(let ((labels (element-parameter type 'markov-state-labels)))
	  (loop for particle in (particle-type-particles type) do
		(loop for state-label in labels do (element-parameter particle state-label nil))))))
	
  



