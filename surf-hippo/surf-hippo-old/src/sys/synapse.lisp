;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10; -*-
;;; (c) Copyright 1988, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 1/25/88 17:58:12
;
; the synapse model, part of the neuron model
;

#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defstruct synapse
  "Model for a synapse"
  (name ""		:type string)   
  model
  (plot-current nil :type boolean)
  (plot-conductance nil :type boolean)
  node
  core-syn
  (cell-element nil)			;What the synapse is a part of (soma or segment)
  (is-controlled nil :type boolean)	;T for synapses whose conductance waveform is an a-priori
  (is-light-dependent nil :type boolean) ;If T then synapse is driven directly by light, if NIL then by
					;the voltage of some other node.
  (pre-synaptic-node )			;For voltage-dependent synapses, the controlling node, and
  (channel)				;the voltage-dependent channel of the post-synaptic membrane/segment.
  (type nil)
  (type-parameters nil)			;List of miscellaneous parameters for certain types
  (delay 0.0 :type single-float)	;For both voltage and light dependent synapses, look at controlling
					;variable at (time - delay). When synapse-waveform is
					;used, then this delay is also used.
  (waveform nil )			;Pre-computed conductance waveform array for light-dependent
					;synapses, or voltage waveform for voltage-dependent synapses.
  impulse				;Impulse response for light-dependent synapses
  (is-within-aperture nil :type boolean) ;For light-dependent synapses, an aperture masks the input.
  (light-spatial-rf-parameters '())	;For light-dependent synapses, a list defining the spatial receptive field.
  ;;  (light-input-offset-distance 0.0)		;Light input offset distance, in microns.
  ;;  (light-input-offset-angle 0.0)		;Light input offset angle, in radians.
  (light-input-delay 0.0 :type single-float) ;Light input delay between light event at offset location and
					;synaptic response, in milliseconds.
  (ca-shell-node nil)
  (gbar-density 0.0 :type single-float)	;pS per square micron
  (gbar 0.0 :type single-float)		;microsiemans
  (v-reversal 0.0 :type single-float)
  (variable-e-ca nil :type boolean)
  (ca-conc-extra 0.0 :type single-float)
  (Eca-Nearst-eqn-const 0.0 :type single-float)
  (current-data '() :type list)
  (conductance-data '() :type list))

#-parallel
(defstruct core-synapse
  "Core model for a synapse."
  (node-pointp nil	:type boolean)	; zero if constant, one if node
  node-point
  (node-const zero	:type single-float)
  ca-shell-point
  (ca-shell-pointp nil	:type boolean)
  (ca-shell-const zero	:type single-float)
  (current 0.0		:type single-float)
  (conductance 0.0      :type single-float)
  (gbar 0.0 :type single-float)
  (v-reversal 0.0 :type single-float)
  (variable-e-ca nil :type boolean)
  (ca-conc-extra 0.0 :type single-float)
  (Eca-Nearst-eqn-const 0.0 :type single-float)
  (delay 0.0 :type single-float)					
  (synapsep nil :type boolean)
  (pre-synaptic-point nil)
  (pre-synaptic-pointp nil)
  (pre-synaptic-const nil)
)

(defun create-synapse-model ()
  "Creates a template for all synapses."
  (let ((template (make-model-template)))
    (setf
      (model-template-name template) (string "synapse")
      (model-template-default-params template) '()
      (model-template-eval-routine template) #'eval-synapse
      (model-template-print-routine template) #'print-synapse
      (model-template-create-routine template) #'create-synapse
      (model-template-create-core-routine template) #'create-core-synapse
      (model-template-add-off-diag-routine template) #'add-off-diag-synapse
      (model-template-find-coupling-routine template) #'find-coupling-synapse
      (model-template-fix-dc-nodes-routine template) #'synapse-fix-dc-nodes
      *models* (cons template *models*)
      (gethash (string "synapse") *model-hash-table*) template
      synapse-hash-table (make-hash-table :test #'equal))
    ; only need one synapse model instance, so create it now.
    (create-model-instance (string "syn") (model-template-name template) '() )))

(defun print-synapse (syn)
  "Prints out this data associated with a synapse."
  (format *output-stream "Synapse ~a , cell ~a; gbar ~,2e uS, gbar-dens ~,2e pS/sq-um, v-rev ~a mV ~%"
	  (synapse-name syn)
	  (cell-name (node-cell (synapse-node syn)))
	  (synapse-gbar syn)
	  (synapse-gbar-density syn)
	  (synapse-v-reversal syn))
  (if (synapse-type-parameters syn) (format *output-stream  "   Type parameters: ~a ~%" (synapse-type-parameters syn)))
  (if (synapse-is-light-dependent syn)
      (format *output-stream "   Light-Dependent Spatial RF: ~a ~%"
	      (first (synapse-light-spatial-rf-parameters syn)))))

(defun set-synapses-parameters ()
  (maphash 'set-synapse-parameters synapse-hash-table))

(defun set-synapse-parameters (name synapse)
  (declare (ignore name))
  (setf (synapse-gbar synapse) (g-element (synapse-cell-element synapse) (synapse-gbar-density synapse))))

;;;; *********** SET UP SYNAPSE FUNCTIONS ********** These functions are for setting up LIGHT dependent synapses
;;;; prior to the simulation run.


;;; Review of Channel Conductance Data (from Hille, 1984 p.222)
;;;
;;;
;;;	Synapse Channels --
;;;
;;; Excitatory (Cation-permeable) Channels
;;;									- pS -
;;;	Ach - agonist
;;;		Amphibian, reptile, bird and mammalian endplate		20-40
;;;		Rat Myotubules						49
;;;		Bovine Chromaffin cells					44
;;;		Aplysia ganglion					8
;;;	Glutamate - agonist
;;;		Locust Muscle						120-130
;;;
;;; Inhibitory (Chloride-permeable) Channels
;;;
;;;	Glycine - agonist
;;;		Lamprey brain stem neurons				73
;;;		Cultured Mouse spinal neurons				30
;;;	GABA - agonist
;;;		Cultured Mouse spinal neurons				18
;;;		Crayfish Muscle						 9
;;;
;;;	Non-Synaptic Channels --
;;
;;; Na channels range from approximately 4 - 20 pS per channel.
;;; K channels range from approximately 2 - 200 pS per channel.
;;;
;;;
;;; Membrane density is highest for receptor proteins, up to 10,000 per square micron.
;;; In comparison typical channel densities for Na and K channels range roughly from 1 - 2000 per square micron.
;;;
;;; For modelling purposes, then reasonable ranges for conductance density parameters are as follows:
;;
;;	Excitatory Channels (Cholinergic) -- 1 - 100,000 ps per square micron
;;	Excitatory Channels (Glutamerigic) -- 100 - 100,000 ps per square micron

;;; Compare these values to typical values of membrane resistivity:

;;; 10,000 ohms-cm-cm = 1 ps/sq-uM
;;; 40,000 ohms-cm-cm = 0.25 ps/sq-uM
;;; 100,000 ohms-cm-cm = 0.1 ps/sq-uM

;;; Need an estimate for synaptic density on typical processes.....

(defvar *g-excitatory-1-dens 10.0)		;pS per square micron
(defvar *g-inhibitory-1-dens 1000.0)		;pS per square micron

(defvar *g-excitatory-2-dens 0.0)			;pS per square micron
(defvar *g-v-excitatory-1-dens 0.0)		;pS per square micron
(defvar *g-ex-fac-dens 50.0)			;pS per square micron
(defvar *g-excitatory-sus-dens 0.0)		;pS per square micron

; Alternate synaptic conductance parameters. Used for example by the G-EX-SYN and G-IN-SYN functions.
(defvar *g-ex-mem 1.0e-5 "Conductance of an excitory synapse in S/cm^2")
(defvar *G-IN-MEM 1.0e-5 "Inhibitory synaptic membrane conductance (ohm-cm-cm)^-1")


;; Variables to modify the relationship between the input light pattern and light-activated synapses.
(defvar *light-input-offset-distance 0.0)	;Light input offset distance, in microns.
(defvar *light-input-offset-angle 0.0)		;Light input offset angle, in radians.
(defvar *light-input-delay 0.0)			;Light input delay between light event at offset
						;location and synaptic response, in milliseconds.

;Reversal potentials of excitatory and inhibitory synapses.
(defvars-w-value (*e-rev-excitatory-1 0.0) (*e-rev-excitatory-2 0.0)
  (*e-rev-ex-fac 0.0)
  (*e-rev-ex-fac-2 0.0)
  (*e-rev-inhibitory-1 -70.0)(*e-rev-inhibitory-1-offset -70.0) 
  (*e-rev-excitatory-sus 0.0)
  (*e-rev-v-excitatory-1 0.0))

(defvar *excitatory-facilitation-impulse-array)

(defvar *include-light-synapses nil)



  ;;;;Synapse conductance determined by (synapse-event-driven-value syn), which references
  ;;;;(synapse-waveform syn), starting when time = (synapse-delay syn).
  ;;;;(get-random-synapse-event-times) may be used to fill all the (synapse-delay syn) slots
  ;;;;according to some criteria of each synapse.
(defvar *use-synapse-events)

(defvar  *use-old-synapse-waveforms nil)
(defvar *synapse-waveforms '())
(defvar *input-waveform-array)
(defvar *light-origin-array)
(defvar *light-speed 0.0)			;Microns per millisecond
(defvar *bar-width 0.0)				;Microns
(defvar *bar-length 0.0)			;Microns
(defvar *light-theta (* 90 2.0 pi-single (/ 1.0 360)))			;Radians
(defvar *cos-theta (cos *light-theta))
(defvar *sin-theta (sin *light-theta))
(defvar *light-input-offset-angle-degrees)	;Just used in menu
(defvar *light-theta-degrees 90.0)			;Just used in menu
(defvar *light-direction T)			;T (nil) => movement is 90 degrees ahead (behind) of *light-theta
(defvar *motion-start-time 0.0)			;Time to start bar moving, milliseconds
(defvar *light-start-position-x 0.0)		;Point of center of stimulus at *motion-start-time in microns
(defvar *light-start-position-y 0.0)
(defvar *motion-stop-time 100000.0)		;Time to stop bar moving, milliseconds
(defvar *grating-temporal-period 1000000.0)		;Milliseconds
(defvar *grating-spatial-period 1000000.0)		;Microns
(defvar *use-aperture nil)
(defvar *aperture-radius 300.0)			;Microns
(defvar *aperture-center-x 0.0)			;Microns
(defvar *aperture-center-y 0.0)			;Microns


(proclaim '(single-float *light-theta *bar-width *bar-length *light-speed
 *light-input-offset-angle-degrees *light-theta-degrees
 *light-start-position-x *light-start-position-y))

(defvar *bar-a-width 0.0 "microns" )
(defvar *bar-a-length 0.0 "microns" )
(defvar *bar-a-intensity 1.0)
(defvar *bar-a-start-time 0.0 "milliseconds")
(defvar *bar-a-stop-time 0.0 "milliseconds")
(defvar *bar-a-position-x 0.0 "microns")
(defvar *bar-a-position-y 0.0 "microns")

(defvar *bar-b-width 0.0 "microns")
(defvar *bar-b-length 0.0 "microns")
(defvar *bar-b-intensity 1.0)
(defvar *bar-b-start-time 0.0 "milliseconds")
(defvar *bar-b-stop-time 0.0 "milliseconds")
(defvar *bar-b-position-x 0.0 "microns")
(defvar *bar-b-position-y 0.0 "microns")

(defvars-w-value (*synapse-g-leak-ratio 1.0)(*light-stimulus 'spot)(*light-stimulus-start-time 0.0)
		 (*light-stimulus-stop-time 100000.0)
		 (*light-stimulus-spot-center-x 0.0)(*light-stimulus-spot-center-y 0.0) 
		 (*light-stimulus-spot-outside-diameter 0.0)(*light-stimulus-spot-inside-diameter 0.0)
		 (*light-stimulus-strength 1.0))
(defvar *spot-outside-diameter 0.0)
(defvar *spot-inside-diameter 0.0)

  
;;; CREATE-SYNAPSE A synapse label should be added to the name of 'segment' or 'node' to generate the
;;; 'synapse-name'. The node for the synapse is the same as the segment node-2 or the soma node.   A
;;; synapse that is controlled by the voltage of another node is specified by supplying 
;;; pre-synaptic-node-name and the pre-synaptic-cell-name. This type of synapse is realized by a channel in the
;;; appropriate cell-element whose  particles are controlled by the voltage of pre-synaptic-node-name. The
;;; type argument is used for setting up the  parameters of the associated channel for a voltage-dependent
;;; synapse.   Conversely, a synapse  which is  controlled  by an  extrinsic light  input is 
;;; specified  by  the  setting the is-light-dependent flag. The impulse reponse of a light-dependent
;;; synapse is given by impulse. The cell-element may be either a soma or a segment.

(defun create-synapse (synapse-name cell-name cell-element gbar-density v-reversal
				    &key (is-controlled nil) (ca-conc-extra 1.8) (Eca-Nearst-eqn-const 0.04299)
				    pre-synaptic-node-name
				    pre-synaptic-cell-name (impulse (impulse-array)) type type-parameters
				    (light-spatial-rf-parameters '(impulse 0 0)) (light-input-delay 0.0)
				    save-current save-conductance is-light-dependent (plot-pane 1))
  (let ((node-name (typecase cell-element
		     (soma (node-name (soma-node cell-element)))
		     (segment (node-name (segment-node-2 cell-element))))))
    (if (gethash synapse-name synapse-hash-table)
	(sim-warning (format nil "create-synapse: synapse ~a  already defined, ignoring"
			     synapse-name))
	(let* ((n1 (create-node node-name :cell-name cell-name :plot-pane plot-pane))
	       (model (gethash "syn" *model-instance-hash-table*))
	       (syn (make-synapse :name synapse-name :node n1 :model model :cell-element cell-element
				  :light-input-delay light-input-delay
				  :is-light-dependent is-light-dependent
				  :gbar-density (coerce gbar-density 'single-float)
				  :v-reversal (coerce v-reversal 'single-float)
				  :ca-conc-extra (coerce ca-conc-extra 'single-float)
				  :Eca-Nearst-eqn-const (coerce Eca-Nearst-eqn-const 'single-float)
				  :impulse impulse :type type
				  :type-parameters type-parameters :is-controlled is-controlled
				  :light-spatial-rf-parameters light-spatial-rf-parameters)))
	  (setf (node-elements n1) (cons syn (node-elements n1)))
	  (if pre-synaptic-node-name	;This synapse is dependent on the voltage of another node.
	      (progn
		(setf (synapse-channel syn)
		      (create-synapse-channel syn cell-element type
					      pre-synaptic-node-name pre-synaptic-cell-name))
		(setf (synapse-pre-synaptic-node syn)
		      (channel-pre-synaptic-node (synapse-channel syn)))))
	  (setf
	   (gethash synapse-name synapse-hash-table)  syn
	   (model-instance-elements model) (cons syn (model-instance-elements model)))
	  (if save-current
	      (if pre-synaptic-node-name (push synapse-name *plot-channel-currents*)
		  (push synapse-name *plot-synapse-currents*)))
	  (if save-conductance
	      (if pre-synaptic-node-name (push synapse-name *plot-channel-conductances*)
		  (push synapse-name *plot-synapse-conductances*)))
	  syn))))


;;; CREATE-SYNAPSE-CHANNEL This creates the channel that implements a synapse whose conductance depends on the
;;; voltage of pre-synaptic-node-name in pre-synaptic-cell-name.
(defun create-synapse-channel (synapse cell-element type pre-synaptic-node-name  pre-synaptic-cell-name)
  (cond ((eq type 'v-ex-1)
	 (create-v-ex-1-synapse-channel
	   (synapse-name synapse) (cell-name (node-cell (synapse-node synapse))) cell-element
	   pre-synaptic-node-name pre-synaptic-cell-name synapse))))


; This function creates a core synapse data structure. In the parallel version, it 
; puts the device on the same processor as the fanout node, which is passed in as the 
; variable 'proc. The serial version ignores this and allocates a struct normally.
; This function only works if the 'nd argument is not a DC source node, because these
; do not have processors allocated to them.

(defun create-core-synapse (syn nd)
  "Creates the core struct for a synapse."
  (let (core-syn
	(proc #-parallel nil #+parallel (allocate-processor)))
    #-parallel (declare (ignore proc))
    #+parallel (*setf (pref fanout-valid proc) t)
    #+parallel (*setf (pref fanout-seg-forward proc) nil)
    (if (not (or (synapse-is-controlled syn)
		 (synapse-is-light-dependent syn))) ;A node-dependent synapse does not get  a
						    ;core-syn or proc since 
	(return-from create-core-synapse  (values))	;it is actually a channel.
	(progn (if (synapse-core-syn syn)		
		   (setf core-syn (synapse-core-syn syn)) ;core synapse has already been allocated
		   (progn		; else allocate one
		     #-parallel (setf core-syn (make-core-synapse))
		     #+parallel (setf core-syn proc)
		     #+parallel (*setf (pref *lisp-struct-type core-syn) core-synapse)
		     (#+parallel *setf #-parallel setf
;		      (#+parallel pref #.core-synapse-delay core-syn) (synapse-delay syn)
		      (#+parallel pref #.core-synapse-gbar core-syn) (synapse-gbar syn)
		      (#+parallel pref #.core-synapse-v-reversal core-syn) (synapse-v-reversal syn)
		      (#+parallel pref #.core-synapse-ca-conc-extra core-syn) (synapse-ca-conc-extra syn)
		      (#+parallel pref #.core-synapse-Eca-Nearst-eqn-const core-syn)
		      (synapse-Eca-Nearst-eqn-const syn)
		      (#+parallel pref core-synapse-variable-e-ca core-syn) (synapse-variable-e-ca syn)
		      )
		     (setf (synapse-core-syn syn) core-syn)
		     (if (synapse-pre-synaptic-node syn)
			 (#+parallel *setf #-parallel setf
			  (#+parallel pref core-synapse-synapsep core-syn) t))))
    
	       (let ((node1 (synapse-node syn))
		     (node-c-shell (synapse-ca-shell-node syn))
		     (node-pre-syn (synapse-pre-synaptic-node syn)))
		 (cond
		   ((eq nd node1)
		    #-parallel
		    (setf
		      (#.core-synapse-node-pointp core-syn) t
		      (#.core-synapse-node-point core-syn) (node-core-nd node1))
		    #+parallel
		    (*setf
		      (pref #.core-synapse-node-pointp core-syn) t
		      (pref #.core-synapse-node-point core-syn) proc))
		   ((eq nd node-c-shell)
		    #-parallel
		    (setf
		      (#.core-synapse-ca-shell-pointp core-syn) t
		      (#.core-synapse-ca-shell-point core-syn) (node-core-nd node-c-shell))
		    #+parallel
		    (*setf
		      (pref #.core-synapse-ca-shell-pointp core-syn) t
		      (pref #.core-synapse-ca-shell-point core-syn) proc))
		   ((eq nd node-pre-syn)
		    #-parallel
		    (setf
		      (#.core-synapse-pre-synaptic-pointp core-syn) t
		      (#.core-synapse-pre-synaptic-point core-syn) (node-core-nd node-pre-syn))
		    #+parallel
		    (*setf
		      (pref #.core-synapse-pre-synaptic-pointp core-syn) t
		      (pref #.core-synapse-pre-synaptic-point core-syn) proc))
		   (t
		    (sim-error "Internal error: called create-core on a device with a invalid node"))
		   ))))))

(defun add-off-diag-synapse (syn diag off-diag off-diag-entry)
  (declare (ignore syn diag off-diag off-diag-entry)))

(defun find-coupling-synapse (nd syn)
  "Returns a pair of (node . value) telling how coupled that node is to nd."
  (declare (ignore nd syn ))
  nil)

(defun synapse-fix-dc-nodes (syn)
  "Fix up the nodes of this element which are connected to dc nodes."
  (if (synapse-core-syn syn)
      (progn
	(if (node-is-dc-source (synapse-node syn))
	  (#+parallel *setf #-parallel setf
	      (#+parallel pref #.core-synapse-node-pointp (synapse-core-syn syn)) nil
	      (#+parallel pref #.core-synapse-node-const (synapse-core-syn syn))
	      (node-voltage (synapse-node syn))))
	)))

(proclaim '(function get-synapse-voltage (core-synapse) single-float))
#-parallel
(defun get-synapse-voltage (syn)
  (if (#.core-synapse-node-pointp syn)
      (core-node-voltage-n+1 (#.core-synapse-node-point syn))
      (#.core-synapse-node-const syn)))


(proclaim '(function get-synapse-ca-shell-value (core-synapse) single-float))
#-parallel
(defun get-synapse-ca-shell-value (syn)
  (if (#.core-synapse-ca-shell-pointp syn)
      (core-node-voltage-n+1 (#.core-synapse-ca-shell-point syn))
      (#.core-synapse-ca-shell-const syn)))

(proclaim '(function get-synapse-pre-synaptic-value (core-synapse) single-float))
#-parallel
(defun get-synapse-pre-synaptic-value (syn)
  (if (#.core-synapse-pre-synaptic-pointp syn)
      (core-node-voltage-n+1 (#.core-synapse-pre-synaptic-point syn))
      (#.core-synapse-pre-synaptic-const syn)))

(defun all-synapse-info ()
  (maphash 'synapse-info synapse-hash-table))

(defun synapse-info  (name syn)
  "Prints out this data associated with a synapse."
  (format t "Synapse ~a at ~a, cell ~a; type ~a, gbar ~a, v-rev ~a input ~a ~%"
	     name
	     (node-name (synapse-node syn))
	     (cell-name (node-cell (synapse-node syn)))
	     (synapse-type syn)
	     (synapse-gbar syn)
	     (synapse-v-reversal syn)
	     (synapse-waveform syn)
;	     (get-distance-from-stimulus-center syn)
	     ))

;;; EVAL-SYNAPSE
;; Only light-dependent and controlled synapses get evaluated here. Node-dependent synapses do not
;; have a core-syn. The activity of these synapses is evaluated by their associated channels.
#-parallel
(defun eval-synapse (syn)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (without-floating-underflow-traps
   (let  ((voltage 0.0) (conductance 0.0) (v-reversal 0.0) (core-syn (synapse-core-syn syn)))
     (declare (single-float voltage conductance v-reversal))
     (if (null core-syn)  (return-from eval-synapse (values)))
     (if (or *use-synapse-events
	     (and (synapse-waveform syn) (/= (#.core-synapse-gbar core-syn) 0.0)
		  (or 
		   (and *include-light-synapses
			;;(> *real-time *light-stimulus-start-time)(< *real-time *light-stimulus-stop-time)
			)
		   (synapse-is-controlled syn))))
	 (let ((interpolated-waveform-value 0.0)) ;Interpolate synapse waveform between millisecond points.
	   (declare (single-float interpolated-waveform-value))
	   (setq interpolated-waveform-value
		 (if (>= (+ 1 *integer-time (synapse-delay syn))
			 (array-dimension (synapse-waveform syn) 0))
		     (the single-float (aref (synapse-waveform syn) (+ *integer-time (synapse-delay syn))))
		     (+ (the single-float (aref (synapse-waveform syn)
						(+ *integer-time (synapse-delay syn))))
			(* *fractional-time
			   (- (the single-float (aref (synapse-waveform syn)
						      (+ 1 *integer-time  (synapse-delay syn)))))
			   (the single-float (aref (synapse-waveform syn)
						   (+ *integer-time (synapse-delay syn))))))))
	   (if (>= interpolated-waveform-value 1.0e-3)
	       (setf conductance 
		     (* *light-stimulus-strength (the single-float (#.core-synapse-gbar core-syn))
			interpolated-waveform-value)))
	   (setf voltage (if (core-synapse-synapsep core-syn)
			     (get-synapse-pre-synaptic-value core-syn)
			     (get-synapse-voltage core-syn))
		 v-reversal (if (core-synapse-variable-e-ca core-syn)
				(let ((ca-shell (get-synapse-ca-shell-value core-syn)))
				  (if (< ca-shell 1e-20) (setf ca-shell 1e-20))
				  (* (the single-float (#.core-synapse-Eca-Nearst-eqn-const core-syn))
				     *Temperature*
				     (log (/ (the single-float (#.core-synapse-ca-conc-extra core-syn))
					     ca-shell))))
				(the single-float (#.core-synapse-v-reversal core-syn))))
	   (setf (#.core-synapse-conductance core-syn) conductance
		 (#.core-synapse-current core-syn) (* conductance (- (if *use-hines* 0.0 voltage) v-reversal)))
	   (if (#.core-synapse-node-pointp core-syn) ;this should never not occur....
	       (if (not (core-node-is-source (#.core-synapse-node-point core-syn))) ;this should never occur....
		   (setf
		    (core-node-current (#.core-synapse-node-point core-syn))
		    (+ (core-node-current (#.core-synapse-node-point core-syn))
		       (#.core-synapse-current core-syn))
		    (core-node-jacobian (#.core-synapse-node-point core-syn))
		    (+ (core-node-jacobian (#.core-synapse-node-point core-syn))
		       conductance))))
	   (if (and *use-hines* (synapse-plot-current syn))
	       ;; We need the actual current for the saved data.
	       (setf (#.core-synapse-current core-syn) (* conductance (- voltage v-reversal)))))))))

#+parallel
(*defun get-synapse-voltage (v1)
  (cond!!
    (#.core-synapse-node-pointp v1)
    (t!! #.core-synapse-node-const)))

#+parallel
(*defun get-synapse-ca-shell-value (v1)
  (cond!!
    (#.core-synapse-ca-shell-pointp v1)
    (t!! #.core-synapse-ca-shell-const)))

#+parallel
(*defun get-synapse-pre-synaptic-value (v1)
  (cond!!
    (#.core-synapse-pre-synaptic-pointp v1)
    (t!! #.core-synapse-pre-synaptic-const)))

#+parallel
(*defun syn-power!! (a b)
  (declare (type (pvar big-float) a))
  (declare (type (pvar (unsigned-byte 8)) b))
  (*let ((res (!! 1))
	 (index b))
    (declare (type (pvar big-float) res))
    (declare (type (pvar (unsigned-byte 8)) index))
	(do ()
	    ((not (*or (>!! index (!! 0)))))
	    (*when (>!! index (!! 0))
		   (*set res (*!! res a))
		   (*set index (-!! index (!! 1)))))
	res))


;;;; fix this
#+parallel
(defun eval-synapse ()
  (let ((adjusted-time (if (= *real-time *duration)(round (1- *duration)) (floor *real-time))))
  (*select-type (core-synapse)
    (*let
      ((voltage nil)
       (ca-shell nil)
       (v-reversal nil)
       (conductance nil))
      (declare (type (pvar big-float) voltage))
      (declare (type (pvar big-float) ca-shell))
      (declare (type (pvar big-float) v-reversal))
      (declare (type (pvar big-float) conductance))
      (*if core-synapse-synapsep
	   (*set voltage (get-synapse-pre-synaptic-value #.core-synapse-pre-synaptic-value))
	   (*set voltage (get-synapse-voltage #.core-synapse-node-voltage)))
      (if (and *include-light-synapses (> *real-time *light-stimulus-start-time)(< *real-time *light-stimulus-stop-time)
	       (or (eq *light-stimulus 'moving-bar)(eq *light-stimulus 'reversing-bar)))
		   (*set conductance (* *light-stimulus-strength (#.core-synapse-gbar core-syn)
					    (aref   (synapse-waveform syn) adjusted-time)))))
						;(get-moving-bar-input syn core-syn time)))
		 (*set conductance 0.0)))

      (*set #.core-synapse-current
	    (*!! conductance (-!! voltage v-reversal)))
      (*set #.core-synapse-conductance conductance)
;;      (format t "voltage ~a  current ~a~%" (pref voltage 6) (pref #.core-synapse-current 6))
      )

(defun get-random-synapse-event-times ())

;;; SETUP-LIGHT-SYNAPSES Assigns input waveforms to each of the light-dependent synapses.
(defvar *synapse-names-to-do-first nil)		;This is useful for *fast-rf-bar simulations in order to first
						;compute synapses with the longest responses.
(defun setup-light-synapses ()
  ;; do this here now - find a better place later
  (setq *cos-theta (cos *light-theta) *sin-theta (sin *light-theta))
  (cond
    ((and *include-synapses *use-synapse-events)
     (get-random-synapse-event-times)
     (maphash 'setup-synapse synapse-hash-table))
    ((and *include-synapses *include-light-synapses)
     (if *use-old-synapse-waveforms
	 (dolist (synapse-waveform-list *synapse-waveforms)
	   (if  (gethash (car synapse-waveform-list) synapse-hash-table)
		(setf (synapse-waveform (gethash (car synapse-waveform-list) synapse-hash-table))
		      (cadr synapse-waveform-list))))
	 (progn (setq *synapse-waveforms '()
		      *input-waveform-array (make-array (round user-stop-time))	;A temporary location
		      *light-origin-array (make-array (round user-stop-time))
		      *spatial-rf-arrays-list '() *synapse-waveforms '())
		(generate-light-origin-trajectory)
					;First compute synapses with the longest responses. 
		(loop for synapse-name in *synapse-names-to-do-first do	
		      (if (gethash synapse-name synapse-hash-table)
			  (setup-synapse synapse-name (gethash synapse-name synapse-hash-table))))
					;Now do the rest.
		(maphash 'setup-synapse synapse-hash-table))))))


;; GENERATE-LIGHT-ORIGIN-TRAJECTORY Generates the trajectory of the light stimulus
;; origin during the simulation, as a list of xy points.
(defun generate-light-origin-trajectory ()
  (do ((time 0.0 (+ 1.0 time)))
      ((>= time user-stop-time))
    (declare (single-float time))
    (let ((moving-time 0.0)(reverse-time 0.0)(x 0.0)(y 0.0))
      (declare (single-float moving-time reverse-time x y))
      (setq moving-time
	    (cond ((> time *motion-stop-time)
		   (- *motion-stop-time *motion-start-time))
		  ((and (> time *motion-start-time)(< time *motion-stop-time))
		   (- time *motion-start-time))
		  (t 0.0)))
      (setq reverse-time
	    (if (eq *light-stimulus 'reversing-bar)
		(if (< (mod moving-time (* 2.0 *grating-temporal-period)) *grating-temporal-period)
		    (mod moving-time (* 2.0 *grating-temporal-period))
		    (- (* 2.0 *grating-temporal-period) (mod moving-time (* 2.0 *grating-temporal-period))))
		moving-time))
      (setq x (* (if *light-direction  (- *light-speed) *light-speed)
		 reverse-time
		 (cos (- pi-over-2 *light-theta))))
      (setq y (* (if *light-direction *light-speed (- *light-speed))
		 reverse-time
		 )			; (sin (- (/ pi-single 2.0) *light-theta))
	    )
      (setf (aref *light-origin-array (truncate time))
	  (list 0.0			; x
		y)))))



;;;(setq *SYNAPSE-EVENT-WAVEFORM
(defvar *event-driven-synapse-waveform-in ;  (alpha-array 1 50 1)
  )
(defvar *event-driven-synapse-waveform-ex  ; (alpha-array 1 10 1)
  )


;;; SETUP-SYNAPSE Pre-computes the conductance waveform for light-dependent and controlled synapses. Note that
;;; light-dependent synapses which are never hit by the light stimulus have their synapse-waveform set to NIL.
;;; All time indices are taken in milliseconds.
(defun setup-synapse (name syn)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (display-message  (format nil "Setting up ~a" name))
  (cond
    (*use-synapse-events
     (cond
       ((eq (synapse-type syn) 'inhibitory)
	(setf (synapse-waveform syn) *event-driven-synapse-waveform-in))
       ((eq (synapse-type syn) 'EXCITATORY)
	(setf (synapse-waveform syn) *event-driven-synapse-waveform-ex))))
    ((or (eq (synapse-type syn) 'cont-inhibitory)
	 (eq (synapse-type syn) 'cont-excitatory))
     (make-synapse-conductance-alpha-waveform syn))
    ((and (synapse-is-light-dependent syn)
	  (or (> (the single-float (synapse-gbar-density syn)) 0.0)
	      (> (the single-float (synapse-gbar syn)) 0.0))
	  (setf (synapse-is-within-aperture syn) (test-for-aperture syn)))
     (let ((previously-computed-synapse-of-same-type
	    (if (or (and (equal *light-stimulus 'spot) *fast-full-field-spot)
		    (and (equal *light-stimulus 'moving-bar) *fast-rf-bar
			 (close-to pi-over-2 *light-theta)))
		(dolist (synapse-waveform-list *synapse-waveforms)
		  (if (equal (synapse-type (gethash (car synapse-waveform-list) synapse-hash-table))
			     (synapse-type syn))
		      (return (gethash (car synapse-waveform-list) synapse-hash-table)))))))
       (cond ((and (equal *light-stimulus 'spot) previously-computed-synapse-of-same-type)
	      (setf (synapse-waveform syn) (synapse-waveform previously-computed-synapse-of-same-type)))
	     ((and (equal *light-stimulus 'moving-bar) *fast-rf-bar previously-computed-synapse-of-same-type)
	      (setf (synapse-waveform syn)
		    (shift-synapse-waveform syn previously-computed-synapse-of-same-type)))
	     (t (if (not (generate-light-input-array syn))
		    (setf (synapse-waveform syn) nil) ;If no light here, then nil waveform
		    (let ((sum 0.0))	;Else, compute waveform
		      (declare (single-float sum))
		      (setf (synapse-waveform syn)
			    (make-array (round user-stop-time) :element-type 'single-float))
		      ;;Convolve input-waveform with synapse impulse response to get conductance waveform.
		      (display-message (format nil "Convolving conductance waveform for ~a" name))
		      (dotimes (i (round user-stop-time))	
			(setq sum 0.0)
			(dotimes (j (1+ i))
			  (declare (fixnum i j))
			  (if (< (- i j) (array-dimension (synapse-impulse syn) 0))
			      (setq sum
				    (+ sum (* (the single-float (aref *input-waveform-array j))
					      (the single-float (aref (synapse-impulse syn) (- i j))))))))
			(setf (aref (synapse-waveform syn) i) (max 0.0 sum)))))))))) ;Threshold waveform 
  ;;Store conductance waveform for later simulation.
  (if (synapse-waveform syn)
      (setq *synapse-waveforms
	    (nconc *synapse-waveforms (list (list name (synapse-waveform syn)))))))



(defun make-synapse-conductance-alpha-waveform (synapse)
  (let ((delay (nth 1 (synapse-type-parameters synapse)))
	(tau (nth 0 (synapse-type-parameters synapse))))
    (setf (synapse-waveform synapse) (make-array (floor user-stop-time)))
    (dotimes (time user-stop-time)
      (declare (single-float delay tau))
      (setf (aref (synapse-waveform synapse) time)
	    (if (< time delay) 0.0 (alpha-function tau 1 (- time delay)))))))


;;; SHIFT-SYNAPSE-WAVEFORM If a light synapse is identical to one whose input has been previously computed, and
;;; the stimulus is the same, just shifted in time, then we can quickly compute the input to the new synapse.
(defun shift-synapse-waveform (new-syn old-syn)
  (let* ((delta-x (- (first (node-absolute-location (synapse-node new-syn)))
		     (first (node-absolute-location (synapse-node old-syn)))))
	 (delta-t (round (/ (* (if *light-direction -1 1) delta-x) *light-speed)))
	 (new-waveform (make-array (round user-stop-time))))
    (declare (single-float delta-x))
    (dotimes (time (round user-stop-time))
      (if (>= delta-t 0.0)
	  (setf (aref new-waveform time) (if (>= time delta-t)
					  (aref (synapse-waveform old-syn) (- time delta-t))
					  0.0))
	  (setf (aref new-waveform time) (if (< (- time delta-t) (round user-stop-time))
					  (aref (synapse-waveform old-syn) (-  time delta-t))
					  0.0))))
    new-waveform))

;;; TEST-FOR-APERTURE If *use-aperture AND synapse is not within aperture, returns NIL, else T.
(defun test-for-aperture (syn)
  (if *use-aperture
      (let ((light-input-offset-distance
	     (if (synapse-light-spatial-rf-parameters syn)
		 (second (synapse-light-spatial-rf-parameters syn))
		 0.0))
	    (light-input-offset-angle
	     (if (synapse-light-spatial-rf-parameters syn)
		 (third (synapse-light-spatial-rf-parameters syn))
		 0.0)))
	(declare (single-float light-input-offset-distance light-input-offset-angle))

	(< (distance (+ (first (node-absolute-location (synapse-node syn)))
			(* light-input-offset-distance (cos light-input-offset-angle)))
		     (+ (second (node-absolute-location (synapse-node syn)))
			(* light-input-offset-distance (sin light-input-offset-angle)))
		     *aperture-center-x *aperture-center-y)
	   *aperture-radius))
      T))


;;; GET-SPATIAL-RF-ARRAY Returns spatial RF array for the synapse.
;;; The list 'light-spatial-rf-parameters is composed of the spatial impulse function and parameters for
;;; that function. For example:

;;          '(impulse light-input-offset-distance light-input-offset-angle)

;;   or

;;;         '(gaussian light-input-offset-distance light-input-offset-angle sigma-x sigma-y normalized-amp)

;;; The spatial RF array is 5 * sigma wide, and the grid size is 20 by 20 microns. Array values are scaled by
;;; area of grid (400).



(defun get-spatial-rf-array (syn)
  (let ((spatial-rf-type (first (synapse-light-spatial-rf-parameters syn)))
	(spatial-rf-array
	 (dolist (spatial-rf-array-list *spatial-rf-arrays-list)
	   (if (equal (car spatial-rf-array-list) (synapse-light-spatial-rf-parameters syn))
	       (return (cadr spatial-rf-array-list))))))
    (if (not spatial-rf-array)
	(if (not spatial-rf-type)
	    (setq spatial-rf-array (make-array '(1 1) :initial-contents '((1))))
	    (case spatial-rf-type
	      (impulse
	       (setq spatial-rf-array (make-array '(1 1) :initial-contents '((1)))))
	      (gaussian
	       (let* ((fast-y-flag (and (or (equal *light-stimulus 'moving-bar)
					    (equal *light-stimulus 'apparent-motion)) *fast-rf-bar))
		      (sigma-x (fourth (synapse-light-spatial-rf-parameters syn)))
		      (sigma-y (fifth (synapse-light-spatial-rf-parameters syn)))
		      ;;Array will be 5 * sigma wide, with a 10*10 uM grid
		      (gaussian-x-size (floor (+ 1 (/ (* 5 sigma-x) 10)))) ;We want odd number of elements
		      (gaussian-y-size (if fast-y-flag 
					   1 (floor (+ 1 (/ (* 5 sigma-y) 10)))))
		      (gaussian-x-normalizer (if (sixth (synapse-light-spatial-rf-parameters syn))
						 (/ 1 (gaussian 0.0 0.0 (* sigma-x sigma-x)))
						 1.0))
		      (gaussian-y-normalizer (if (sixth (synapse-light-spatial-rf-parameters syn))
						 (/ 1 (gaussian 0.0 0.0 (* sigma-y sigma-y)))
						 1.0)))
		 (declare (single-float sigma-x sigma-y gaussian-x-normalizer gaussian-y-normalizer))
		 (declare (fixnum gaussian-x-size gaussian-y-size))
		 (if (or (= sigma-x 0.0)(= sigma-y 0.0))
		     (sim-error (format nil "Cannot have zero sigma for synapse ~a" (synapse-name syn))))
		 (setq spatial-rf-array (make-array (list gaussian-x-size gaussian-y-size)))
		 (dotimes (x gaussian-x-size)
		   (dotimes (y gaussian-y-size)
		     (declare (fixnum x y))
		     (let* ((gauss-x (gaussian
				      (* 10 (- x (* 0.5 (- gaussian-x-size 1))))
				      0.0 (* sigma-x sigma-x)))
			    (gauss-y (gaussian
				      (* 10 (- y (* 0.5 (- gaussian-y-size 1))))
				      0.0 (* sigma-y sigma-y)))
			    (gauss-xy (* 10 (if fast-y-flag 1.0 10)
					 (if (> (* gaussian-x-normalizer gauss-x) 0.00001)
					     (* gaussian-x-normalizer gauss-x) 0)
					 (if fast-y-flag 1.0
					     (if (> (* gaussian-y-normalizer gauss-y) 0.00001)
						 (* gaussian-y-normalizer gauss-y) 0)
					     ))))
		       (declare (single-float gauss-x gauss-y gauss-xy))
		       ;;Make the gaussian somewhat circularly symmetric, despite the finite array.
		       (setf (aref spatial-rf-array x y) (if (> gauss-xy .0001) gauss-xy 0)))))))	
					;Store spatial-rf-array.
	      (setq *spatial-rf-arrays-list (cons (list (synapse-light-spatial-rf-parameters syn)
							spatial-rf-array)
						  *spatial-rf-arrays-list))))
	spatial-rf-array)))

(defun array-vol (array grid)
  (let ((volume 0.0))
    (declare (single-float volume))
    (dotimes (i (array-dimension array 0))
      (dotimes (j (array-dimension array 1))
	(setq volume (+ volume (* (aref array i j) grid grid)))))
    volume))

(defun gaussian (x mean variance)
  (declare (single-float x mean variance))
    (without-floating-underflow-traps
      (* (/ 1.0 (expt (* 2.0 3.14156 variance ) 0.5))
	 (exp (- (float (/ (* (- x mean) (- x mean)) (* 2.0 variance))))))))


;;; GENERATE-LIGHT-INPUT-ARRAY Returns nil if *input-waveform-array is
;;; all zeros, ie the light never hits this synapse, else fills
;;; *input-waveform-array with the light input over time for the
;;; light-dependent synapse syn.
(defun generate-light-input-array (syn)
  (let* ((spatial-rf-array (get-spatial-rf-array syn))
	 (spatial-rf-x-size (if spatial-rf-array
				(array-dimension spatial-rf-array 0)
				1))
	 (spatial-rf-y-size (if spatial-rf-array
				(array-dimension spatial-rf-array 1)
				1))
	 (light-input-offset-distance (if (synapse-light-spatial-rf-parameters syn)
					  (second (synapse-light-spatial-rf-parameters syn)) 0.0))
	 (light-input-offset-angle (if (synapse-light-spatial-rf-parameters syn)
				       (third (synapse-light-spatial-rf-parameters syn)) 0.0))
	 (x-synapse (+ (first (node-absolute-location (synapse-node syn)))
		       (* light-input-offset-distance (cos light-input-offset-angle))))
	 (y-synapse (+ (second (node-absolute-location (synapse-node syn)))
		       (* light-input-offset-distance (sin light-input-offset-angle)))))
    (declare (fixnum spatial-rf-x-size spatial-rf-y-size))
    (declare (single-float light-input-offset-distance light-input-offset-angle
			   x-synapse y-synapse))
    (dotimes (time (round user-stop-time)) ;Generate array telling when stimulus is present
      (declare (fixnum time))
      (setf (aref *input-waveform-array time) 0.0) ;Start at 0.
      (let ((delayed-time (floor (- time  (synapse-light-input-delay syn)))))
	(declare (fixnum delayed-time))
	(if (and (>= delayed-time *light-stimulus-start-time)
		 (< delayed-time *light-stimulus-stop-time)) ;If before synapse delay then nothing
	    ;;Else, first locate stimulus origin
	    (let ((x-light-origin (first (aref *light-origin-array delayed-time)))
		  (y-light-origin (second (aref *light-origin-array delayed-time)))
		  (scaled-x-rf-offset (* -10.0 (/ (- spatial-rf-x-size 1.0) 2.0)))
		  (scaled-y-rf-offset (* -10.0 (/ (- spatial-rf-y-size 1.0) 2.0))))
	      (declare (single-float x-light-origin y-light-origin scaled-x-rf-offset scaled-y-rf-offset))
	      ;;Now integrate over the synapse's spatial RF. The
	      ;;spatial-rf array has a sigma-x*sigma-y uM grid size,
	      ;;while the synapse and light origin coordinates are in
	      ;;units of 1uM.
	      (let ((x-synapse-rf (+ x-synapse scaled-x-rf-offset)) ;Initialize x.
		    (y-synapse-rf 1.0))
		(declare (single-float x-synapse-rf y-synapse-rf))
		(dotimes (x-rf-index spatial-rf-x-size)
		  (declare (fixnum x-rf-index))
		  (setq  y-synapse-rf (+ y-synapse scaled-y-rf-offset) ;Initialize y.
			 x-synapse-rf (+ x-synapse-rf (* 10 x-rf-index)))
		  (dotimes (y-rf-index spatial-rf-y-size)
		    (declare (fixnum y-rf-index))
		    (setq y-synapse-rf (+ y-synapse-rf (* 10 y-rf-index)))
		    (setf (aref *input-waveform-array time)
			  (+ (aref *input-waveform-array time)
			     (* (aref spatial-rf-array x-rf-index y-rf-index) ;Weighting of spatial RF.
				(case *light-stimulus 
				  (reversing-bar
				   ())
				  (apparent-motion
				   (apparent-motion-stimulus x-synapse-rf y-synapse-rf delayed-time))
				  (moving-bar
				   (bar-stimulus x-synapse-rf y-synapse-rf x-light-origin y-light-origin))
				  (moving-bar-grating
				   (bar-grating-stimulus x-synapse-rf y-synapse-rf
							 x-light-origin y-light-origin))
				  (moving-sine-grating
				   (sine-grating-stimulus x-synapse-rf y-synapse-rf
							  x-light-origin y-light-origin))
				  (moving-spot
				   (moving-spot-stimulus x-synapse-rf y-synapse-rf
							 x-light-origin y-light-origin))
				  (spot
				   (spot-stimulus x-synapse-rf y-synapse-rf))
				  (annulus
				   (annulus-stimulus x-synapse-rf y-synapse-rf))))))))))))))
  (array-not-all-zerop *input-waveform-array))

(defun array-not-all-zerop (array)
  (loop for i from 0 to (- (array-dimension array 0) 1)
	do
	(if (/= 0 (aref array i)) (return t))
	finally (return nil)))

;; SPOT-STIMULUS
(defun spot-stimulus (x-synapse y-synapse)
  (declare (single-float x-synapse y-synapse))
  (if (> (/ *spot-outside-diameter 2.0)
	 (distance x-synapse y-synapse *light-start-position-x *light-start-position-y))
      1.0 0.0))

;;; APPARENT-MOTION-STIMULUS
(defun apparent-motion-stimulus (x-synapse y-synapse delayed-time)
  (declare (single-float x-synapse y-synapse))
  (+ (if (and (> delayed-time *bar-a-start-time) (< delayed-time *bar-a-stop-time))
	 (let* ((x-shifted (- x-synapse  *bar-a-position-x))
		(y-shifted (- y-synapse *bar-a-position-y))
		(x-shifted-rotated (+ (* x-shifted (if (= *light-theta-degrees 90) 0 (cos *light-theta)))
				      (* y-shifted (if (= *light-theta-degrees 90) 1 (sin *light-theta))))) 
		(y-shifted-rotated (- (* y-shifted (if (= *light-theta-degrees 90) 0 (cos *light-theta)))
				      (* x-shifted (if (= *light-theta-degrees 90) 1 (sin *light-theta))))))
	   (declare (single-float x-shifted y-shifted x-shifted-rotated y-shifted-rotated))
	   (if (and (< (abs  x-shifted-rotated) (/ *bar-a-length 2))
		    (< (abs y-shifted-rotated) (/ *bar-a-width 2)))
	       *bar-a-intensity 0.0))
	 0.0)
     (if (and (> delayed-time *bar-b-start-time) (< delayed-time *bar-b-stop-time))
	 (let* ((x-shifted (- x-synapse  *bar-b-position-x))
		(y-shifted (- y-synapse *bar-b-position-y))
		(x-shifted-rotated (+ (* x-shifted (if (= *light-theta-degrees 90) 0 (cos *light-theta)))
				      (* y-shifted (if (= *light-theta-degrees 90) 1 (sin *light-theta)))))
		(y-shifted-rotated (- (* y-shifted (if (= *light-theta-degrees 90) 0 (cos *light-theta)))
				      (* x-shifted (if (= *light-theta-degrees 90) 1 (sin *light-theta))))))
	   (declare (single-float x-shifted y-shifted x-shifted-rotated y-shifted-rotated))
  	   (if (and (< (abs x-shifted-rotated) (/ *bar-b-length 2))
		    (< (abs y-shifted-rotated) (/ *bar-b-width 2)))
	       *bar-b-intensity 0.0))
	 0.0)))


;;; MOVING-SPOT-STIMULUS
(defun moving-spot-stimulus (x-synapse y-synapse x-light-origin y-light-origin)
  (declare (single-float x-synapse y-synapse x-light-origin y-light-origin))
  (if (> (/  *spot-outside-diameter 2.0)
	  (distance x-synapse y-synapse
		    (+ *light-start-position-x x-light-origin)
		    (+ *light-start-position-y y-light-origin)))
       1.0 0.0))

;; ANNULUS-STIMULUS
(defun annulus-stimulus (x-synapse y-synapse)
  (declare (single-float x-synapse y-synapse))
  (let ((distance-from-stimulus
	  (distance x-synapse y-synapse *light-start-position-x *light-start-position-y)))
    (declare (single-float distance-from-stimulus))
    (if (and (> (/ *spot-outside-diameter 2.0)
		distance-from-stimulus)
	     (< (/ *spot-inside-diameter 2.0)
		distance-from-stimulus))
	1.0 0.0)))

;;; DISTANCE Returns the Euclidean distance between the two points (x-1,y-1) and (x-2,y-2).
(defun distance (x-1 y-1 x-2 y-2)
  (sqrt (+ (square (- (coerce x-1 'single-float) (coerce x-2 'single-float)))
	   (square (- (coerce y-1 'single-float) (coerce y-2 'single-float))))))



;;; BAR-STIMULUS This determines whether the moving bar is presently
;;; overhead the syn. Note that this function first translates the syn
;;; coordinates according to the bar original position, and then
;;; rotates the coordinates according to the tilt of the rotated bar
;;; (the bar frame).  Coordinates are then translated according to the
;;; bar's movement from its original position (in the bar frame's
;;; y-direction).
(defun bar-stimulus (x-synapse y-synapse x-light-origin y-light-origin)
  (declare (single-float x-synapse y-synapse x-light-origin y-light-origin))
  (let* ((x-shifted (- x-synapse *light-start-position-x))
	 (y-shifted (- y-synapse *light-start-position-y))
	 (x-shifted-rotated (+ (* x-shifted *cos-theta)
			       (* y-shifted *sin-theta)))
	 (y-shifted-rotated (- (* y-shifted *cos-theta)
			       (* x-shifted *sin-theta))))
    (declare (single-float x-shifted y-shifted  x-shifted-rotated y-shifted-rotated))
    (if (and (< (abs  (- x-shifted-rotated x-light-origin)) (/ *bar-length 2.0))
	     (< (abs (- y-shifted-rotated y-light-origin)) (/ *bar-width 2.0)))
	1.0 0.0)))

;;; BAR-GRATING-STIMULUS This determines whether the moving bar grating is presently overhead the syn. Note that this
;;; function first translates the syn coordinates according to the bar original position, and then rotates the
;;; coordinates according to the tilt of the rotated bar (the bar frame).  Coordinates are then translated
;;; according to the bar's movement from its original position (in the bar frame's y-direction).
(defun bar-grating-stimulus (x-synapse y-synapse x-light-origin y-light-origin)
  (declare (single-float x-synapse y-synapse x-light-origin y-light-origin))
  (let* ((x-shifted (- x-synapse (+ *light-start-position-x x-light-origin)))
	 (y-shifted (- y-synapse (+ *light-start-position-y y-light-origin)))
	 (x-shifted-rotated (+ (* x-shifted (if (= *light-theta-degrees 90.0) 0.0 (cos *light-theta)))
			       (* y-shifted (if (= *light-theta-degrees 90.0) 1.0 (sin *light-theta)))))
	 (y-shifted-rotated (- (* y-shifted (if (= *light-theta-degrees 90.0) 0.0 (cos *light-theta)))
			       (* x-shifted (if (= *light-theta-degrees 90.0) 1.0 (sin *light-theta))))))
    (declare (single-float x-shifted y-shifted x-shifted-rotated y-shifted-rotated))
    (if (and (or (>  (abs (rem y-shifted-rotated *grating-spatial-period))
		     (+ *grating-spatial-period (/ *bar-width -2.0)))
		 (< (abs (rem y-shifted-rotated *grating-spatial-period))
		    (/ *bar-width 2)))
	     (< (abs x-shifted-rotated) (/ *bar-length 2.0)))
	1.0 0.0)))



;;; SINE-GRATING-STIMULUS This determines whether the moving sine grating is presently overhead the syn. Note
;;; that this function first translates the syn coordinates according to the bar original position, and then
;;; rotates the coordinates according to the tilt of the rotated bar (the bar frame).  Coordinates are then
;;; translated according to the bar's movement from its original position (in the bar frame's y-direction).
(defun sine-grating-stimulus (x-synapse y-synapse x-light-origin y-light-origin)
  (declare (single-float x-synapse y-synapse x-light-origin y-light-origin))
  (let* ((x-shifted (- x-synapse (+ *light-start-position-x x-light-origin)))
	 (y-shifted (- y-synapse (+ *light-start-position-y y-light-origin)))
	 (y-shifted-rotated (- (* y-shifted (if (= *light-theta-degrees 90.0) 0.0 (cos *light-theta)))
			       (* x-shifted (if (= *light-theta-degrees 90.0) 1.0 (sin *light-theta))))))
    (declare (single-float x-shifted y-shifted  y-shifted-rotated))
    (sin (* 2.0 pi-single (/ y-shifted-rotated *grating-spatial-period)))))



;;; ********* CREATE SYNAPSE TYPE FUNCTIONS ********

(defstruct synapse-type-template
  "The template for the synapse types"
  (name "" 		:type string)
  (default-params () 	:type list)
  (instances ()		:type list)
  (impulse)
  (impulse-function)
  (light-spatial-rf-parameters '()	:type list)
  (g-dens 0.0 :type single-float)
  (e-rev 0.0 :type single-float)
  (is-light-dependent nil :type boolean)
  (is-controlled nil :type boolean))

;;;  CREATE-SYNAPSE-TYPE
(defun create-synapse-type (cell-element synapse-type)
  (let ((template)
	(node (typecase cell-element
		(soma  (soma-node cell-element))
		(segment   (segment-node-2 cell-element)))))
    (if (not (setq template (gethash (string synapse-type) *synapse-type-model-hash-table*)))
	(setq template (create-synapse-type-model synapse-type)))
    (create-synapse (concatenate 'string  "Syn-" (string (node-name node)) "-" (string synapse-type))
		    (cell-name (node-cell node))
		    cell-element
		    (coerce (synapse-type-template-g-dens template) 'single-float)
		    (coerce (synapse-type-template-e-rev template) 'single-float)
		    :light-spatial-rf-parameters
		    (synapse-type-template-light-spatial-rf-parameters template)
		    :impulse 
		    (synapse-type-template-impulse template) 
		    :type synapse-type
		    :is-light-dependent
		    (synapse-type-template-is-light-dependent template)
		    :is-controlled
		    (synapse-type-template-is-controlled template))))

;;; CREATE-SYNAPSE-TYPE-MODEL
;(defun create-synapse-type-model (synapse-type)
;  (case synapse-type
;    (cont-ex  (create-controlled-ex-synapse-model))
;    (cont-ex-ds  (create-controlled-ex-ds-synapse-model))
;    (cont-in  (create-controlled-in-synapse-model))
;    (excitatory-1 (create-excitatory-1-synapse-model))
;    (excitatory-2 (create-excitatory-2-synapse-model))
;    (ex-3 (create-ex-3-synapse-model))
;    (in-3 (create-in-3-synapse-model))
;    (inhibitory-1 (create-inhibitory-1-synapse-model))
;    (inhibitory-1-offset (create-inhibitory-1-offset-synapse-model))
;    (excitatory-facilitation (create-excitatory-facilitation-synapse-model))
;    (excitatory-trans (create-excitatory-trans-synapse-model))))


(defvar *ex-synapse-impulse-function '(double-alpha 200 10 60 1))
(defvar *in-synapse-impulse-function '(double-alpha 200 100 30 0))


;; CREATE-SYNAPSE-TYPE-MODEL
;;; The format for the type-parameters list is (name g-dens e-rev is-light-dependent impulse-function).
(defun create-synapse-type-model (synapse-type)
  (let ((template (make-synapse-type-template))
	(type-parameters
	 (case synapse-type
	   (cont-ex)
	   (cont-ex-ds)
	   (cont-in  )
	   (excitatory-1 (list 'excitatory-1 *g-excitatory-1-dens *e-rev-excitatory-1 t
			       *ex-synapse-impulse-function))
	   (ex-1 (list 'excitatory-1 *g-excitatory-1-dens *e-rev-excitatory-1 t
		       *ex-synapse-impulse-function))
	   (excitatory-2)
	   (ex-3)
	   (in-3)
	   (inhibitory-1 (list 'inhibitory-1 *g-inhibitory-1-dens *e-rev-inhibitory-1 t
			       *in-synapse-impulse-function))
	   (inhibitory (list 'inhibitory *g-inhibitory-1-dens *e-rev-inhibitory-1 t
			       *in-synapse-impulse-function))
	   (in-1 (list 'inhibitory-1 *g-inhibitory-1-dens *e-rev-inhibitory-1 t
		       *in-synapse-impulse-function))
	   (inhibitory-1-offset
	    (list 'inhibitory-1-offset *g-inhibitory-1-dens *e-rev-inhibitory-1-offset t '(alpha-array 300 50 1)))
	   (excitatory-facilitation
	    (list 'excitatory-facilitation *g-ex-fac-dens *e-rev-ex-fac t '(double-alpha 250 10 40 .9)))
	   (excitatory
	    (list 'excitatory *g-ex-fac-dens *e-rev-ex-fac t '(double-alpha 250 10 40 .9)))
	   (excitatory-trans ))))
    (setf
     (synapse-type-template-name template) (string (nth 0 type-parameters))
     (synapse-type-template-g-dens template) (coerce (nth 1 type-parameters) 'single-float)
     (synapse-type-template-e-rev template) (coerce (nth 2 type-parameters) 'single-float)
     (synapse-type-template-is-light-dependent template) (nth 3 type-parameters)
     (synapse-type-template-impulse-function template) (nth 4 type-parameters))
    (setf
     (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template))
     *synapse-type-models* (cons template *synapse-type-models*)
     (gethash (synapse-type-template-name template) *synapse-type-model-hash-table*) template)
    template))



(defun print-synapse-type-parameters (name template)
    (format *output-stream "Synape Type ~a:~%  Impulse ~a~%  Spatial RF params ~a~%  g-dens ~a, E-rev ~a~%"
	    name
	    (synapse-type-template-impulse-function template)
	    (synapse-type-template-light-spatial-rf-parameters template)
	    (synapse-type-template-g-dens template)
	    (synapse-type-template-e-rev template))
    (if (synapse-type-template-default-params template)
	(format *output-stream "   Default params ~a~%"
		(synapse-type-template-default-params template))))



(defun create-synapses-from-name-list (list type)
    (dolist (name list)
      (let ((location
	     (if (gethash name segment-hash-table)(gethash name segment-hash-table)(gethash name soma-hash-table))))
	(if location (create-synapse-type location type)))))

;;;        
(defvar *ex-3-sigma-x 20)		;Sigma (X) of synaptic spatial receptive field gaussian of type "EX-3" (microns) 
(defvar *ex-3-sigma-y 20)		;Sigma (Y) of synaptic spatial receptive field gaussian of type "EX-3" (microns)
(defvar *in-3-sigma-x 20)		;Sigma (X) of synaptic spatial receptive field gaussian of type "IN-3" (microns) 
(defvar *in-3-sigma-y 20)		;Sigma (Y) of synaptic spatial receptive field gaussian of type "IN-3" (microns)
(defvar *syn-rf-normalized-amp nil)	;If T then synaptic spatial receptive field gaussian amplitude is
					;normalized, else area (volume) is normalized.
(defvar *in-3-delay 0)		;Delay for synapse type "IN-3" (milliseconds)


;;; Format for gaussian spatial receptive field parameter list.
;;;         '(gaussian light-input-offset-distance light-input-offset-angle sigma-x sigma-y normalized-amp)



(defvars *in-tau			;for marchiafava 50 (ms)
      *in-delay
    *ex-delay)

(defvar *g-cont-in-dens 1)		;for marchiafava 1 (ps/sq-um)

;;;  CREATE-CONTROLLED-IN-SYNAPSE
					;(defun create-controlled-in-synapse (cell-element)
					;  (let* ((node (typecase cell-element
					;		 (soma  (soma-node cell-element))
					;		 (segment   (segment-node-2 cell-element))))
					;	 (synapse (create-synapse (format nil "Syn-~a-cont-in" (node-name node))
					;				  (cell-name (node-cell node))
					;				  cell-element
					;				  *g-cont-in-dens
					;				  *e-rev-inhibitory-1
					;				  :type 'cont-inhibitory :type-parameters (list *in-tau *in-delay)
					;				  :is-controlled t)))
					;    (make-synapse-conductance-alpha-waveform synapse)))

(defun create-controlled-in-synapse-model ()
    (let ((template (make-synapse-type-template)))
      (setf
       (synapse-type-template-name template) (string 'cont-inhibitory)
       (synapse-type-template-default-params template) (list *in-tau *in-delay)
       (synapse-type-template-is-controlled template) t
       *synapse-type-models* (cons template *synapse-type-models*)
       (gethash (string 'cont-inhibitory) *synapse-type-model-hash-table*) template)
      template))

(defvar *g-cont-ex-dens 0.1)		;for marchiafava 
(defvar *ex-tau 5)			;for marchiafava 

;;;;  CREATE-CONTROLLED-EX-SYNAPSE
					;(defun create-controlled-ex-synapse (cell-element)
					;  (let* ((node (typecase cell-element
					;		 (soma  (soma-node cell-element))
					;		 (segment   (segment-node-2 cell-element))))
					;	 (synapse (create-synapse (format nil "Syn-~a-cont-ex" (node-name node))
					;				  (cell-name (node-cell node))
					;				  cell-element
					;				  *g-cont-ex-dens
					;				  *e-rev-excitatory-1
					;				  :type 'cont-excitatory :type-parameters (list *ex-tau *ex-delay)
					;				  :is-controlled t)))
					;    (make-synapse-conductance-alpha-waveform synapse)))


(defun create-controlled-ex-synapse-model ()
    (let ((template (make-synapse-type-template)))
      (setf
       (synapse-type-template-name template) (string 'cont-excitatory )
       (synapse-type-template-default-params template) (list *ex-tau *ex-delay) 
       (synapse-type-template-is-controlled template) t
       *synapse-type-models* (cons template *synapse-type-models*)
       (gethash (string 'cont-excitatory) *synapse-type-model-hash-table*) template)
      template))


(defvar *g-cont-ex-ds-dens 0.8)	;for marchiafava 0.8 (ps/sq-um)
(defvar *ex-ds-tau 30)		;for marchiafava 30 (ms)
(defvar *ex-ds-delay 400)

;;;  CREATE-CONTROLLED-EX-DS-SYNAPSE
					;(defun create-controlled-ex-ds-synapse (cell-element)
					;  (let* ((node (typecase cell-element
					;		 (soma  (soma-node cell-element))
					;		 (segment   (segment-node-2 cell-element))))
					;	 (synapse (create-synapse (format nil "Syn-~a-cont-ds-ex" (node-name node))
					;				  (cell-name (node-cell node))
					;				  cell-element
					;				  *g-cont-ex-ds-dens
					;				  *e-rev-excitatory-1
					;				  :type 'cont-excitatory :type-parameters (list *ex-ds-tau *ex-ds-delay)
					;				  :is-controlled t)))
					;    (make-synapse-conductance-alpha-waveform synapse)))

(defun create-controlled-ex-ds-synapse-model ()
    (let ((template (make-synapse-type-template)))
      (setf
       (synapse-type-template-name template) (string 'cont-excitatory-ds )
       (synapse-type-template-default-params template) (list *ex-ds-tau *ex-ds-delay) 
       (synapse-type-template-is-controlled template) t
       *synapse-type-models* (cons template *synapse-type-models*)
       (gethash (string 'cont-excitatory-ds) *synapse-type-model-hash-table*) template)
      template))



;;;  CREATE-IN-3-SYNAPSE
					;(defun create-in-3-synapse (cell-element)
					;  (let ((node (typecase cell-element
					;		(soma  (soma-node cell-element))
					;		(segment   (segment-node-2 cell-element)))))
					;    (create-synapse (format nil "Syn-~a-in3" (node-name node))
					;		    (cell-name (node-cell node))
					;		    cell-element
					;		    *g-inhibitory-1-dens
					;		    *e-rev-inhibitory-1
					;		    :light-input-delay *in-3-delay
					;		    :light-spatial-rf-parameters
					;		    (list 'gaussian 0 0 *in-3-sigma-x *in-3-sigma-y *syn-rf-normalized-amp)
					;		    :impulse (distorted-exponential 2100 20 410)  ;;;This values give a impulse duration of 1000 msec.
					;		    ;;(distorted-alpha 1700 20 240)         
					;		    ;;(double-alpha 350 50 999 .0) (alpha-array 300 40 1) (impulse-array) (gamma-distribution 260 13 10)
					;		    :type 'inhibitory-3
					;		    :is-light-dependent t)))

(defun create-in-3-synapse-model ()
    (let ((template (make-synapse-type-template)))
      (setf
       (synapse-type-template-name template) (string 'inhibitory-3)
       (synapse-type-template-g-dens template) *g-inhibitory-1-dens   
       (synapse-type-template-e-rev template) *e-rev-inhibitory-1  
       (synapse-type-template-light-spatial-rf-parameters template)
       (list 'gaussian 0.0 0.0 *in-3-sigma-x *in-3-sigma-y *syn-rf-normalized-amp)
       (synapse-type-template-is-light-dependent template) t
      ;;;This values give a impulse duration of 1000 msec.
       (synapse-type-template-impulse-function template) '(distorted-exponential 2100 20 410))
      (setf
       (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template)) 
       ;;(distorted-alpha 1700 20 240)         
       ;;(double-alpha 350 50 999 .0) (alpha-array 300 40 1) (impulse-array) (gamma-distribution 260 13 10)
       *synapse-type-models* (cons template *synapse-type-models*)
       (gethash (string 'inhibitory-3) *synapse-type-model-hash-table*) template
       )
      template))



;;;  CREATE-EX-3-SYNAPSE
					;(defun create-ex-3-synapse (cell-element)
					;  (let ((node (typecase cell-element
					;		(soma  (soma-node cell-element))
					;		(segment   (segment-node-2 cell-element)))))
					;    (create-synapse (format nil "Syn-~a-ex3" (node-name node))
					;		    (cell-name (node-cell node))
					;		    cell-element
					;		     *g-excitatory-1-dens
					;		    *e-rev-excitatory-1
					;		    :light-spatial-rf-parameters
					;		    (list 'gaussian 0 0 *ex-3-sigma-x *ex-3-sigma-y *syn-rf-normalized-amp)
					;		    :impulse (gamma-distribution 140 20 1)       ;;;This values give a step risetime of 80 msec.
					;		    ;;(triple-alpha 50 3 6 10 5) (zero-triple-alpha 70 3 6 1000 1)
					;		    ;;(impulse-array) (alpha-array 75 3 1) (double-alpha 100 15 999 .0)
					;		    :type 'excitatory-3
					;		    :is-light-dependent t)))

(defun create-ex-3-synapse-model ()
    (let ((template (make-synapse-type-template)))
      (setf
       (synapse-type-template-name template) (string 'excitatory-3)
       (synapse-type-template-g-dens template) *g-excitatory-1-dens  
       (synapse-type-template-e-rev template) *e-rev-excitatory-1 
       (synapse-type-template-light-spatial-rf-parameters template)
       (list 'gaussian 0.0 0.0 *ex-3-sigma-x *ex-3-sigma-y *syn-rf-normalized-amp)
       (synapse-type-template-is-light-dependent template) t
      ;;;This values give a step risetime of 80 msec.
       (synapse-type-template-impulse-function template) '(gamma-distribution 140 20 1))
      (setf
       (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template))
       ;;(triple-alpha 50 3 6 10 5) (zero-triple-alpha 70 3 6 1000 1)
       ;;(impulse-array) (alpha-array 75 3 1) (double-alpha 100 15 999 .0)
       *synapse-type-models* (cons template *synapse-type-models*)
       (gethash (string 'excitatory-3) *synapse-type-model-hash-table*) template)
      template))







(defun create-excitatory-2-synapse-model ()
    (let ((template (make-synapse-type-template)))
      (setf
       (synapse-type-template-name template) (string 'excitatory-2)
       (synapse-type-template-g-dens template) *g-excitatory-2-dens  
       (synapse-type-template-e-rev template) *e-rev-excitatory-2 
       (synapse-type-template-is-light-dependent template) t
       (synapse-type-template-impulse-function template) '(alpha-array 60 10 1))
      (setf
       (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template))
       *synapse-type-models* (cons template *synapse-type-models*)
       (gethash (string 'excitatory-2) *synapse-type-model-hash-table*) template)
      template))

					;(defun create-inhibitory-1-synapse-model ()
					;  (let ((template (make-synapse-type-template)))
					;    (setf
					;      (synapse-type-template-name template) (string 'inhibitory-1)
					;      (synapse-type-template-g-dens template) *g-inhibitory-1-dens 
					;      (synapse-type-template-e-rev template) *e-rev-inhibitory-1 
					;      (synapse-type-template-is-light-dependent template) t
					;      (synapse-type-template-impulse-function template) '(double-alpha 200 40 30 0) )
					;    (setf
					;      (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template))
					;      *synapse-type-models* (cons template *synapse-type-models*)
					;      (gethash (string 'inhibitory-1) *synapse-type-model-hash-table*) template)
					;    template))

					;(defun create-inhibitory-1-offset-synapse-model ()
					;  (let ((template (make-synapse-type-template)))
					;    (setf
					;      (synapse-type-template-name template) (string 'inhibitory-1-offset)
					;      (synapse-type-template-g-dens template) *g-inhibitory-1-dens
					;      (synapse-type-template-e-rev template) *e-rev-inhibitory-1-offset
					;      (synapse-type-template-is-light-dependent template) t
					;      (synapse-type-template-impulse-function template) '(alpha-array 300 50 1))
					;    (setf
					;      (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template))
					;      (synapse-type-template-light-spatial-rf-parameters template)
					;      (list 'impulse *light-input-offset-distance *light-input-offset-angle )
					;      *synapse-type-models* (cons template *synapse-type-models*)
					;      (gethash (string 'inhibitory-1-offset) *synapse-type-model-hash-table*) template)
					;    template))



					;(defun create-excitatory-facilitation-synapse-model ()
					;  (let ((template (make-synapse-type-template)))
					;    (setf
					;      (synapse-type-template-name template) (string 'excitatory-facilitation)
					;      (synapse-type-template-g-dens template) *g-excitatory-facilitation-dens
					;      (synapse-type-template-e-rev template) *e-rev-excitatory-facilitation-2
					;      (synapse-type-template-is-light-dependent template) t
					;      (synapse-type-template-impulse-function template) '(double-alpha 200 10 20 .5)) ;'(double-alpha 30 3 5 .5))
					;    (setf
					;      (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template))
					;      *synapse-type-models* (cons template *synapse-type-models*)
					;      (gethash (string 'excitatory-facilitation) *synapse-type-model-hash-table*) template)
					;    template))
  ;;          '(impulse light-input-offset-distance light-input-offset-angle)

(defvar *synapse-impulse-arrays* '())


(defun get-synapse-type-impulse-array (type type-impulse-function &optional type-impulse-function-args )
    (let ((impulse-array))
      (dolist (impulse-array-list *synapse-impulse-arrays*)
	(if (eq type (car impulse-array-list))
	    (setq impulse-array (cadr impulse-array-list))))
      (if (not impulse-array)
	  (progn
	    (setq impulse-array (funcall type-impulse-function type-impulse-function-args))
	    (setq *synapse-impulse-arrays* (cons (list type impulse-array) *synapse-impulse-arrays* ))))
      impulse-array))


;;;  CREATE-EXCITATORY-TRANS-SYNAPSE
					;(defun create-excitatory-trans-synapse (cell-element)
					;  (let ((node (typecase cell-element
					;		(soma  (soma-node cell-element))
					;		(segment   (segment-node-2 cell-element)))))
					;  (create-synapse (format nil "Syn-~a-extran" (node-name node)) 
					;		  (cell-name (node-cell node))
					;		  cell-element
					;		   *g-excitatory-1-dens
					;		   *e-rev-excitatory-1
					;		  :impulse (list-to-array '(.1 .4 .2 -.2 -.2 -.1 -.1 -.1 ))
					;		  :type 'excitatory-trans
					;		  :is-light-dependent t)))

(defun create-excitatory-trans-synapse-model ()
    (let ((template (make-synapse-type-template)))
      (setf
       (synapse-type-template-name template) (string 'excitatory-trans)
       (synapse-type-template-g-dens template) *g-excitatory-1-dens
       (synapse-type-template-e-rev template) *e-rev-excitatory-1
       (synapse-type-template-is-light-dependent template) t
       (synapse-type-template-impulse-function template) '(list-to-array '(.1 .4 .2 -.2 -.2 -.1 -.1 -.1 )))
      (setf
       (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template))
       *synapse-type-models* (cons template *synapse-type-models*)
       (gethash (string 'excitatory-trans) *synapse-type-model-hash-table*) template)
      template))

;;;  CREATE-EXCITATORY-SUS-SYNAPSE
					;(defun create-excitatory-sus-synapse (cell-element)
					;  (let ((node (typecase cell-element
					;		(soma  (soma-node cell-element))
					;		(segment   (segment-node-2 cell-element)))))
					;  (create-synapse (format nil "Syn-~a-exsus" (node-name node))
					;		  (cell-name (node-cell node))
					;		  cell-element
					;		   *g-excitatory-sus-dens
					;		   *e-rev-excitatory-sus
					;		  :impulse (list-to-array '(.05))
					;		  :type 'excitatory-sus
					;		  :is-light-dependent t)))

(defun create-excitatory-synapse-model ()
    (let ((template (make-synapse-type-template)))
      (setf
       (synapse-type-template-name template) (string 'excitatory-sus)
       (synapse-type-template-g-dens template) *g-excitatory-sus-dens
       (synapse-type-template-e-rev template) *e-rev-excitatory-sus
       (synapse-type-template-is-light-dependent template) t
       (synapse-type-template-impulse-function template) '(list-to-array '(.05)))
      (setf
       (synapse-type-template-impulse template) (eval (synapse-type-template-impulse-function template))
       *synapse-type-models* (cons template *synapse-type-models*)
       (gethash (string 'excitatory-sus) *synapse-type-model-hash-table*) template)
      template))


;;; CREATE-V-EX-1-SYNAPSE
(defun create-v-ex-1-synapse (cell-element pre-synaptic-node-name pre-synaptic-cell-name)
    (let ((node (typecase cell-element
		  (soma (soma-node cell-element))
		  (segment (segment-node-2 cell-element)))))
      (create-synapse (format nil "Syn-~a-vex1" (node-name node))
		      (cell-name (node-cell node))
		      cell-element
		      *g-v-excitatory-1-dens
		      *e-rev-v-excitatory-1
		      :type 'v-ex-1
		      :pre-synaptic-node-name pre-synaptic-node-name
		      :pre-synaptic-cell-name pre-synaptic-cell-name)))



;;; CREATE-V-EX-1-SYNAPSE-CHANNEL
(defun create-v-ex-1-synapse-channel (name cell-name cell-element
					   pre-synaptic-node-name pre-synaptic-cell-name
					   synapse)
  (create-channel name cell-name cell-element
		  1 (list "m" *valence-m1 *gamma-m1
			  *base-rate-m1 *v-half-m1 *base-tm1 *qten-factor-at-24) 
		  1 (list "h" (- *valence-h1) (- 1.0 *gamma-h1)
			  *base-rate-h1 *v-half-h1 *base-th1 *qten-factor-at-24) 
		  0 '()
		  (synapse-gbar-density synapse) 0
		  :pre-synaptic-node-name pre-synaptic-node-name
		  :pre-synaptic-cell-name pre-synaptic-cell-name))



;;; ****** MISCELLANEOUS


;;; SYNAPSE-WEIGHT
(defun synapse-weight (conductance position cable-length lambda)
					;  (* conductance (+ *synapse-weight-offset (/ position *synapse-weight-slope))))
    (* conductance (exp (/ (- position cable-length) (* *lambda-factor lambda)))))