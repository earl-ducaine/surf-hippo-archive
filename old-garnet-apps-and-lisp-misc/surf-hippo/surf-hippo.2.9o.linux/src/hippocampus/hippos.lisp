;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
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

;; Some sample neurons.


(in-package "SURF-HIPPO")

;; See the surf-hippo/doc/basics.doc file for more information.

(defun dead-HIPPO (&optional (name "Hippo") cell-type nucleus-diameter)
  (hippo name :cell-type cell-type :active nil :nucleus-diameter nucleus-diameter))

(push 'dead-hippo *CIRCUIT-FUNCTIONS*)  

;; This version creates an instance of the cell HIPPO, but allows a name argument, and sets up some
;; simple simulation parameters.
(defun BASIC-HIPPO (&optional (name "Hippo"))
  (let* ((soma (cell-soma (working-hpc :name name)))
	 (source (add-isource soma)))
    (pulse-list source '((10.0 20.0 2.0) (100.0 110.0 -5.0)))
    (enable-element-plot soma)
    (enable-element-plot source)
    (enable-element-plot (distal-tips))
    (setq *user-stop-time* 200.0)
    ;; Returns the created cell, useful if embedded in another circuit function.
    *cell*))

(defun Two-HIPPOs ()
  ;; Make two HIPPO cells, next to each other.
  (let ((cell1 (working-hpc :name "Hippo-1" :cell-origin '(-300.0 00.0 100.0)))
	(cell2 (working-hpc :name "Hippo-2" :cell-origin '(0.0 -100.0 0.0))))

    ;; A connection from cell Hippo-1 to cell Hippo-2 - 
    (create-synapse "Hippo-2-4"		; Destination is a segment of cell2.
		    'fast-ex
		    ;; The axon creation is embedded in the subsequent synapse creation.
		    (create-axon (cell-soma cell1) ; Presynaptic signal originates is the soma of
					; cell1 and conveyed by a simple axon.
				 'simple))

    ;; And another synapse onto the tree of Hippo-2, this time driven directly from the soma voltage
    ;; of Hippo 1
    (create-synapse "Hippo-2-3"		; Destination is a segment of cell2.
		    'fast-ex
		    (cell-soma cell1))	; Presynaptic signal
  
    ;; And a connection from the soma of cell Hippo-2 to the dendrite of cell Hippo-1 - 
    (create-synapse "Hippo-1-5"
		    'fast-ex
		    (create-axon
		     (cell-soma cell2)	; Presynaptic signal
		     'simple)) 

    ;; Some external inputs to Hippo-1

    ;; One gets a simple pulse    
    (pulse-list (add-ISOURCE (cell-soma cell1)) '((10.0 20.0 2.0)))
    ;; The other a sin burst
    (add-waveform (add-isource "Hippo-1-3")
		  :waveform-args '(:SIN (STEP . 0.1) (DELAY . 60.0) (TAU . 10.0) (DURATION . 30.0)
				   (AMPLITUDE . 4.0) (PHASE . 0.0) (FREQUENCY . 0.4) (OFFSET . 4.0)))

    (setq *user-stop-time* 100.0)


    ;; Set up some plotting 

    (plot-segments-to-soma "Hippo-1-5")
    (plot-segments-to-soma "Hippo-2-5")

    (enable-element-plot (list (somas) '("Hippo-1-5" "Hippo-2-3" "Hippo-2-5")))
    (enable-element-plot (list (channels) (synapses)) 'conductance)
    (enable-element-plot (isources))
    (enable-element-plot (axons))

    ;; To mark the synapses on the histology
    (set-type-graphics 'fast-ex 'yellow)
    nil))

; (push 'two-hippos *CIRCUIT-FUNCTIONS*)


(defun three-HIPPOs ()
  (let* ((cell1 (working-hpc :name "Hippo-1" :cell-origin '(-300.0 00.0 100.0)))
	 (cell2 (working-hpc :name "Hippo-2"  :cell-origin '(0.0 200.0 -300.0)))
	 (cell3 (working-hpc :name "Hippo-3"  :cell-origin '(400.0 -200.0 0.0)))
	 (axon-1-3 (create-axon (cell-soma cell1) 'simple
				:mid-points '((-210.0 -100.0 200.0)
					      (0.0  -250.0 100.0) 
					      (280.0 35.0 -200.0) 
					      (200.0 820.0 -100.0)))))
    (setf (axon-type-propagation-velocity (element-type axon-1-3)) 20.0)
    (set-axons-parameters)
    (create-synapse "Hippo-3-4" 'fast-ex axon-1-3)
    (create-synapse "Hippo-2-4" 'fast-ex (cell-soma cell1))
    (add-events (create-synapse "Hippo-1-5" 'auto-fast-ex) (poisson-events .20 40.0 100.0))

    (element-type-param 'auto-fast-ex :gbar-source :absolute)
    (element-type-param 'auto-fast-ex :gbar-ref 0.05)
	
    (pulse-list (add-isource (cell-soma cell1)) '(10.0 20.0 2.0))

    (setq *user-stop-time* 250.0)

    (plot-segments-to-soma "Hippo-1-5")
    (plot-segments-to-soma "Hippo-2-5")
    (plot-segments-to-soma "Hippo-3-5")

    (enable-element-plot (list (somas)
			       '("Hippo-1-5" "Hippo-2-4" "Hippo-3-4")
			       (axons)))
    (enable-element-plot (list (axons) (synapses)) 'event)

    (setq *group-plot-data-by-cell* nil)

    (set-type-graphics 'fast-ex 'yellow)
    (set-type-graphics 'auto-fast-ex 'cyan)
  
    nil))

(push 'three-hippos *CIRCUIT-FUNCTIONS*)




(defun axon-HIPPO ()
  (working-hpc :name "Hippo-1" :cell-origin '(0.0 100.0 0.0))
  (create-axon *soma* 'simple :length 100 :delay 15)
  (pulse-list (add-isource *soma*) '((10.0 20.0 2.0)))
  (enable-element-plot (list *soma* "Hippo-1-5" *axon*))
  (setq *user-stop-time* 100.0)
  *cell*)



;
;
;Deeds which populate the dimensions of space and which reach their end
;when someone dies may cause us wonderment, but one thing, or an infinite
;number of things, dies in every final agony, unless there is a universal
;memory as the theosophists have conjectured. In time there was a day that
;extinguished the last eyes to see Christ; the battle of Junin and the
;love of Helen died with the death of a man. What will die with me when I
;die, what pathetic or fragile form will the world lose? The voice of
;Macedonia Fernandez, the image of a red horse in the vacant lot at Serrano
;and Charcas, a bar of sulphur in the drawer of a mahogany desk?
;
;				- Jorge Luis Borges, "The Witness"



(defun ca3_32da-hippo ()
  (hippo "ca3_32da-hippo"
	 :active nil
	 :dendrite-diameter 13.4
	 :dendrite-length (* 5 200.0)
	 :soma-diameter 30.07
	 :r-cyto 311.0
	 :r-mem 1.13e5
	 :r-mem-soma 2.02e5
	 :cap-mem 9.54e-1
	 :cap-mem-soma 9.54e-1))
