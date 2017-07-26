;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing


;; Some sample neurons.


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defvar *distal-a-current nil)
(defvar *num-nodes 2)


;; HIPPO Hippo linear cell geometry, channels. This circuit is taken from MIT-AI TR 1161, with some
;; modifications.

(defvar *active)

(defun hippo (cell-name &key (active *active) synapse-types save-particle
			(cell-origin '(0.0 0.0 0.0))
			(final-node-number 5) ; The Thesis had 5 segments, 240 uM each
			include-ca-variation)
  (let ((segment-length (/ 1200.0 final-node-number))(segment-diameter 12.0) soma)
    (create-cell-type "CA1"
		      :membrane-resistivity 40000.0
		      :cytoplasmic-resistivity 200.0
		      ;; Note that the HIPPO thesis value for R-M-soma
		      ;; is 1/3 too small. 2550 ohms-cm2 is the
		      ;; correct value for matching R-in of 39Mohms.
		      :soma-resistivity 2550.0	
		      :specific-capacitance 1.0)
    (create-cell cell-name :cell-type-name "CA1" :cell-origin cell-origin)
    (setf soma (create-soma (format nil "~a-soma" cell-name) cell-name 35.0 
			    :model-ca-variation include-ca-variation))
    (if active
	(create-channels '(na1 na2 na3 a dr) soma :save-particle save-particle))
    (if include-ca-variation
	(create-channels '(ca c ahp) soma :save-particle save-particle :model-ca-variation t))
    (create-source *clamp-type soma)
    (dotimes (node-number final-node-number)
      (let* ((node-name (if (= node-number 0)
			    (node-name (soma-node soma)) (format nil "~a-~d" cell-name node-number)))
	     (next-node-name (format nil "~a-~d" cell-name (+ 1 node-number)))
	     (seg (create-segment next-node-name node-name cell-name
				  (list (cons 'length segment-length)
					(cons 'diameter segment-diameter)
					(cons 'phi 0.0)))))
	(loop for type in synapse-types
	      do (create-synapse-type seg type))))))


;;; AUTO-RUN This function calls "surf" repeatedly with various parameters.
(defun auto-run ()
  (setq *overlay-simulations nil *automatic-run t *modify-cell-type nil *hard-copy-screen nil *save-simulation nil
 	user-stop-time 400.0 *bar-a-start-time 20.0 *bar-a-stop-time 50.0 *bar-b-start-time 20.0 *bar-b-stop-time 50.0)
  (surf 'single-cable-1 nil)
  (setq *overlay-simulations t)
  (let ((b-delay 50))
    (dotimes (i 5)
    (setq *bar-b-start-time (+ *bar-b-start-time  b-delay)
	  *bar-b-stop-time (+ *bar-b-stop-time  b-delay))
    (if (= i 5) (setq 	 *hard-copy-screen t))
    (surf 'single-cable-1 nil)))
  (setq  *automatic-run t *modify-cell-type nil *hard-copy-screen nil *save-simulation nil)
  (setq *bar-b-start-time 20 *bar-b-stop-time 50 *bar-a-start-time 20 *bar-a-stop-time 50)
  (surf 'single-cable-1 nil)
  (setq *overlay-simulations t)
  (let ((a-delay 50))
    (dotimes (i 5)
    (setq *bar-a-start-time (+ *bar-a-start-time  a-delay)
	  *bar-a-stop-time (+ *bar-a-stop-time  a-delay))
    (if (= i 5) (setq *hard-copy-screen t))
    (surf 'single-cable-1 nil)))
  )

(defvar *hippo-synapse-type 'ex-3)
;;; Some variants on "hippo".


(setq *event-driven-synapse-waveform-in   (alpha-array 1 50 1)
	*event-driven-synapse-waveform-ex	 (alpha-array 1 10 1)
	user-stop-time 400.0
	*use-synapse-events t
	*SOMA-AMPLITUDES '()
	*SOMA-INTEGRALS '()
	*SYNAPSE-DATA '()
	*SYNAPSE-STATS '()
 *SYNAPSE-EVENT-waveform  (alpha-array 1 20 1))

(defun syn-cable-10 ()
  (setq *plot-nodes* '("hippo-2" "hippo-4" "hippo-6" "hippo-8" "hippo-10" "hippo-soma") 
	;;	*plot-channel-currents* '("hippo-soma-a" "hippo-soma-na3" "hippo-soma-na2"
	;;				  "hippo-soma-na1" "hippo-soma-dr" )
	*plot-synapse-currents* '("Syn-hippo-10-EXCITATORY" 
				  "Syn-hippo-6-EXCITATORY"
				  "Syn-hippo-2-INHIBITORY" 
				  "Syn-hippo-2-EXCITATORY" 
				  "Syn-hippo-6-INHIBITORY" 
				  "Syn-hippo-10-INHIBITORY"))
  (setq *plot-synapse-conductances*	*plot-synapse-currents*)
  (setq *include-sources nil	
    *old-pulse-lists* (list (list (cons "hippo-soma-istim"  'ISOURCE)
				  (list *isource-start *isource-stop *imag))))
  (setq *active nil)
  (hippo "hippo" :cell-origin '(-400.0 0.0 0.0) :final-node-number 10
	 :synapse-types (list `EXCITATORY `inhibitory)))


(defun syn-circuit ()
  (hippo "cable-1" :cell-origin '(0.0 200.0 0.0)  :synapses nil :include-distal-a-current *distal-a-current
	      :include-ca-current nil :include-ca-variation nil)
  (hippo-input "cable-2" "cable-1-10" "cable-1"))

(defun soma-1 ()
  (hippo-soma "soma-1" :cell-origin '(0.0 0.0 0.0)))

(defun hippo-soma (cell-name &key (plot-pane 1) (active t) (save-particle nil)(cell-origin '(0 0 0)))
  "Hippo linear cell geometry, channels"
  (without-floating-underflow-traps
    (let (soma)
      (create-cell cell-name :cell-type-name "soma" :cell-origin cell-origin )
      (setf soma (create-soma  cell-name (* 5.0 2) :soma-name (format nil "~a-soma" cell-name)))
      (if active (create-channels '(na1 na2 na3 dr a) soma :save-particle save-particle  :plot-pane plot-pane))
      (create-source *clamp-type soma)
      (let ((next-node-name (format nil "~a-~d" cell-name (+ 1 0))))
	(create-segment next-node-name (format nil "~a-soma" cell-name) cell-name
			(list (cons 'length 100) (cons 'diameter 2) (cons 'phi 0))
			:plot-pane plot-pane)))))

(defun hippo-input (cell-name input-node-name input-cell-name &key (plot-pane 1) 
			 (cell-origin '(0 0 0)) (final-node-number 10))
  (let ((segment-length 30.0)(segment-diameter 0.50)
	soma)
    (create-cell cell-name :cell-type-name "cable" :cell-origin cell-origin )
    (setf soma (create-soma  cell-name (* 5.0 2) :soma-name (format nil "~a-soma" cell-name)))
    (create-source *clamp-type soma)
    (dotimes (node-number final-node-number)
      (let ((node-name (if (= node-number 0) (node-name (soma-node soma))
			   (format nil "~a-~d" cell-name node-number)))
	    (next-node-name (format nil "~a-~d" cell-name (+ 1 node-number))))
	(let ((seg
		(create-segment next-node-name node-name cell-name
				(list (cons 'length segment-length)
				      (cons 'diameter segment-diameter)
				      (cons 'phi 0))
				:plot-pane plot-pane)))
	  (if (= node-number (- final-node-number 1))
	      (create-synapse "receiver" cell-name seg 10 -70 nil :type 'v-ex-1
			      :pre-synaptic-node-name input-node-name
			      :pre-synaptic-cell-name input-cell-name :save-current t)))))))


(defun lots-of-hippos () 
 (dotimes (i 20)
    (let ((cell-name (format nil "cell-~a" i)))
    (hippo cell-name :plot-pane 2 :active t :save-particle nil))))

(defun tree-cell (cell-name &key (plot-pane 1)(active nil))
  (ignore active)
  (let ((soma-node-name (format nil "~a-1" cell-name))
	)
    (create-soma soma-node-name cell-name 0 0.01509 -70 1.282816e-2
		 :model-ca-variation t
		 :area (* 4.0 3.1415 *soma-radius *soma-radius 1.0e-8)
		 :plot-pane plot-pane)
    (tree-control cell-name soma-node-name 4 'left 3 0 :plot-pane plot-pane)))




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



(defun generic-cell (cell-name cell-type-name segment-list soma-radius)
  (declare-ground  "Ground")
  (create-cell cell-name :cell-type-name cell-type-name)
  (let ((soma (create-soma (format nil "~a-soma" cell-name)
			   cell-name
			   (* 2.0 soma-radius) )))
    (create-channels '(na1 na2 na3 dr a) soma :plot-pane 3)
    (create-source *clamp-type soma)
    (create-tree soma segment-list)))


;;; This is the start of a CA1 cell with a real dendritic tree.
(defun real-hippo (cell-name &key (cell-origin '(0.0 0.0 0.0)) (synapse nil) (extras-list '()))
  (declare-ground  "Ground")
  (setq *soma-radius 17.5)
  (create-cell cell-name :cell-type-name "hippo-ca1" :cell-origin cell-origin)
  (let ((soma (create-soma (format nil "~a-soma" cell-name) cell-name (* 2.0 *soma-radius))))
;    (create-channels '(na1 na2 na3 dr a) 3 soma :save-particle nil)
    (if *include-sources (create-source *clamp-type soma))
    (setf 	(cell-type-notes (gethash "hippo-ca1" cell-type-hash-table))
		"A mock CA1 cell.~%"
		)
    (create-tree (cell-soma (gethash cell-name cell-hash-table)) 


;; The segment-list format is as follows: (mother-segment-name segment-name x y z diameter extras-list)


		 `(
		   (soma	1		2		30  0      2    ,extras-list)
		   (1		11		10		40 5     1      ,extras-list)
		   (11		111		10		100 5  .5 ,extras-list)
		   (1		12		-10		40 5     1      ,extras-list)
		   (12		121		-10		100 5  .5 ,extras-list)
		   (soma	2		5		-60 0      2    ,extras-list))
		 :default-diameter .5 :synapse synapse :xy-factor 1)

    ))