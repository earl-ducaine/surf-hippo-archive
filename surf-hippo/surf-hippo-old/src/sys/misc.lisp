; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10;  -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing



#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


;;; CREATE-TREE This creates a segment tree from a list of lists. The sublist format is as follows:  
;;
;;  (mother-segment-name segment-name x y z diameter &optional extras-list)

;; The mother-segment-name refers to the proximal segment or soma, the segment-name is for the segment to be
;; created, and x, y, and z refer to the coordinates of the distal node of the segment to be created. The
;; extras-list is a list of lists for adding channels or synapses to a segment. "xy-factor" and "z-factor" are
;; scaling factors for node coordinates, which may be useful when translating histological renderings into the
;; sublists. The mother-segment-name of the first sublist will refer to the soma, which has been created already
;; with (create-soma). For example, the segment sublist:

;;   (soma		1a		7		-1  -5          1.2 )

;; specifies a segment named "1a" whose proximal end connects to the node "soma", whose distal node has
;; coordinates (7*xy-factor, -1*xy-factor, -5*z-factor), and whose diameter is 1.2 microns. Likewise, the
;; segment sublist:

;;   (1a		1b		12		-3  -7          0.6    '((channel a)) )

;; specifies a segment named "1b" whose proximal end connects to the distal node of segment "1a", whose distal
;; node has coordinates (12*xy-factor, -3*xy-factor, -7*z-factor), and whose diameter is 0.6 microns. In
;; addition, an a-type channel is included at the segment's distal node. If the mother-segment-name and
;; segment-name are the same, then this is the soma, and the diameter is the soma diameter. This entry will be
;; used to reference the coordinates of the segments, so that they are created in relative coordinates.  The
;; soma origin is set elsewhere if it is to be other than (0,0,0).

(defun create-tree (soma segment-list &key (plot-pane 2) (synapse nil) (z-factor 1.0)
			 (xy-factor 1.0) (default-diameter 0.5)
			 (save-current nil))
  (let ((cell-name (cell-name (soma-cell soma)))(soma-name (soma-name soma))(x-reference 0)(y-reference 0))
    (dolist (segment segment-list)	;look for soma entry for xy reference, soma diameter
      (if (equal (nth 0 segment) (nth 1 segment))
	  (progn
	    (setf x-reference  (nth 2 segment)
		  y-reference  (nth 3 segment)
		  (soma-diameter soma) (nth 5 segment))
	    (setq segment-list (remove segment segment-list)) ;remove soma list from segment list to avoid hassles below.
	    (return))))
    (dolist (segment-data segment-list)
      (let* ((proximal-node-name (if (eq 'soma (nth 0 segment-data))
				     soma-name
				     (format nil "~d" (nth 0 segment-data))))
	     (segment-name (format nil "~d" (nth 1 segment-data)))
	     (node2-x (- (nth 2 segment-data) x-reference))
	     (node2-y (- (nth 3 segment-data) y-reference))
	     (node2-z (nth 4 segment-data))
	     (diameter (if (nth 5 segment-data)
			   (nth 5 segment-data)
			   default-diameter))
	     (seg (create-segment segment-name proximal-node-name cell-name
				  (list '(v-leak . -70.0)(cons 'diameter diameter))
				  :plot-pane plot-pane
				  :relative-location (list (* xy-factor node2-x)(* xy-factor node2-y)
							   (* z-factor node2-z)))))
	;;	(format t "seg ~a rel loc=~a~%" (segment-name seg) (node-relative-location (segment-node-2 seg)))
	(dolist (extra-component (nth 6 segment-data))
	  (cond  ((and synapse (eq (car  extra-component) 'synapse))
		  (create-synapse-type seg (cadr extra-component) ))
		 ((eq (car extra-component)  'channel)
		  (cond ((eq (cadr extra-component) 'dr)
			 (create-dr-channel seg cell-name :save-current save-current))
			((eq (cadr extra-component) 'na1)
			 (create-na1-channel seg cell-name :save-current save-current))
			((eq (cadr extra-component) 'a)
			 (create-a-channel seg cell-name  :save-current save-current))))))))))


;;CLOSE-TO Returns T if the arguments are equal at the level of "resolution".
(defun close-to (first-float second-float &optional (resolution 0.001))
    (declare (optimize (safety 0) (speed 3) (space 1)))
  (= (round (/ (the single-float first-float) (the single-float resolution)))
     (round (/ (the single-float second-float)(the single-float resolution)))))


(defun impulse-array ()
  (let ((array (make-array 1)))
    (setf (aref array 0) 1.0)
    array))


;;; ALPHA-ARRAY Returns an array of length 'length of an alpha function whose area is 1 (when tau-power = 1)
(defun alpha-array (length tau tau-power)
  (declare (ignore length))
  (without-floating-underflow-traps		
    (do ((i 0 (1+ i))
	 (output '(0.0) (cons
			(alpha-function tau tau-power i) output)))
	((and (> i (* 3 (max tau))) (< (abs (car output)) 0.01))
	 (list-to-array (reverse output))))))



(defun alpha-function (tau tau-power time)
  (* (expt (/ time tau) tau-power) (exp (/ (- time) tau))))

(defvar *fac)

;;;This function calculates factorials of n up to 10.  It loads it in the
;;;global list fac.

(defun FACT ()        
  (setq *fac '(1))
  (do ((i 1 (+ i 1)))
      ((= i 11))
    (setq *fac (cons (* i (car *fac)) *fac)))
  (setq *fac (reverse *fac)))

;;;The following function evaluates the factorial-list upon loading.

(EVAL-WHEN (load)
  (fact))


;;; GAMMA-DISTRIBUTION returns an array of length 'length of a normalized gamma distribution function.

(defun GAMMA-DISTRIBUTION (length tau power)
  (without-floating-underflow-traps		
    (let ((*excitatory-facilitation-impulse-array (make-array length)))
      (dotimes (i length)
	(setf (aref *excitatory-facilitation-impulse-array i)
	      (* (/ 1.0 (* (nth power *fac) tau)) (expt (/ i tau) power) (exp (- (/ i tau))))))
      *excitatory-facilitation-impulse-array)))


;;; The following function is the convolution of two normalized alpha function with time constants tau1 and tau2.

(defun DISTORTED-ALPHA (length tau1 tau2)
  (without-floating-underflow-traps		
    (let ((*excitatory-facilitation-impulse-array (make-array length)))
      (dotimes (i length)
	(let ((e1 (exp (- (/ i tau1))))
	      (e2 (exp (- (/ i tau2)))))	      
	  (setf (aref *excitatory-facilitation-impulse-array i)
		(/ (+ (* i (+ e1 e2)) (/ (* -2 tau1 tau2 (- e1 e2)) (- tau1 tau2))) (* (- tau1 tau2) (- tau1 tau2))))))
      *excitatory-facilitation-impulse-array)))

;;; The following function is the convolution of a normalized alpha function with time constant tau1 and a
;;; normalized exponential distribution with time constant tau2.

(defun DISTORTED-EXPONENTIAL (length tau1 tau2)
  (without-floating-underflow-traps		
    (let ((*excitatory-facilitation-impulse-array (make-array length)))
      (dotimes (i length)
	(let ((e1 (exp (- (/ i tau1))))
	      (e2 (exp (- (/ i tau2)))))	      
	  (setf (aref *excitatory-facilitation-impulse-array i)
		(/ (- (/ (* tau1 tau2 (- e2 e1)) (- tau2 tau1)) (* i e1)) (* tau1 (- tau2 tau1))))))
      *excitatory-facilitation-impulse-array)))

;;; DOUBLE-ALPHA returns an array of length 'length of the difference of two alpha functions, with the positive
;;;having unity area and the negative having area alpha-proportion.

;;; This now truncates the array when the function is less than 0.02 the maximum or the (abs (- integral (1 -
;;; alpha-proportion))) is less than 0.01. The array values are adjusted so that the total integral is equal to
;;; (1 - alpha-proportion).
(defun DOUBLE-ALPHA (length tau1 tau2 alpha-proportion)
  (declare (ignore length))
  (without-floating-underflow-traps		
    (do* ((i 0 (1+ i))
	  (output '(0.0) (cons
			 (- (* (/ i (* tau1 tau1)) (exp (- (/ i tau1))))
			    (* alpha-proportion (/ i (* tau2 tau2)) (exp (- (/ i tau2)))))
			 output))
	  (integral 0.0 (+ integral (car output)))
	  (max 0.0 (if (> (abs (car output)) max) (abs (car output)) max)))
	 ((and (> i (* 2 (max tau1 tau2)))
	       (or (< (abs (car output)) (* 0.02 max))
		   (< (abs (- integral (- 1.0 alpha-proportion))) 0.02)))
	  (let ((fudge (/ (- integral (- 1.0 alpha-proportion)) (- (length output) 1))))
	    (do* ((j (reverse output) (cdr j))
		  (new-output '(0.0)
			      (cons (- (car j) fudge)
				    new-output)))
		 ((null (cdr j)) (list-to-array (reverse new-output)))))))))


;;;ZERO-TRIPLE-ALPHA returns an array of length 'length of the difference between one alpha function and the sum
;;;of two others.  The first alpha function is fast ('tau1) while the other two are slow.  The second alpha
;;;function is faster ('tau2) than the third ('tau3).  The total area is zero.  The second alpha function
;;;contains 'alpha-proportion proportion of the total area of the negative functions.

(defun ZERO-TRIPLE-ALPHA (length tau1 tau2 tau3 alpha-proportion)
  (without-floating-underflow-traps		
    (let ((*excitatory-facilitation-impulse-array (make-array length)))
      (dotimes (i length)
	(setf (aref *excitatory-facilitation-impulse-array i)
	      (- (*                        (/ i (* tau1 tau1)) (exp (- (/ i tau1))))
		 (* alpha-proportion       (/ i (* tau2 tau2)) (exp (- (/ i tau2))))
		 (* (- 1 alpha-proportion) (/ i (* tau3 tau3)) (exp (- (/ i tau3)))))))
      *excitatory-facilitation-impulse-array)))



;;; TRIPLE-ALPHA Returns an array of length 'length of the difference of two alpha functions, with a total area
;;; of 0, plus another alpha function of area 1. The amplitude of the alpha difference (the transient part) is
;;; scaled by the the optional argument 'transient-amplitude.
(defun triple-alpha (length tau1 tau2 sus-tau &optional (transient-amplitude 1))
  (without-floating-underflow-traps		
    (let ((array (make-array length)))
      (dotimes (i length)
	(setf (aref array i)
	      (+ 
		(* transient-amplitude
		   (- (* (/ i (* tau1 tau1)) (exp (- (/ i tau1)))) (* (/ i (* tau2 tau2)) (exp (- (/ i tau2))))))
		(* (/ i (* sus-tau sus-tau)) (exp (- (/ i sus-tau)))))))
      array)))

;;; D-ALPHA-ARRAY Returns an array of length length of the derivitave of an alpha function whose maximum amplitude is 1.
(defun d-alpha-array (length tau tau-power)
  (declare (ignore length tau tau-power))
  (let ((d-alpha-array (make-array 5)))
    (setf (aref d-alpha-array 0) 1)
    (setf (aref d-alpha-array 1) 0)
    (setf (aref d-alpha-array 2) -.5)
    (setf (aref d-alpha-array 3) -.3)
    (setf (aref d-alpha-array 4) -.2)
    d-alpha-array))

;;; TRANS-SUS-ARRAY
(defun trans-sus-array (length tau tau-power)
  (declare (ignore length tau tau-power))
  (let ((array (make-array 5)))
    (setf (aref array 0) 1.25)
    (setf (aref array 1) 0)
    (setf (aref array 2) -.5)
    (setf (aref array 3) -.3)
    (setf (aref array 4) -.2)
    array))



(defun cable-lambda (r-membrane r-axial diameter)
  "r-membrane in ohms-cm-sq, r-axial in ohms-cm, diameter in microns, result in microns"
  (* 1e4 (expt (/ (* r-membrane diameter 0.25 1e-4) r-axial) 0.5)))



(defun surf-area (radius)			;sphere surface area is in sq-cm - argument is in micrometers     
  (* 4.0 pi-single (* radius radius) 1.0e-8))


;;; CAP-MEM Returns membrane capacitance in nF of cable with dimensions length and diameter (both in microns).
;;; cap-mem is in units of microfarads/sq-cm.
(defun cap-mem (length diameter cap-mem)
  (* 1.0e3 pi-single diameter length 1.0e-8 cap-mem))
 
;;; G-LEAK-MEM Returns membrane leak conductance in uS of cable with dimensions length and diameter (both in
;;; microns). r-mem is in units of ohms-cm-cm.
(defun g-leak-mem (length diameter r-mem) 
  (* 1.0e6 pi-single diameter length 1.0e-8 (/ 1.0 r-mem)))


;;; G-EX-SYN Returns excitatory synaptic conductance in uS of cable with dimensions length and diameter
;;; (both in microns). Uses global variable *g-ex-mem, which is in units of (ohms-cm-cm)^-1.
(defun g-ex-syn (length diameter)
  (* 1.0e6 pi-single diameter length 1.0e-8  *g-ex-mem))


;;; G-IN-SYN Returns inhibitory synaptic conductance in uS of cable with dimensions length and diameter
;;; (both in microns). Uses global variable *g-in-mem, which is in units of (ohms-cm-cm)^-1.
(defun g-in-syn (length diameter)
  (* 1.0e6 pi-single diameter length 1.0e-8  *g-in-mem))	

;;; G-AXIAL Returns axial conductance in uS of cable with dimensions length and diameter (both in
;;; microns). r-cyto is in units of ohms-cm.
(defun g-axial (length diameter r-cyto)
  (* 1.0e6 pi-single diameter diameter 1.0e-4 0.25 (/ 1.0 length) (/ 1.0 r-cyto)))

;;; G-SOMA Radius in microns, resistivity in ohms cm*cm. Answer in microsiemens.
(defun g-soma (radius resistivity) 
  (* 1.0e6 (/ (surf-area radius) resistivity)))

;;; CAP-SOMA Radius in microns, capacitance in microfarads per cm*cm. Answer in nanofarads.
(defun cap-soma (radius capacitance) 
  (* 1.0e3 (* (surf-area radius) capacitance )))



;;; UPDATE-QTENS
(defun update-qtens ()
  (setq *qten-factor-at-25 (qten-tau-factor (+ 273 25.0) *temperature* *qten)
	*qten-factor-at-25-m (qten-tau-factor (+ 273 25.0) *temperature* *qten-m)
	*qten-factor-at-32 (qten-tau-factor (+ 273 32.0) *temperature* *qten)	;Ca kinetics
	*qten-factor-at-30 (qten-tau-factor (+ 273 30.0) *temperature* *qten)	;DR and A kinetics
	*qten-factor-at-27 (qten-tau-factor (+ 273 27.0) *temperature* *qten)
	*qten-factor-at-22 (qten-tau-factor (+ 273 22.0) *temperature* *qten)
	*qten-factor-at-24 (qten-tau-factor (+ 273 24.0) *temperature* 5.0)	;Na kinetics.
	*qten-g-24 (qten-rate-factor (+ 273 24.0) *temperature* *qten-ionic)	;Qten for  ionic conductance of Na currents.
	*qten-g-30 (qten-rate-factor (+ 273 30.0) *temperature* *qten-ionic)	;Qten for ionic conductance of DR and
						;A currents.
	*qten-g-32 (qten-rate-factor (+ 273 32.0) *temperature* *qten-ionic)	;Qten for ionic conductance of Ca currents.
	*qten-factor-at-37 (qten-tau-factor (+ 273 37.0) *temperature* *qten)
	*qten-factor-at-14 (qten-tau-factor (+ 273 14.0) *temperature* *qten)))

;;;QTEN-TAU-FACTOR This calculates the qten factor for time constants (as temperature goes up, tau goes down).
(defun qten-tau-factor (reference-temp temp qten)
  (expt qten (/ (-  reference-temp temp ) 10.0)))

;;;QTEN-RATE-FACTOR This calculates the qten factor for rate constants (as temperature goes up, so does rate).
(defun qten-rate-factor (reference-temp temp qten)
  (expt qten (/ (-  temp reference-temp  ) 10.0)))




;;;;Things to initialize node voltages via *init-value-list*, "initial-voltage" slot in each node, and menus.
;;; *old-init-value-list* will hold values from last simulation.



;;; For now nodes are created with their "initial-voltage" slot set to *e-holding.

;;; Note: Do not add the ground node to the *init-value-list* - this crashes later because
;;; set-node-voltage expects a core for all of the nodes it operates on, and dc-solution calls set-node-voltage
;;; for all the nodes in *init-value-list*. The ground node does not have a core-node since it is a DC source.

;;; SET-INITIAL-NODE-VOLTAGES
(defun set-initial-node-voltages ()
  (setq *init-value-list* '())
  (maphash 'generate-init-value-list node-hash-table)
  (setq *old-init-value-list* *init-value-list*)	;keep this run's initial values in *old-init-value-list*
  )


(defun generate-init-value-list (name nd)
  (if (node-core-nd nd)				;This prevents voltage source nodes from being added to
						;*init-value-list*.
  (setq *init-value-list* (nconc *init-value-list* (list (cons name (node-initial-voltage nd)))))))

;;; UPDATE-CELL-NAME-LIST We are keeping a list of cell names in *cells* which is updated by
;;; this function. Right now, this function is called from create-segment and create-membrane. Note that *cells*
;;; should be cleared by the sim-file.
(defun update-cell-name-list (cell-name)
  (if (not (member cell-name *cells* :test #'equal))
      (setq *cells* (nconc *cells* (list cell-name)))))







;;;TREE-CONTROL Generates a binary tree structure recursively with number of bifurcations equal to "depth".
;;;Segments are named by their distal node. Each branch of the tree has "branch-depth" segments.
(defun tree-control (cell-name parent-node-name depth direction branch-depth distance-from-soma &key (plot-pane 1)
		     (length 50))
  (let (last-node-name (leftright (if (eq direction 'left) "a" "b")) theta)
    (if (not (= depth 0))
	(progn
	  (do* ((branch-number 0 (1+ branch-number))
		(node1-name parent-node-name (format nil "~a-~a~d" parent-node-name leftright branch-number))
		(node2-name (format nil "~a-~a~d" parent-node-name leftright (1+ branch-number))
			    (format nil "~a-~a~d" parent-node-name leftright (1+ branch-number))))	
	       ((= branch-number branch-depth))
;	    (format t "~%Creating cell ~a segment ~a, input node ~a, output node ~a, distance from soma ~d"
;		    cell-name (format nil "~a-seg" node2 parent)
;		    node1 node2 (+ distance-from-soma branch-number))
	    (cond ((and (= branch-number 0) (eq direction 'left))
		   (setq theta (* pi-single -0.02 depth)))
		  ((and (= branch-number 0) (eq direction 'right))
		   (setq theta (* pi-single 0.02 depth)))
		  (t (setq theta 0)))
	    (create-segment (format nil "~a-seg" node2-name) node1-name cell-name	;
			    (list '(v-leak . -70)
				  (cons 'length length)
				  (cons 'diameter (* depth 0.2))
				  (cons 'theta theta) '(phi . 0)) :plot-pane plot-pane)
	    (setq last-node-name node2-name))

	  (tree-control cell-name last-node-name (1- depth) 'left branch-depth (+ distance-from-soma branch-depth)
			:plot-pane plot-pane)
	  (tree-control cell-name last-node-name (1- depth) 'right branch-depth
			(+ distance-from-soma branch-depth) :plot-pane plot-pane))))) 




;;; GROW-SPINES Calls grow-cell-spines for all the cells.
;(defun grow-spines ()				;
;  (dolist (cell-name *cells*)		;Go through the cells
;    (grow-cell-spines cell-name)))



						;
;;; MAKE-SEGMENTS-AND-MEMBRANES-LIST This searches through the instances of the segment and membrane models,
;;; picks up their nodes, and adds those nodes to a list which is returned.
(defun make-segments-and-membranes-list  (cell-name)
  (let ((temp-list '()))
    (dolist (mod *models*)
      (cond ((string-equal "segment" (model-template-name mod))	;Pick up segment model
	     (dolist (inst (model-template-instances mod))
	       (dolist (elt (model-instance-elements inst))
		 (if (string-equal cell-name (cell-name (segment-cell elt)))
		     (let ((node1 (node-name (segment-node-1 elt)))
			   (node2 (node-name (segment-node-2 elt))))
		       (if (not (member node1 temp-list))
			   (setq temp-list (nconc temp-list (list node1))))
		       (if (not (member node2 temp-list))
			   (setq temp-list (nconc temp-list (list node2)))))))))
	    ((string-equal "soma" (model-template-name mod))	;Pick up soma model
	     (dolist (inst (model-template-instances mod))
	       (dolist (elt (model-instance-elements inst))
		 (if (string-equal cell-name (cell-name (soma-cell elt)))
		     (let ((node (node-name (soma-node elt))))
		       (if (not (member node temp-list))
			   (setq temp-list (nconc temp-list (list node)))))))))
	    ))
    temp-list))





;;; MAKE-CHANNELS-LIST This searches through the instances of the channel models and returns a list of all their
;;; names.
(defun make-channels-list  ()
  (let ((all-channels '()))			
    (dolist (mod *models*)
      (cond ((string-equal "channel" (model-template-name mod))	;Pick up segment model
	     (dolist (inst (model-template-instances mod))
	       (dolist (elt (model-instance-elements inst))
		 (setq all-channels (nconc all-channels (list (channel-name  elt)))))))))
    all-channels))

;;; MAKE-PARTICLES-LIST This searches through the instances of the particle models and returns a list of all their
;;; names.
(defun make-particles-list  ()
  (let ((all-particles '()))			
    (dolist (mod *models*)
      (cond ((string-equal "particle" (model-template-name mod))	;Pick up segment model
	     (dolist (inst (model-template-instances mod))
	       (dolist (elt (model-instance-elements inst))
		 (setq all-particles (nconc all-particles (list (particle-name  elt)))))))))
    all-particles))

;;; MAKE-SYNAPSES-LIST This searches through the instances of the synapse models and returns a list of all their
;;; names.
(defun make-synapses-list  ()
  (let ((all-synapses '()))			
    (dolist (mod *models*)
      (cond ((string-equal "synapse" (model-template-name mod))	;Pick up segment model
	     (dolist (inst (model-template-instances mod))
	       (dolist (elt (model-instance-elements inst))
		 (setq all-synapses (nconc all-synapses (list (synapse-name  elt)))))))))
    all-synapses))



;;; CREATE-SOURCE
(defun create-source (clamp-type cell-element)
  (if *include-sources
      (if  (equal clamp-type "Current clamp")
	   (create-pwl-isource cell-element)
	   (create-pwl-vsource cell-element))))

;;; SET-CIRCUIT-SOURCE-VALUES
(defun  set-circuit-source-values ()
  (maphash 'get-source-hash-values isource-hash-table)
  (maphash 'get-source-hash-values vsource-hash-table))

;;;  GET-SOURCE-HASH-VALUES
(defun get-source-hash-values (source-name source)
  (let ((type (named-structure-symbol source)))
    (get-source-values source-name type)))


;; Stimulus Source Parameters - Stimulus sources, both current and voltage, generate piece-wise linear waveforms
;; which in turn are derived from user-specified sequences of pulses. The pulse sequences may be specified by
;; entries in the the list *old-pulse-lists*, as follows:
;;
;;     (source-list1 source-list2 source-list3 ...)
;;
;; The format of each source-list is as follows:
;;
;;  (source-name-type-list pulse-list1 pulse-list2 ...)
;;
;; The format of each source-name-type-list is as follows:
;;
;;   (cons source-name source-type), e.g. ("cable-1-soma-stim" . ISOURCE)
;;
;; The format of each pulse-list is as follows:
;;
;;  (start-time stop-time magnitude), e.g. (4 6 .1) 
;;
;;
;; For example, consider if *old-pulse-lists* =
;;
;;  ((("cable-1-soma-stim" . ISOURCE) (3 4 0.1) (10 12 -0.2))(("cable-2-stim" . VSOURCE) (20 25 -70)))
;;
;; This would specify the waveform for a current source "cable-1-soma-stim" which provides a 0.1 nA pulse from
;; 3 milliseconds to 4 milliseconds and a -0.2 nA pulse from 10 milliseconds to 12 milliseconds and 0 nA otherwise, and
;; the waveform for a voltage source "cable-2-stim" which holds its circuit node to -70 millivolts from 20 to 25
;; milliseconds, and to 0 millivolts otherwise.
;;
;; *old-pulse-lists* may be specified in the original circuit file.



;;; GET-SOURCE-VALUES Updates and prompts, if necessary, changes in the cell source by converting pulse list
;;; into pwl list. Updates value list for the source. Updates *old-pulse-lists*.
(defun get-source-values (source-name type)
  (let ((temp-list (if (eq type `isource)
		       (list (cons 0  0))
		       (list (cons 0  *vclamp-default-magnitude))))
	(number-of-pulses (get-pulse-number source-name))
	(source-list (car (member source-name *old-pulse-lists* :test #'string= :key 'caar)))
	(temp-pulse-list '()) (pulse-number 0))
    (if (or (not source-list) number-of-pulses)	;For renovating pulses or for new sources.
	(dotimes (i (if number-of-pulses number-of-pulses 1))
	  (setq *temp-pulse-start-time 0.0 *temp-pulse-stop-time 0.0 *temp-pulse-magnitude 0.0)
	  (setq temp-list (add-pulse pulse-number type source-name temp-list))
	  (push (list *temp-pulse-start-time
		      (if (> *temp-pulse-stop-time user-stop-time) user-stop-time *temp-pulse-stop-time)
		      *temp-pulse-magnitude)
		temp-pulse-list)
	  (setq pulse-number (1+ pulse-number)))
	(dolist (pulse-list (cdr source-list))	;For editing old pulses.
	  (setq *temp-pulse-start-time (coerce (nth 0 pulse-list) 'single-float)
		*temp-pulse-stop-time (coerce (nth 1 pulse-list) 'single-float)
		*temp-pulse-magnitude (coerce (nth 2 pulse-list) 'single-float))
	  (setq temp-list (add-pulse pulse-number type source-name temp-list))
	  (push (list *temp-pulse-start-time
		      (if (> *temp-pulse-stop-time user-stop-time) user-stop-time *temp-pulse-stop-time)
		      *temp-pulse-magnitude)
		temp-pulse-list)
	  (setq pulse-number (1+ pulse-number))))
    (if source-list (setq *old-pulse-lists* (remove source-list *old-pulse-lists*)))
    (push (cons (cons source-name type)  (reverse temp-pulse-list)) *old-pulse-lists*)
    (if (/=  (car (car temp-list)) user-stop-time)	;Add final zero stimulus interval
	(progn (push (cons (+ (car (car temp-list)) 0.1) 0.0) temp-list)
	       (push (cons user-stop-time 0.0) temp-list)))
    (setq temp-list (sort (delete-duplicates temp-list :test #'equal) #'< :key 'car))
    (if (eq type `isource)
	(setf (isource-value-list (gethash source-name isource-hash-table)) temp-list)
	(setf (vsource-value-list (gethash source-name vsource-hash-table)) temp-list))))

(defun get-pulse-number (source-name)
  (if (and *surf-interactive* *modify-stimulus)
      (if (y-or-n-p (format nil "Do you want to change the number of pulses for source ~a?" source-name))
	  (prompt-and-read :number
			   (format nil "Enter number of pulses for source ~a - " source-name)))))


(defun add-pulse (pulse-number type source-name temp-list)
  (declare (ignore pulse-number source-name))	; use in gui later
  (let ((start 0.0)	(stop 0.0)
	(current-mag (cdr (car temp-list)))
	(new-mag *temp-pulse-magnitude))
    (setq stop  (min user-stop-time *temp-pulse-stop-time)
	  start  (min user-stop-time *temp-pulse-start-time))
    ;;Insert a zero nA or *vclamp-default-magnitude mV stimulus interval.
    (if (/= start (car (car temp-list)))	
	(push (cons (+ (car (car temp-list)) 0.1)
		    (if (eq type `isource) 0.0 *vclamp-default-magnitude)) temp-list))
    (push (cons start (coerce current-mag 'single-float))
	  temp-list)
    (push (cons (+ start 0.1) new-mag) temp-list) ;Construct pulse
    (push (cons stop new-mag) temp-list)
    temp-list))

;(defun add-pulse (pulse-number type source-name temp-list)
;  (let ((start 0.0)
;        (stop 0.0)
;        (current-mag (cdr (car temp-list)))
;        (new-mag *temp-pulse-magnitude))
;    (print "made it here 0")
;    (print  (max (float user-stop-time)(float *temp-pulse-stop-time)))
;    (print "made it here 1")
;    (setq stop  (min user-stop-time *temp-pulse-stop-time)
;          start  (min user-stop-time *temp-pulse-start-time))
;    (print "made it here 2")
;
;
;
;    #-sun (if *modify-stimulus
;              (if (eq type `isource)
;                  (menu-for-isource-values source-name pulse-number)
;                  (menu-for-vsource-values source-name pulse-number)))
;    ;;Insert a zero nA or *vclamp-default-magnitude mV stimulus interval.
;    (if (/= start (car (car temp-list)))        
;        (push (cons (+ (car (car temp-list)) 0.1)
;                    (if (eq type `isource) 0.0 *vclamp-default-magnitude)) temp-list))
;
;    (push (cons start (coerce current-mag 'single-float))
;          temp-list)
;    (push (cons (+ start 0.1) new-mag) temp-list) ;Construct pulse
;    (push (cons stop new-mag) temp-list)
;    temp-list))



(defun estimate-r-input-limits (cell-name)
  (if (y-or-n-p "Do you want to change the cell membrane?") 
      (progn (setq *modify-cell-type t)
	     (set-circuit-elements-parameters)
	     (setq *modify-cell-type nil)))
  (let* ((cell (gethash cell-name cell-hash-table))
	 (g-in (+ (soma-g-shunt (cell-soma cell)) (soma-g-leak (cell-soma cell)))))
    (dolist (segment (cell-segments cell))
      (setq g-in (+ g-in (segment-g-leak segment))))
    (format t "~%Cell ~a has a maxmimum G-in = ~,2e uS, or minimum R-in = ~,2e Mohms ~%"
	    cell-name g-in (/ 1.0 g-in))))



;; LAMBDA-CABLE Returns cable electrotonic space constant in cm. Intracellular resistivity r-i is in
;; ohm*cm, membrane resistivity r-m is in ohm*cm*cm, and cable radius a-um is in microns.
(defun lambda-cable (r-i r-m a-um)
  (let ((a (* 1.0e-4 a-um)))
    (sqrt (/ (* a r-m)
	     (* 2.0 r-i)))))

;; Z-IN-CABLE Returns input resistance (Mohms) to sealed-end (open circuit) cable of length l-um in microns.
;; Intracellular resistivity r-i is in ohm*cm, membrane resistivity r-m is in ohm*cm*cm, and cable radius a-um
;; is in microns. Optional g-end is in uS.
(defun z-in-cable (r-i r-m a-um l-um &optional (g-end 0))
  (let ((a (* 1.0e-4 a-um))(l (* 1.0e-4 l-um))(g-inf)(b-1))
    (setq g-inf (/ (* pi-single a a 1e6) 
		   (* r-i (lambda-cable r-i r-m a-um))))
    (setq b-1 (/ g-end g-inf))
    (/ 1.0
       (* g-inf
	  (/ (+ b-1 (tanh (/ l  (lambda-cable r-i r-m a-um))))
	     (+ 1 (* b-1 (tanh (/ l (lambda-cable r-i r-m a-um))))))))))
	


;; R-IN Returns somatic input resistance to soma-short-cable structure with soma radius a-soma-um in microns,
;; soma membrane resistivity r-m-soma in ohm*cm*cm. Intracellular resistivity r-i is in ohm*cm, membrane
;; resistivity r-m is in ohm*cm*cm, and cable radius a-um is in microns.
(defun r-in (r-i r-m a-um l-um a-soma-um r-m-soma)
  (/ 1.0 (+  (* 1.0e-6 (g-soma a-soma-um r-m-soma))
	     (/ 1.0 (z-in-cable r-i r-m a-um l-um)))))



;
;(defvar w '())
;(defvar phase-hi '())
;(defvar phase-lo '())
;(defvar mag-hi '())
;(defvar mag-lo '())
;
;(defun test-phase ()
;  (setq w '() phase-hi '() phase-lo '() mag-hi '() mag-lo '())
;  (do ((omega 0.01 (+ omega 0.01)))
;      ((> omega 50.0))
;    (setq w (cons omega w)
;	  phase-lo (cons  (atan (- omega)) phase-lo)
;	  phase-hi (cons  (atan (/ 1.0 omega)) phase-hi)
;	  mag-lo (cons  (/ 1.0 (expt (+ 1 omega) 0.5))
;			mag-lo)
;	  mag-hi (cons  (/ 1.0 (expt (+ 1 (/ 1.0 omega)) 0.5))  mag-hi))))


;;; Generic function to display times

(defun display-time()
  (format t "Simulation time: ~5,0d. Stop time: ~5,0d~%" *real-time user-stop-time))
