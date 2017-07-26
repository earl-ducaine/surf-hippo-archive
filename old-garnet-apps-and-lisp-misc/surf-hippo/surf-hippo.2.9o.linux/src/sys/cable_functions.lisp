;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10; -*- Lyle Borg-Graham, Equipe
;; Cogniscience, Institut Alfred Fessard, CNRS

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


;;; SYS Source file: cable_functions.lisp

(in-package "SURF-HIPPO")

;; Functions for evaluating electrical properties of somas, cables and cable compartments.

(proclaim '(inline g-element))
(defun g-element (element g-density)
  "G-DENSITY is in pS per square micron. Conductance returned is in uS."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float g-density))
  (the sf (* (the sf (element-area element)) ; um2
	     (the sf (* 1e-6		; Convert pS to uS
			g-density)))))

(defun cable-lambda (r-membrane r-axial diameter)
  "R-MEMBRANE in ohms-cm-sq, R-AXIAL in ohms-cm, DIAMETER in microns, result in microns"
  (* 1e4 (expt (/ (* r-membrane diameter 0.25 1e-4) r-axial) 0.5)))

(proclaim '(inline cap-mem))
(defun cap-mem (length diameter cap-mem)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float length diameter cap-mem))
  ;; (* 1.0e-8 1.0e3 pi-single) = 3.1415927e-5
  (* 3.1415927e-5 diameter length cap-mem))

(defun capacitance-mem (length diameter cap-mem)
  "Returns membrane capacitance in nF of cable with dimensions LENGTH and DIAMETER [both in microns].
CAP-MEM is in units of microfarads/sq-cm."
  (cap-mem length diameter cap-mem))

(proclaim '(inline g-leak-mem))
(defun g-leak-mem (length diameter r-mem)
  "Returns membrane leak conductance in uS of cable with dimensions LENGTH and DIAMETER [both in
microns]. R-MEM is in units of ohms-cm-cm."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float length diameter r-mem))
  ;; (* 1.0e-8 1.0e6 pi-single) = 0.03141593
  (/ (* 0.03141593 diameter length) r-mem))

(proclaim '(inline g-axial))
(defun g-axial (length diameter r-cyto)
  "Returns axial conductance in uS of cable with dimensions LENGTH and DIAMETER (both in microns).
R-CYTO is in units of ohms-cm."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float length diameter r-cyto))
  ;; (* 1.0e6 pi-single 1.0e-4 0.25) = 78.53982
  (/ (* 78.53982 diameter diameter)
     (* length r-cyto)))

 
(defun g-soma (radius resistivity)
  "RADIUS in microns, RESISTIVITY in ohms cm*cm. Answer in microsiemens."
  (* 1.0e6 (/ (sphere-area-cm2 radius) resistivity)))


(defun cap-soma (radius capacitance)
  "RADIUS in microns, CAPACITANCE in microfarads per cm*cm. Answer in nanofarads."
  (* 1.0e3 (* (sphere-area-cm2 radius) capacitance )))


(defun R-IN-SOMA-SHORT-CABLE (r-i r-m a-um l-um a-soma-um r-m-soma)
  "Returns somatic input resistance (ohms) to soma-short-cable structure with soma radius A-SOMA-UM in
microns, soma membrane resistivity R-M-SOMA in ohm*cm*cm. Intracellular resistivity R-I is in
ohm*cm, membrane resistivity R-M is in ohm*cm*cm, and cable radius A-UM is in microns."
  (/ 1.0 (+  (* 1.0e-6 (g-soma a-soma-um r-m-soma))
	     (/ (* 1.0e6 (z-cable-in r-i r-m a-um l-um))))))

(defun max-g-in (&optional (cell *cell*) (exclude-electrodes t))
  "Returns the linear somatic input conductance (uS) of CELL, with the cytoplasmic resistivity set to zero."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((electrodes (electrodes))
	 (g-in (+ (soma-g-leak (cell-soma cell))
		  (if (soma-include-shunt (cell-soma cell)) (soma-g-shunt (cell-soma cell)) 0.0))))
    (loop for segment in (cell-segments cell)
	  when (or (not exclude-electrodes) (not (member segment electrodes :test #'eq)))
	  sum (segment-g-leak segment) into result double-float
	  finally (return (s-flt (+ result g-in))))))

 
(defun cell-cap (&optional (cell *cell*) (exclude-electrodes t))
  "Returns the total capacitance of the CELL in nF."
  (let ((segments (if exclude-electrodes (segments-not-electrodes cell) (cell-segments cell))))
    (s-flt (+ (soma-capacitance (cell-soma cell))
	      (loop for seg in segments sum (segment-capacitance seg))))))


(proclaim '(inline lambda-cable))
(defun lambda-cable (r-i r-m a-um)
  "Cable electrotonic space constant in cm. Intracellular resistivity R-I is in ohm*cm, membrane
resistivity R-M is in ohm*cm*cm, and cable radius A-UM is in microns."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type single-float a-um r-m r-i))
  (the sf (sqrt (/ (* 1.0e-4 a-um r-m)
		   (* 2.0 r-i)))))

(defun length-from-lambda (r-i r-m a-um L)
  "Returns cable length in um given intracellular resistivity R-I [ohm*cm], membrane resistivity R-M
[ohm*cm*cm], cable radius A-UM [microns], and electrotonic length L [dimensionless!]."
  (declare (type number r-i r-m a-um L))
  (* (* 10000				; lambda-cable returns cm
	(lambda-cable (float r-i) (float r-m) (float a-um)))
     L))

 
(defun segment-electrotonic-length (seg)
  "Returns electrotonic length of segment SEG."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((membrane-factor (get-a-value 'membrane-area-coefficient (segment-parameters seg))))
    (the sf
	 (electrotonic-length
	  (segment-length seg)
	  (segment-diameter seg)
	  (segment-cytoplasmic-resistivity seg)
	  (segment-ri-coeff seg)
	  (if membrane-factor
	      (the sf (/ (segment-membrane-resistivity seg) (the sf membrane-factor)))
	      (segment-membrane-resistivity seg))))))

(proclaim '(inline electrotonic-length))
(defun electrotonic-length (length diameter cell-type-cytoplasmic-resistivity ri-coeff
				   cell-type-membrane-resistivity)
  "Returns electrotonic length of segment given explicit parameters LENGTH (uM), DIAMETER (uM),
CELL-TYPE-CYTOPLASMIC-RESISTIVITY (ohms cm), RI-COEFF (dimensionless), and
CELL-TYPE-MEMBRANE-RESISTIVITY (ohms cm cm)."
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type single-float length diameter cell-type-cytoplasmic-resistivity ri-coeff
		 cell-type-membrane-resistivity))
  (/ length
     (* 10000.0				; lambda-cable returns cm
	(the sf (lambda-cable
		 (* cell-type-cytoplasmic-resistivity ri-coeff)
		 cell-type-membrane-resistivity
		 (* 0.5 diameter))))))
		

(proclaim '(inline g-inf-in))
(defun g-inf-in (r-i r-m a-um &optional lambda-cable)
  "Input conductance of semi-infinite cable, in uS.  Intracellular resistivity R-I is in ohm*cm,
membrane resistivity R-M is in ohm*cm*cm, and cable radius A-UM is in microns."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((a (the sf (* 1.0e-4 (the sf a-um)))) ; Convert to cm
	(lambda-cable (or lambda-cable (lambda-cable r-i r-m a-um))))
    (declare (type single-float a lambda-cable r-i))
    (/ (* pi-single a a 1e6)
       (* r-i lambda-cable))))


(proclaim '(inline z-cable-in))
(defun z-cable-in (r-i r-m a-um l-um &optional (g-end 0.0))
  "Returns input resistance (Mohms) to sealed-end (open circuit) cable of length L-UM in microns.
Intracellular resistivity R-I is in ohm*cm, membrane resistivity R-M is in ohm*cm*cm, and cable
radius A-UM is in microns. Optional G-END is in uS."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((l (* 1.0e-4 (the sf l-um)))
	 (lambda-cable (lambda-cable r-i r-m a-um))
	 (g-inf (the sf (g-inf-in r-i r-m a-um lambda-cable)))
	 (b-1 (the sf (/ (the sf g-end) g-inf)))
	 (tanh-l/lamb (the sf (tanh (the sf (/ l (the sf lambda-cable)))))))
    (declare (type single-float l g-inf b-1 tanh-l/lamb lambda-cable))
    (/ 1.0
       (* g-inf (/ (+ b-1 tanh-l/lamb)
		   (+ 1.0 (* b-1 tanh-l/lamb)))))))


(defun z-cable-in-seg (segment &key store-segment-z-cable-in)
  "Returns input resistance (Mohms) of SEGMENT, taking into account the tree distal to the segment,
using the cable parameters."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((membrane-factor (get-a-value 'membrane-area-coefficient (segment-parameters segment)))
	 (g-end (loop for distal-segment in (distal-segments segment)
		      summing (/ 1.0 (the sf (z-cable-in-seg distal-segment)))
		      into result single-float finally (return result)))
	 (z-cable-in-seg
	  (z-cable-in
	   (the sf (* (segment-cytoplasmic-resistivity segment)
		      (segment-ri-coeff segment)))
	   (if membrane-factor
	       (the sf (/ (segment-membrane-resistivity segment)
			  (the sf membrane-factor)))
	       (segment-membrane-resistivity segment))
	   (* 0.5 (segment-diameter segment))
	   (segment-length segment)
	   g-end)))
    (when (and (or store-segment-z-cable-in *store-segment-z-cable-in*)
	       (not (element-parameter-fast 'soma-segment (segment-parameters segment))))
      (set-element-parameter-fast segment 'g-cable-end g-end (segment-parameters segment))
      (set-element-parameter-fast segment 'z-cable-in z-cable-in-seg (segment-parameters segment)))
    (values z-cable-in-seg g-end)))
(defun segment-g-end (segment)
  (or (element-parameter-fast 'g-end (segment-parameters segment))
      (nth-value 1 (z-cable-in-seg segment :store-segment-z-cable-in t))))

(defun segment-g-cable-end (segment)
  (or (element-parameter-fast 'g-cable-end (segment-parameters segment))
      (nth-value 1 (z-cable-in-seg segment :store-segment-z-cable-in t))))

(defun segment-g-end-discrete (segment)
  (or (element-parameter-fast 'g-discrete-end (segment-parameters segment))
      (nth-value 1 (z-discrete-in-seg segment :store-segment-z-discrete-in t))))




(defun clear-all-z-cable-in (&optional cell-or-type)
  (let ((cell-or-type (element cell-or-type)))
    (if (cell-type-p cell-or-type)
	(loop for cell in (cell-type-cells cell-or-type) do (clear-all-z-cable-in cell))
	(loop for segment in (if (cell-p cell-or-type)
				 (cell-segments cell-or-type)
				 (segments))
	      do
	      (set-element-parameter-fast segment 'g-cable-end nil (segment-parameters segment))
	      (set-element-parameter-fast segment 'z-cable-in nil (segment-parameters segment))
	      (set-element-parameter-fast segment 'g-discrete-end nil (segment-parameters segment))
	      (set-element-parameter-fast segment 'z-discrete-in nil (segment-parameters segment))))))
			    
(defun clear-z-cable-in-to-soma (seg)
  (loop for segment in (segments-in seg) do
	(set-element-parameter-fast segment 'g-cable-end nil (segment-parameters segment))
	(set-element-parameter-fast segment 'z-cable-in nil (segment-parameters segment))
	(set-element-parameter-fast segment 'g-discrete-end nil (segment-parameters segment))
	(set-element-parameter-fast segment 'z-discrete-in nil (segment-parameters segment))))

(defun z-tree-cable-in-cell-from-stored-values (&optional (cell *cell*) include-virtual-soma)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when (cell-tree-p cell)
    (the df (/ 1.0 
	       ;;Start at the cell soma, and work on each of the branches that originate there in turn.
	       (loop for element in (if include-virtual-soma
					(loop for elt in (node-elements (soma-node (cell-soma cell)))
					      when (typecase elt (segment t)) collect elt)
				      (trunk-segments cell))
		     sum (/ 1.0 (the sf (or (get-cell-element-param-fast element 'z-cable-in)
					    (z-cable-in-seg element :store-result t))))
		     into result single-float finally (return result))))))

(defun r-in (&optional (cell *cell*))
  "Returns input resistance (Mohms) of CELL, using the cable parameters for the dendritic tree if there is one."
  (z-cable-in-cell cell))
    
(defun z-cable-in-cell (&optional (cell *cell*) z-tree)
  "Returns input resistance (Mohms) of CELL, using the cable parameters for the dendritic tree if
Z-TREE is not supplied. Otherwise, the input resistance is calculated from the soma resistance and
the Z-TREE argument (Mohms)."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (/ 1.0 (+ (soma-g-leak (cell-soma cell))
	    (if (soma-include-shunt (cell-soma cell)) (soma-g-shunt (cell-soma cell)) 0.0)
	    (the sf (cond (z-tree (/ 1.0 (the sf z-tree)))
			  ((cell-tree-p cell) (/ 1.0 (z-tree-cable-in-cell cell t)))
			  (t 0.0))))))

(defun cell-tree-p (cell)
  (loop for seg in (get-node-elements-of-type (cell-soma cell) 'segment)
	unless (electrode-p seg) do (return t)))

(defun z-tree-cable-in-cell (&optional (cell *cell*) include-virtual-soma)
  "Returns input resistance (Mohms) of dendritic tree of CELL, using the cable parameters. If no tree,
returns NIL. If INCLUDE-VIRTUAL-SOMA is T, include any segments assigned to the soma."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when (cell-tree-p cell)
    (/ 1.0
       ;;Start at the cell soma, and work on each of the branches that originate there in turn.
       (loop for element in (if include-virtual-soma (element-segments (cell-soma cell)) (trunk-segments cell))
	     sum (/ 1.0 (the sf (z-cable-in-seg element :store-result t)))
	     into result single-float finally (return result)))))


(defun z-discrete-in-seg (segment &key (look-distally t) explicit-g-end store-segment-z-discrete-in)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((g-end (cond
		  (look-distally (loop for distal-seg in (distal-segments segment)
				       summing (the df (/ 1.0
							  (the df (z-discrete-in-seg distal-seg
											 
										     :store-segment-z-discrete-in
										     store-segment-z-discrete-in))))
				       into result double-float finally (return result)))
		  (explicit-g-end (the df explicit-g-end))
		  (t 0.0d0)))
	 (g-temp (+ (the df g-end) (segment-g-leak segment)))
	 (z-discrete-in-seg (/ (+ (segment-g-axial segment) g-temp)
			       (* (segment-g-axial segment) g-temp))))			
    (when (and (or store-segment-z-discrete-in *store-segment-z-discrete-in*)
	       (not (element-parameter-fast 'soma-segment (segment-parameters segment))))
      (set-element-parameter-fast segment 'g-discrete-end g-end (segment-parameters segment))
      (set-element-parameter-fast segment 'z-discrete-in z-discrete-in-seg (segment-parameters segment)))
    (values z-discrete-in-seg g-end)))

(defun z-discrete-in-cell (&optional cell z-tree)
  "Returns somatic input resistance (Mohms) of CELL, using the compartmental network parameters."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((cell (or cell *cell*)))
    (/ 1.0 (+ (soma-g-leak (cell-soma cell))
              (the sf (if (soma-include-shunt (cell-soma cell))
                          (soma-g-shunt (cell-soma cell))
                          0.0))
	      (cond (z-tree (/ 1.0 z-tree))
		    ((cell-tree-p cell) (/ 1.0 (z-tree-discrete-in-cell cell t)))
		    (t 0.0))))))

(defun z-tree-discrete-in-cell-from-stored-values (&optional (cell *cell*) include-virtual-soma)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when (cell-tree-p cell)
    (s-flt (/ 1.0 
	      ;;Start at the cell soma, and work on each of the branches that originate there in turn.
	      (loop for element in (if include-virtual-soma
				       (loop for elt in (node-elements (soma-node (cell-soma cell)))
					     when (typecase elt (segment t)) collect elt)
				     (trunk-segments cell))
		    sum (the df (/ 1.0 (the df (or (get-cell-element-param-fast element 'z-discrete-in)
						   (z-discrete-in-seg element :store-result t)))))
		    into result double-float finally (return result))))))


(defun z-tree-discrete-in-cell (&optional (cell *cell*) include-virtual-soma)
  "Returns input resistance (Mohms) of dendritic tree of CELL, using the compartmental network
parameters. If no tree, returns NIL. If INCLUDE-VIRTUAL-SOMA is T, include any segments assigned to
the soma."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (when (cell-tree-p cell)
    (s-flt
     (/ 1.0 
	;;Start at the cell soma, and work on each of the branches that originate there in turn.
	(loop for element in (if include-virtual-soma
				 (loop for elt in (node-elements (soma-node (cell-soma cell)))
				       when (typecase elt (segment t)) collect elt)
			       (trunk-segments cell))
	      sum (the df (/ 1.0 (the df (z-discrete-in-seg element :store-result t))))
	      into result double-float finally (return result))))))

(defun rho (&optional (cell *cell*))
  "Tree/soma conductance ratio of cell associated with CELL."
  (let ((cell (element-cell cell))) 
    (when cell
      (/ 1.0
	 (* (z-tree-discrete-in-cell cell t)
	    (+ (soma-g-leak (cell-soma cell))
	       (if (soma-include-shunt (cell-soma cell)) (soma-g-shunt (cell-soma cell)) 0.0)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun steady-state-linear-segment-voltage-clamp-voltage (segment distal-voltage &optional setit go-on)
  (let ((steady-state-segment-voltage-rel
	 (/ (+ (* distal-voltage (segment-g-axial segment))
	       ; (* (segment-v-leak segment) (segment-g-leak segment))
	       )
	    (+ (segment-g-end-discrete segment) (segment-g-axial segment) (segment-g-leak segment)))))
    (when (or go-on setit) (set-segment-voltage segment
						(+ steady-state-segment-voltage-rel (segment-v-leak segment))))
    (when go-on
      (loop for distal-segment in (distal-segments segment) do
	    ;; (format t "clamping distally ~A at ~A, distal v ~A ..~%" distal-segment
	    ;; (segment-voltage segment) distal-voltage)
	    (steady-state-linear-segment-voltage-clamp-voltage distal-segment
							       steady-state-segment-voltage-rel
							       t t)))
    steady-state-segment-voltage-rel))

(defun steady-state-linear-voltage-clamp (vsource &optional holding-potential)
  (unless *circuit-processed* (process-circuit-structure))
  (let* ((vsource-element (element-cell-element vsource))
	 (cell (element-cell vsource))
	 (soma (cell-soma cell))
	 (holding-potential (or holding-potential (element-current-value vsource 'voltage)))
	 (soma-holding-voltage holding-potential)
	 (soma-electrode-p (and (attached-to-soma-p vsource-element) (electrode-p vsource-element))))
    (if (not (or (soma-p vsource-element) soma-electrode-p))
	(format t "STEADY-STATE-LINEAR-VOLTAGE-CLAMP only works from soma or soma electrodes for now...~%")
	(progn
	  ;; First, setup all the g-end values.
	  (loop for seg in (trunk-segments vsource-element) do (segment-g-end-discrete seg))

	  (when soma-electrode-p
	    (setq soma-holding-voltage
		  (+ (soma-v-leak soma)
		     (/ (* (- holding-potential (soma-v-leak soma)) (cell-z-discrete-in-cell cell))
			(+ (ELECTRODE-RESISTANCE vsource-element) (cell-z-discrete-in-cell cell))))))
	  (set-element-voltage soma soma-holding-voltage)
			       
	 
	  ;; Now apply the clamp.
	  (loop for seg in (trunk-segments vsource-element)
		unless (eq seg vsource-element)
		do
					; (format t "clamping ~A..~%" seg)
		(steady-state-linear-segment-voltage-clamp-voltage
		 seg (- soma-holding-voltage (segment-v-leak seg))
		 t t))))))
      


(defun estimate-r-input-limits (cell-name)
  (when (Y-OR-N-P-DEFAULT-NO "Do you want to change the cell membrane?") (set-circuit-elements-parameters))
  (let ((g-in (max-g-in (gethash cell-name (CELL-HASH-TABLE)))))
    (format t "~%Cell ~a has a maxmimum G-in = ~,2e uS, or minimum R-in = ~,2e Mohms ~%"
	    cell-name g-in (/ 1.0 g-in))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometric stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cell-area (&optional (cell *cell*))
  "Returns the total membrane area of cell associated with CELL in square microns."
  (element-area (element-cell cell)))


(defun tree-area (&optional (cell *cell*))
  "Returns the area in square microns of the dendritic (and axonal) tree attached to the soma of
associated with CELL"
  (loop for cell in (coerce-to-list (element-cell cell)) sum
	(loop for seg in (cell-segments cell)
	      unless (member seg (soma-segments (cell-soma cell))) sum (element-area seg))))

(defun trunk-3/2s-info (&optional (cell (cells)))
  "Prints out the 2/3 root of the sum of the dendritic trunk diameters raised to the 3/2."
  (loop for cell in (coerce-to-list (element-cell cell)) do 
	(format t "Cell ~A [sum(d^3/2)]^2/3 trunks: ~,2fum~%"
		(cell-name cell) (expt (loop for seg in (trunk-segments cell)
					     sum (expt (segment-diameter seg) (/ 3 2))) (/ 2 3)))))


(defun tree-length (&optional (cell *cell*))
  "Returns the total length in microns of the dendritic (and axonal) tree attached to the soma of CELL."
  (loop for seg in (segments (element-cell cell)) sum (segment-length seg)))

(defun print-area-info (&optional (stream t))
  (loop for cell in (cells)
	do (format stream "Cell ~A: soma area ~,2f um2, tree area ~,2f um2~%"
		   (cell-name cell) (element-area (cell-soma cell)) (tree-area cell))))

(defun print-rall-info (&optional (stream t))
  (loop for cell in (cells) do
	(format stream
		"Cell ~A: 2/3 Power of sum of ~d trunk diameter~:p, each raised to 3/2 [um] - ~,2f~%"
		(cell-name cell) (length (TRUNK-SEGMENTS cell))
		(expt (loop for seg in (TRUNK-SEGMENTS cell)
			    sum (expt (segment-diameter seg) 1.5)) (/ 2.0 3.0)))))


#| ANTIQUE

;;; G-EX-SYN Returns excitatory synaptic conductance in uS of cable with dimensions length and
;;; diameter (both in microns). Uses global variable *g-ex-mem, which is in units of
;;; (ohms-cm-cm)^-1.
(defun g-ex-syn (length diameter)
  (* 1.0e6 pi-single diameter length 1.0e-8  *g-ex-mem))


;;; G-IN-SYN Returns inhibitory synaptic conductance in uS of cable with dimensions length and
;;; diameter (both in microns). Uses global variable *g-in-mem, which is in units of
;;; (ohms-cm-cm)^-1.
(defun g-in-syn (length diameter)
  (* 1.0e6 pi-single diameter length 1.0e-8  *g-in-mem))	






;;; Z-DISCRETE-IN-SEG  Returns input resistance (Mohms) of segment, taking into account the tree distal
;;; to the segment, using the compartmental network parameters.
(defun z-discrete-in-seg (segment)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (the sf
       (let ((g-end 0.0)(g-temp 0.0) result)
	 (dolist (distal-segment (distal-segments segment))
	   (setq g-end (the sf
			    (+ g-end
			       (the sf
				    (/ 1.0
				       (the sf (z-discrete-in-seg distal-segment))))))))
	 (setq g-temp (the sf (+ (segment-g-leak segment)
					   g-end)))
	 (setq result
	       (/ 1.0 (the sf (/ (the sf (* (segment-g-axial segment) g-temp))
					   (the sf (+ (segment-g-axial segment) g-temp))))))
	 (element-parameter segment 'z-discrete-in result)
	       
	 )))


(defun z-cable-in-old (r-i r-m a-um l-um &optional (g-end 0))
  (let ((a (* 1.0e-4 a-um))
	(l (* 1.0e-4 l-um))
	(g-inf)(b-1))
    (setq g-inf (/ (* pi-single a a 1e6) 
		   (* r-i (lambda-cable r-i r-m a-um))))
    (setq b-1 (/ g-end g-inf))
    (/ 1.0
       (* g-inf
	  (/ (+ b-1 (tanh (/ l  (lambda-cable r-i r-m a-um))))
	     (+ 1 (* b-1 (tanh (/ l (lambda-cable r-i r-m a-um))))))))))

(defun lambda-cable-old (r-i r-m a-um)
  (let ((a (* 1.0e-4 a-um)))
    (sqrt (/ (* a r-m)
	     (* 2.0 r-i)))))


|#