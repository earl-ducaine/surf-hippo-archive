;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF ; Base: 10; -*-
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


;;; SYS Source file: plot.lisp

(in-package "SURF-HIPPO")

;;; The function ELEMENT-DATA has now been defined to provide a more consistent interface, although
;;; it calls RETRIEVE-SINGLE-DATA (see element_functions.lisp).
;;;
;;; RETRIEVE-SINGLE-DATA Given the string or element ELEMENT and the symbol DATA-TYPE, returns the
;;; reversed plot data list directly from the circuit structure referenced in ELEMENT. If either ELEMENT does
;;; not refer to a circuit element, or if the DATA-TYPE is inconsistent with the referenced element,
;;; or if the data was not stored in the last simulation (i.e. was not earmarked for plotting), then
;;; the function returns NIL.

;;; Note that the data is in correct time order (ie the same as *SIM-PLOT-TIME-LIST*).

;;; The element names which refer to the following element types may be matched with data types as
;;; follows [for example, (RETRIEVE-SINGLE-DATA "Hippo-soma" 'soma-voltage)]:

;;; extracellular-electrode 'field-potential

;;; segment 'voltage, 'voltage-derivative , 'dvdt

;;; soma 'dendrite-current, 'voltage, 'voltage-derivative, 'dvdt

;;; channel 'current, 'reversal-potential, 'conductance

;;; synapse 'current, 'reversal-potential, 'conductance

;;; particle 'state 'markov-states

;; For markov state data, the optional STATE-INDEX integer argument must be supplied

;;; conc-particle 'state

;;; axon 'voltage

;;; vsource 'current

;;; isource 'current

;;; conc-int 'concentration-1, 'concentration-2, 'concentration-3, 'total-concentration
;;; conc-int 1, 2, 3, 'total

;;; buffer 'concentration

;;; pump 'current

(defun retrieve-single-data (element data-type &optional state-index)
  (let ((element (if (node-p element) (node-cell-element element) element)))		   
    (case data-type
      (markov-states (find-markov-particle-state-data element state-index))
      (use-*vsrvolt* (reverse *vsrvolt*))
      (use-*vsrnodevolt* (reverse *vsrnodevolt*))
      (t (car (retrieve-plot-data (list (list (list element) data-type))))))))

  
;;; RETRIEVE-PLOT-DATA This function returns a list of data lists, each of which is of the type
;;; referenced in the cadr of each sublist in STRUCTURES-STRUCTURE-DATA-TYPES and comes from the
;;; structure name which is the car of the sublists. The data is reversed from that stored in the
;;; structures, thus it is returned in correct time order. This function allows the order of the
;;; traces (top to bottom) of multiple data plots to be determined by the order in the original
;;; name-lists (e.g. *plot-nodes* or *plot-channel-currents*).
#|
(defun retrieve-plot-data (structures-structure-data-types)
  (no-nils
   (loop
    for structure-structure-data-type in structures-structure-data-types
    nconcing
    (loop
     for element in (first structure-structure-data-type)
     collecting
     (reverse
      (let ((element (element element))
	    (data-type (second structure-structure-data-type)))
	(typecase element
	  (extracellular-electrode
	   (case data-type
	     (field-potential (extracellular-electrode-field-potential-data element))))
	  (segment
	   (case data-type
	     (voltage (segment-voltage-data element))
	     ((dvdt voltage-derivative node-voltage-derivative) (segment-voltage-derivative-data element))))
	  (soma
	   (case data-type
	     (voltage (soma-voltage-data element))
	     ((dvdt voltage-derivative node-voltage-derivative) (soma-voltage-derivative-data element))
	     (dendrite-current (soma-dendrite-current-data element))))
	  (channel
	   (case data-type
	     (current (channel-current-data element))
	     (reversal-potential (channel-reversal-potential-data element))
	     (conductance (channel-conductance-data element))))
	  (synapse
	   (case data-type
	     (current (synapse-current-data element))
	     (reversal-potential (synapse-reversal-potential-data element))
	     (conductance (synapse-conductance-data element))))
	  (particle
	   (case data-type
	     (state (particle-state-data element))))
	  (conc-particle
	   (case data-type
	     (state (conc-particle-state-data element))))
	  (conc-int
	   (case data-type
	     ((1 concentration-1 shell-1) (conc-int-shell-1-data element))
	     ((2 concentration-2 shell-2) (conc-int-shell-2-data element))
	     ((3 concentration-3 shell-3) (conc-int-shell-3-data element))
	     ((total total-concentration) (conc-int-total-data element))))
	  (pump
	   (case data-type
	     (current (pump-current-data element))))
	  (buffer
	   (case data-type
	     (concentration (buffer-concentration-data element))))
	  (axon
	   (case data-type
	     (voltage (axon-voltage-data element))))
	  (isource
	   (case data-type
	     (current (isource-current-data element))))
	  (vsource
	   (case data-type
	     (current (vsource-current-data element))
	     (voltage (vsource-voltage-data element))
	     )))))))))
|#
(defun retrieve-plot-data (structures-structure-data-types)
  (no-nils
   (loop
    for structure-structure-data-type in structures-structure-data-types
    nconcing
    (loop
     for element in (first structure-structure-data-type)
     collecting
     (reverse
      (let* ((element (element element))
	     (model (element-model element))
	     (data-type (second structure-structure-data-type))
	     (saved-data-function (model-output-saved-data-function model data-type)))
	(when saved-data-function (funcall saved-data-function element))))))))
	  
	  


(defun pick-diagonal-somas-for-plotting ()
  (setq *plot-nodes* nil
	*plot-soma-nodes*
	(loop for soma being the hash-value of (SOMA-HASH-TABLE)
	      when
	      (or
	       (= (first (element-absolute-location soma))
		  (second (element-absolute-location soma)))
	       (= (first (element-absolute-location soma))
		  (third (element-absolute-location soma)))
	       (= (third (element-absolute-location soma))
		  (second (element-absolute-location soma))))
	      collect (soma-name soma))))


(defun dump-all-plot-lists ()
  (let* ((pathname-directory (get-surf-data-directory))
	 (info-filename (get-surf-filename pathname-directory "plot-settings.lisp"))
	 (new-file (not (probe-file info-filename)))
	 (*print-pretty* nil))
    (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
    (when (probe-file (ext:unix-namestring pathname-directory nil))
      (with-open-stream (*standard-output* (open info-filename :direction :output :if-exists :supersede
						 :if-does-not-exist :create))
	(when new-file (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%"))
	(format t "#|~%~%")
	(print-circuit t)
	(format t "|#~%~%")
      
	
	(loop for plot-list-info in *plot-lists-info*
	      when (symbol-value (plot-list-info-names plot-list-info)) do
	      (write-lisp-sym-list-to-stream (plot-list-info-names plot-list-info)
					     (symbol-value (plot-list-info-names plot-list-info))
					     t 10 0 nil))
	(loop for plot-list-info in *plot-lists-info* do
	      (format t "(setq ~A ~A)~%"
		      (plot-list-info-enable-var plot-list-info)
		      (true-p (symbol-value (plot-list-info-enable-var plot-list-info)))))

	(loop for plot-var in '( *create-new-simulation-plots* *plot-standard-windows* *plot-synapses-by-major-ion*
				*plot-channels-by-major-ion* ;; *plot-currents-by-major-ion*
				;; *position-plots-by-hand
				*enable-plot-labels*
				*SAVE-CONDUCTANCES-normalized*
				*use-same-line-style* *group-plot-data-by-cell*
				*GROUP-PLOT-DATA-BY-CELL-TYPE* *plot-voltages-solid
				*overlay-all-plots* *accomodate-all-overlays*
				;; *overlay-simulations *overlay-simulations-last
				*plot-custom-windows* *simulation-plot-window-comment*
				*traces-per-plot* *automatic-voltage-plot-scaling
				*voltage-plot-min *voltage-plot-max
				*automatic-soma-voltage-plot-scaling *soma-voltage-plot-min
				*soma-voltage-plot-max *save-data-step*
				*save-data-step-count* *plot-total-concs-separately*
				;; *plot-shell-concs-separately*
				*plot-event-generators* *plot-events*
				*plot-node-elements* *plot-separate-soma-window
				;; *plot-concentrations
				;; *transformed-plot
				)
	      do
	      (WRITE-LISP-SYM-AND-VALUE-TO-STREAM plot-var t)))
      (format t "File ~a written~%" info-filename))))
				   


(defun clear-all-plot-lists ()
  (setq *plot-lists-cells* nil
	*plot-lists-cell-types* nil)
  (loop for plot-list-info in *plot-lists-info*
	do (set (plot-list-info-names plot-list-info) nil)
	(set (plot-list-info-structures plot-list-info) nil))
  (setup-models-output-data-enabled))


(defun remove-entry-from-*plot-node-elements* (name)
  (setq *plot-node-elements* (remove name *plot-node-elements* :test 'equal)))


(defun parse-plot-node-elements ()
  (setq *plot-node-elements*
	(loop for elt in (delete-duplicates (coerce-to-list *plot-node-elements*))
	      collect (element-name (or (element-cell-element elt 'soma)
					(element-cell-element elt 'segment)))))
  (loop for node-name in *plot-node-elements* do
	(let ((node (or (element-physical-node node-name 'soma)
			(element-physical-node node-name 'segment))))
	  (when node
	    (loop for elt in (node-elements node) do
		  (typecase elt
		    (pump (when (and *plot-pump-currents-p (not (eq *plot-pump-currents* 'all)))
			    (push (pump-name elt) *plot-pump-currents*)))
		    (isource (when (and *plot-isource-currents-p (not (eq *plot-isource-currents* 'all)))
			       (push (isource-name elt) *plot-isource-currents*)))
		    (vsource (when (and *plot-vsource-currents-p (not (eq *plot-vsource-currents* 'all)))
			       (push (vsource-name elt) *plot-vsource-currents*)))
		    (soma (when (and *plot-soma-voltage-p
				     (or (not (eq *plot-nodes* 'all)) (not (eq *plot-soma-nodes* 'all))))
			    (push (node-name node) *plot-soma-nodes*))
			  (when (and *plot-soma-voltage-derivative-p
				     (or (not (eq *plot-node-derivatives* 'all))
					 (not (eq *plot-soma-node-derivatives* 'all))))
			    (push (node-name node) *plot-soma-node-derivatives*))
			  (when (not (eq *plot-soma-dendrite-currents* 'all))
			    (push (node-name node) *plot-soma-dendrite-currents*)))
		    (segment (when (and *plot-node-voltages-p (not (eq *plot-nodes* 'all))
					(eq node (segment-node-2 elt)))
			       (push (node-name node) *plot-nodes*))
			     (when (and *plot-node-voltage-derivatives-p (not (eq *plot-node-derivatives* 'all))
					(eq node (segment-node-2 elt)))
			       (push (node-name node) *plot-node-derivatives*)))
		    (axon (when (and *plot-axon-voltages-p (not (eq *plot-axons* 'all)) (eq node (axon-node elt)))
			    (push (node-name node) *plot-axons*)))
		    (channel (when (and *plot-channel-currents-p (not (eq *plot-channel-currents* 'all)))
			       (push (channel-name elt) *plot-channel-currents*))
			     (when (and *plot-channel-reversal-potentials-p
					(not (eq *plot-channel-reversal-potentials* 'all)))
			       (push (channel-name elt) *plot-channel-reversal-potentials*))
			     (when (and *plot-channel-conductances-p (not (eq *plot-channel-conductances* 'all)))
			       (push (channel-name elt) *plot-channel-conductances*)))
		    (synapse (when (not (eq *plot-synapse-currents* 'all))
			       (push (synapse-name elt) *plot-synapse-currents*))
			     (when (not (eq *plot-synapse-conductances* 'all))
			       (push (synapse-name elt) *plot-synapse-conductances*)))
		    (conc-int (when (and *plot-shell-1-concentrations-p (not (eq *plot-conc-1-ints* 'all)))
				(push (conc-int-name elt) *plot-conc-1-ints*))
			      (when (and *plot-shell-2-concentrations-p (not (eq *plot-conc-2-ints* 'all)))
				(push (conc-int-name elt) *plot-conc-2-ints*))
			      (when (and *plot-shell-3-concentrations-p (not (eq *plot-conc-3-ints* 'all)))
				(push (conc-int-name elt) *plot-conc-3-ints*))
			      (when (and *plot-concentrations-p (not (eq *plot-conc-ints* 'all)))
				(push (conc-int-name elt) *plot-conc-ints*)))
		    (conc-particle (when (not (eq *plot-conc-particles* 'all))
				     (push (conc-particle-name elt) *plot-conc-particles*)))
		    (particle (when (and *plot-particles-p (not (eq *plot-particles* 'all)))
				(push (particle-name elt) *plot-particles*)))))))))


;;; CHOOSE-PLOT-DATA First checks to see if any of the global plot list variables is set to 'ALL
;;; - if so then that variable is set to a list of names of all instances of the appropriate circuit
;;; element. Then, modifies various plot lists according to *PLOT-NODE-ELEMENTS*. The global plot
;;; lists are then checked to remove any names that do not refer to actual circuit element
;;; instances, and remove any duplicate entries.

;;; *PLOT-LISTS-INFO* global variable structure:
;;;
;;; global-var-list-of-structure-names list-of-tables  structure-slot enable-var global-var-list-of-structures
;;; plot-y-label

;;; Note that macros access these different posistions in *PLOT-LISTS-INFO* sublists.

;;; Lists of appropriate structures are then generated, from which the actual data saving is referenced.
(defun choose-plot-data ()
  (loop for plot-list-info in *plot-lists-info* do
	(if (eq (symbol-value (plot-list-info-names plot-list-info)) 'all) ; GLOBAL-VAR-LIST-OF-STRUCTURE-NAMES
	    (set (car plot-list-info)
		 (loop for table in (plot-list-info-tables plot-list-info) ; LIST-OF-TABLES
		       nconcing (list-all-hash-names table)))))
  (parse-plot-node-elements)

  (loop for plot-list-info in *plot-lists-info* do
	(set (plot-list-info-names plot-list-info) ; GLOBAL-VAR-LIST-OF-STRUCTURE-NAMES
	     (delete-duplicates
	      (remove nil (loop for elt-name in (symbol-value (plot-list-info-names plot-list-info)) collecting
				(when
				    (and
				     ;; Test to filter out non event-driven elements from event plotting.
				     (or (not (eq 'event (plot-list-info-structure-slot plot-list-info)))
					     (event-driven-element-p elt-name))
				     ;; Named element is a member of the right hash-table.
					 (loop for table in (plot-list-info-tables plot-list-info) ; LIST-OF-TABLES
					       do (if (gethash (element-name elt-name) table) (return t))))
				  (element-name elt-name))))
	      :test #'equal)))
  
  (init-plot-lists-cells-and-types)
  ;; Transfer the names in the *plot-xx* lists to the corresponding structures in *plot-xx-structures* lists
  (loop for plot-list-info in *plot-lists-info* do
	(set (plot-list-info-structures plot-list-info)	; GLOBAL-VAR-LIST-OF-STRUCTURES
	     (when (symbol-value (plot-list-info-enable-var plot-list-info)) ; ENABLE-VAR
	       (loop for name in (symbol-value (plot-list-info-names plot-list-info)) ; GLOBAL-VAR-LIST-OF-STRUCTURE-NAMES
		     collect
		     (loop for table in (plot-list-info-tables plot-list-info)
			   when (gethash name table)
			   do (return
			       (let* ((element (gethash name table))
				      (element-cell (element-cell element)))
				 (case (plot-list-info-names plot-list-info)
				   (*plot-conc-1-ints* (setq name (conc-int-shell-1-trace-label name)))
				   (*plot-conc-2-ints* (setq name (conc-int-shell-2-trace-label name)))
				   (*plot-conc-3-ints* (setq name (conc-int-shell-3-trace-label name)))
				   (*plot-conc-ints* (setq name (conc-int-total-trace-label name))))
				 (update-plot-lists-cells-and-types element-cell name)
				 element)))))))
  
  (setq *analysis-nodes-structures* (loop for name in *analysis-nodes* collect (element-cell-element name)))
  (setq *all-save-voltage-nodes*
	(delete-duplicates (loop for struct-list in (list *plot-nodes-structures*
							  *plot-path-nodes-structures*
							  *PLOT-soma-NODES-STRUCTURES*
							  *analysis-nodes-structures*)
				 nconc (copy-list struct-list)))
	*all-save-dvdt-nodes*
	(loop for struct-list in (list *plot-node-derivatives-structures* *plot-soma-node-derivatives-structures*)
	      nconc (copy-list struct-list)))
  (setup-models-output-data-enabled)
  (prepare-plot-file-output)		; Prepare pointers for possible data files.
  nil)

;;; PREPARE-PLOT-FILE-OUTPUT This updates *FILE-OUTPUT-VARIABLE-LIST* according to the output pointer lists, for
;;; writing plot data to a file.
(defun prepare-plot-file-output ()
  (setq *file-output-variable-list* '())
  (loop for plot-list-info in *plot-lists-info* do
	(loop for element in (symbol-value (plot-list-info-structures plot-list-info))
	      do
	      ;; Concantenate data list information onto *FILE-OUTPUT-VARIABLE-LIST* that will be dumped to file.
	      (let ((name (element-name element))
		    (data-slot (plot-list-info-structure-slot plot-list-info)))
		(unless
		    (typecase name
		      (string (and (member name *plot-path-nodes* :test 'equal)
				   (not (member name *analysis-nodes* :test 'equal))))
		      (t (and (member name *plot-path-nodes*)
			      (not (member name *analysis-nodes*)))))
		  (push (list (create-output-symbol *simulation-name* t name data-slot)
			      (element-name element)
			      data-slot)
			*FILE-OUTPUT-VARIABLE-LIST*)))))
  nil)



(defun init-plot-lists-cells-and-types ()
  (setq *plot-lists-cells* nil *plot-lists-cell-types* nil))


(defun update-plot-lists-cells-and-types (cell name)
  (when cell
    (setq *plot-lists-cells* (acons name cell *plot-lists-cells*)
	  *plot-lists-cell-types* (acons name (cell-type cell) *plot-lists-cell-types*))))


(defun conc-int-shell-1-trace-label (name) (format nil "~A-1" name))
  
(defun conc-int-shell-2-trace-label (name) (format nil "~A-2" name))

(defun conc-int-shell-3-trace-label (name) (format nil "~A-3" name))

(defun conc-int-total-trace-label (name) (format nil "~A-total" name))


(defun name-list-from-structures (structures)
  (loop for structure in structures collect
	(funcall (read-from-string (format nil "~S-NAME" (type-of structure))) structure)))


(defun slot-descriptor-string (table slot)
  (string-capitalize
   (REPLACE-CHAR-W-SPACE-IN-STRING 
    (format nil "~S ~s" (loop for struct being the hash-value of (typecase table
								   (symbol (symbol-value table))
								   (t table))
			      do (return (type-of struct))) slot)
    #\-)))


(defun menu-to-clear-plot-lists ()
  (let ((menu-list) (dummy27 nil))
    (loop for plot-list-info in *plot-lists-info*
	  for dummy-sym in '(dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 
			     dummy11 dummy12 dummy13 dummy14 dummy15 dummy16 dummy17 dummy18 dummy19 dummy20 dummy21 
			     dummy22 dummy23 dummy24 dummy25 dummy26)
	  when (symbol-value (plot-list-info-names plot-list-info))
	  do (setf (symbol-value dummy-sym) nil)
	  (push (list dummy-sym
		      (format nil "Clear all plotted ~A~As"
			      (if (eq (first plot-list-info) '*plot-path-nodes*) "Path " "")
			      (slot-descriptor-string (car (plot-list-info-tables plot-list-info))
						      (plot-list-info-structure-slot plot-list-info)))
		      :boolean) menu-list))
    (when *plot-node-elements* (push '(dummy27 "Clear nodes for which all elements are plotted" :boolean) menu-list))
    (choose-variable-values  menu-list  ':label "Clear plotting lists")
    (loop for plot-list-info in *plot-lists-info*
	  for dummy-sym in '(dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 
			     dummy11 dummy12 dummy13 dummy14 dummy15 dummy16 dummy17 dummy18 dummy19 dummy20 dummy21 
			     dummy22 dummy23 dummy24 dummy25 dummy26)
	  when (and (symbol-value dummy-sym) (symbol-value (plot-list-info-names plot-list-info)))
	  do (setf (symbol-value (plot-list-info-names plot-list-info)) nil))
    (if dummy27 (setq *plot-node-elements* nil))))


;; PLOT-ALL-SOMAS Plot all the soma voltages.
(defun plot-all-somas ()
  (enable-element-plot (somas)))


;;; CHOOSE-PLOT-ELEMENTS-MENU
(defun choose-plot-elements-menu ()
  (when (and (>= (hash-table-count (SOMA-HASH-TABLE)) 4)
	     (go-ahead-menu "Clear all plotted nodes and just get somas on the diagonal"))
    (PICK-DIAGONAL-SOMAS-FOR-PLOTTING))
  (parse-plot-node-elements)
  ;; Set the flags that authorizes plotting of the various variable classes if each flag is already
  ;; set and there is an element in the circuit that can generate the variable class.
  (loop for plot-list-info in *plot-lists-info*
	when (symbol-value (plot-list-info-enable-var plot-list-info))
	do (set (plot-list-info-enable-var plot-list-info)
		(true-p (and (symbol-value (plot-list-info-enable-var plot-list-info))
			   (non-empty-hash-tables (plot-list-info-tables plot-list-info))))))
  (let ((plot-menu-class-enable
	 (choose-list-values-from-keys
	  (loop for plot-list-info in *plot-lists-info* 
		when (and
		      (not (eq '*PLOT-PATH-NODES* (plot-list-info-names plot-list-info)))
		      (= (length (plot-list-info-tables plot-list-info)) 1)
		      (symbol-value (plot-list-info-enable-var plot-list-info))
		      (non-empty-hash-tables (plot-list-info-tables plot-list-info)))
		collect
		(let ((slot-descriptor-string (slot-descriptor-string (car (plot-list-info-tables plot-list-info))
								      (plot-list-info-structure-slot plot-list-info)))
		      (num (loop for table in (plot-list-info-tables plot-list-info)
				 sum (if (eq 'MARKOV-STATE (plot-list-info-structure-slot plot-list-info))
				       (loop for thing being the hash-value of table
					     when (and (particle-p thing) (eq (particle-type-class (particle-type thing)) :markov))
					     sum 1)
				       (HASH-TABLE-count table)))))
					; Not sure why i need SYMBOL-VALUE here
		  ; (format t "slot-descriptor-string ~A, slot ~A~%" slot-descriptor-string (plot-list-info-structure-slot plot-list-info))
		  (list (format nil "~A (~a instance~:p)" slot-descriptor-string num)
			slot-descriptor-string)))
	  nil
	  :do-all-at-once t :rank-margin 1 :direction :horizontal :label "Which classes need plot modification?")))
    (plot-selection-menus plot-menu-class-enable)))


(defun plot-menu-class-enable (element &optional type)
  (let ((hash (element-hash-table element type)))
    (loop for plot-list-info in *plot-lists-info* 
	  when (loop for sym in (plot-list-info-tables plot-list-info)
		     when (eq hash sym) do (return t))
	  collect (slot-descriptor-string (car (plot-list-info-tables plot-list-info))
					  (plot-list-info-structure-slot plot-list-info)))))


(defun plot-element-menu (&optional element type)
  (element-wrapper
   (element type "Plot Characteristics")
   (plot-selection-menus (plot-menu-class-enable elt type) elt type)))


(defun plot-selection-menus (plot-menu-class-enable &optional element type)	
  ;; For each variable class for which plotting is enabled, generate a selection menu for all the
  ;; instances of the variable class.
  (let ((element (element element type)) slot-descriptor-string slot)
    (loop for plot-list-info in *plot-lists-info*
	  do (setq slot (plot-list-info-structure-slot plot-list-info)
		   slot-descriptor-string (slot-descriptor-string (car (plot-list-info-tables plot-list-info)) slot))
	  when (and (member slot-descriptor-string plot-menu-class-enable :test 'equal)
		    (or element (symbol-value (plot-list-info-enable-var plot-list-info)))
		    (not (eq '*PLOT-PATH-NODES* (plot-list-info-names plot-list-info)))
		    (non-empty-hash-tables (plot-list-info-tables plot-list-info)))
	  do (if element
	       (let* ((info-names (symbol-value (plot-list-info-names plot-list-info)))
		      (already-there (member (element-name element) info-names)))
		 (set (plot-list-info-names plot-list-info)
		      (if (go-ahead-menu (format nil "Select ~a for Plotting" slot-descriptor-string)
					 (format nil "Plotting for ~A" (element-name element)) already-there)
			(if already-there info-names (cons (element-name element) info-names))
			(remove (element-name element) info-names))))
	       (set (plot-list-info-names plot-list-info)
		    (reorder-names-by-type
		     (select-hash-values-menu
		      (non-empty-hash-tables (plot-list-info-tables plot-list-info))
		      (format nil "Select ~as for Plotting" slot-descriptor-string)
		      :max-per-menu 30 :direction :horizontal :rank-margin 2 
		      :inclusion-key (when (eq 'MARKOV-STATE slot) 'MARKOV-PARTICLE-P)
		      :selected-list (if (eq (symbol-value (plot-list-info-names plot-list-info)) 'all)
				       (loop for table in (non-empty-hash-tables (plot-list-info-tables plot-list-info))
					     collect (list-all-hash-names table))
				       (symbol-value (plot-list-info-names plot-list-info))))))))))
		      


;; possible bug in element-type 1/17/98
(defun reorder-names-by-type (list-of-names)
  (let ((types-in-names
	 (delete-duplicates (loop for name in list-of-names collect (element-type name)))))
    (loop for type in types-in-names
	  nconcing (loop for name in list-of-names when (eq type (element-type name)) collect name))))
			 

(defun plot-archive-vars ()
  (setq *ARCHIVE-VARIABLE-LIST* (remove-duplicates *ARCHIVE-VARIABLE-LIST* :key 'car)) ; Do this here, anyway.
  (loop for simulation in
	(CHOOSE-LIST-VALUES-FROM-KEYS (loop for simulation in *ARCHIVE-VARIABLE-LIST*
					    collect (list (car simulation) simulation)) nil
					    :label "Choose Archived Simulations to Plot")
	do
	(let* (*group-plot-data-by-cell-type*
	       *group-plot-data-by-cell*
	       (*simulation-name* (ARCHIVE-VARIABLE-LIST-SIMULATION-NAME simulation))
	       (*sim-reverse-plot-time-list* (ARCHIVE-VARIABLE-LIST-SIMULATION-TIME simulation))
	       (menu-list (loop for symbol-and-type in
				(ARCHIVE-VARIABLE-LIST-SIMULATION-symbols-and-data-types simulation)
				collect
				(list (ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL-descriptor symbol-and-type)
				      symbol-and-type)))
	       (next-plots		; Plotted-symbols-and-types
		(CHOOSE-LIST-VALUES-FROM-KEYS menu-list nil
					      :label (format nil "Choose Data from Simulation ~a" *simulation-name*)))
	       this-plot this-type)
	  (loop until (null next-plots)	do
		(setq this-type (cadar next-plots))
		(let ((this-and-next
		       (loop for symbol-and-type in next-plots
			     when (eq this-type (cadr symbol-and-type))
			     collect symbol-and-type into this-plot
			     else
			     collect symbol-and-type into next-plots
			     finally (return (list this-plot next-plots)))))
		  (setq next-plots (cadr this-and-next))
		  (setq this-plot (car this-and-next))
		  (setq this-type (cadar this-plot))
		  (surf-plot (loop for symbol-and-type in this-plot
				   collect (ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL-VALUE symbol-and-type))
			     (loop for symbol-and-type in this-plot collect
				   (string-remove-head (string (ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL symbol-and-type))
						       (1+ (length *simulation-name*))))
			     (string this-type)
			     :x-max nil :x-min nil :x-label "ms" :y-label (plot-y-label-from-data-type this-type)))))))


(defun ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL-descriptor (symbol-and-type)
  (let ((symbol (ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL symbol-and-type))
	(data-type (ARCHIVE-VARIABLE-LIST-SIMULATION-DATA-TYPE symbol-and-type)))
    (format nil "~a (~a)"
	    (string-remove-head (string symbol)
				(1+ (length *simulation-name*)))
	    (string-head		; remove "-data"
	     (string-downcase data-type)
	     (- (length (string data-type))
		(if (search "-data" (string-downcase data-type))
		    5 0))))))


(defun plot-y-label-from-data-type (data-type)
  (case data-type
    ((voltage soma-voltage-data segment-voltage-data)  "mV")
    ((voltate-derivative soma-voltage-derivative-data segment-voltage-derivative-data) "mV/ms")
    (axon-voltage-data "mV")
    ((current isource-current-data vsource-current-data
	      soma-dendrite-current-data synapse-current-data channel-current-data) "nA")
    ((conductance channel-conductance-data synapse-conductance-data) "uS")
    ((concentration 1 2 3 total
		    conc-int-concentration-1-data
		    conc-int-concentration-2-data
		    conc-int-concentration-3-data
		    conc-int-total-concentration-data) "mM")
    ((state conc-particle-state-data particle-state-data) "State")))
     

(defun choose-custom-plot-menu ()
  (setq *custom-plot-lists*
	(remove nil
		(loop for plot-list-info in *plot-lists-info* collecting
		      (let* ((slot-descriptor-string
			      (slot-descriptor-string (car (plot-list-info-tables plot-list-info))
						      (plot-list-info-structure-slot plot-list-info)))
			     (select-list
			      (select-hash-values-menu
			       (plot-list-info-tables plot-list-info)
			       (format nil "Select ~as for custom plotting" slot-descriptor-string))))
			(when select-list (list (list select-list slot-descriptor-string))))))))


(defun event-plotter ()
  (cond-every
   ((and *plot-synapse-events-structures* *plot-synapse-events-p)
    (when *plot-event-generators*
      (raster-plots :event-elements *plot-synapse-events-structures*
		    :title-postfix "Synapse"
		    :only-event-generators t))
    (when *plot-events*
      (raster-plots :event-elements *plot-synapse-events-structures*
		    :title-postfix "Synapse"
		    :only-event-generators nil)))
   ((and *plot-axon-events-structures* *plot-axon-events-p)
    (when *plot-event-generators*
      (raster-plots :event-elements *plot-axon-events-structures*
		    :title-postfix "Axon"
		    :only-event-generators t))
    (when *plot-events*
      (raster-plots :event-elements *plot-axon-events-structures*
		    :title-postfix "Axon"
		    :only-event-generators nil)))))

(defvar *voltage-plot-waterfall-x-offset* 0.0)
(defvar *voltage-plot-waterfall-y-offset* 50.0)
(defvar *voltage-plot-waterfall* nil)
(defvar	*AUTO-PLOT-WATERFALL* t)

(defvar *normalized-gbar-label* (format nil "rel.~%gbar"))

(defun massage-element-plot-label (element &optional type)
  (if (not *massage-element-plot-labels*)
    element
    (let* ((elt (typecase type
		  (cons (loop for typ in type
			      when (element element typ) ; (equal (type-of (element element typ)) typ)
			      return (element element typ)))
		  (t (element element type))))
	   (print-name (element-name elt)))
      (if (not print-name)
	element
	(let ((add-cell-name (HASH-TABLE-HAS-MORE-THAN-ONE-VALUE-P (cell-hash-table))))
	  (if (numberp print-name)
	    (typecase elt
	      (soma (format nil "Soma ~D~A" print-name (if add-cell-name (format nil " (Cell ~A)" (cell-name (soma-cell elt))) "")))
	      (segment (format nil "Seg ~D~A" print-name
			       (if add-cell-name (format nil " (Cell ~A)" (cell-name (segment-cell elt))) "")))
	      ((or channel synapse particle conc-particle pump buffer conc-int)
	       (let ((cell-elt (element-cell-element elt)))
		 (format nil "~A ~D (~A ~A~A)"
			 (typecase elt
			   (synapse "Syn")
			   (channel "Chan")
			   (particle "Part")
			   (conc-particle "ConcPart")
			   (pump "Pump")
			   (buffer "Buffer")
			   (conc-int "CincInt"))
			 print-name
			 (typecase cell-elt
			   (soma "Soma")
			   (segment "Seg"))
			 (element-name cell-elt)
			 (if add-cell-name (format nil ", Cell ~A" (cell-name (element-cell elt))) ""))))
	      (t element))
	    print-name))))))

(defun massage-element-plot-labels (elements &optional type)
  (loop for elt in (coerce-to-list elements)
	collect (massage-element-plot-label elt type)))
	


(defun plot-channel-conductances ()
  (plot-memb-elt-data-type '*plot-channel-conductances*))

(defun plot-channel-reversal-potentials ()
  (plot-memb-elt-data-type '*plot-channel-reversal-potentials*))

(defun plot-channel-currents ()
  (plot-memb-elt-data-type '*plot-channel-currents*))

(defun plot-synapse-conductances ()
  (plot-memb-elt-data-type '*plot-synapse-conductances*))

(defun plot-synapse-reversal-potentials ()
  (plot-memb-elt-data-type '*plot-synapse-reversal-potentials*))

(defun plot-synapse-currents ()
  (plot-memb-elt-data-type '*plot-synapse-currents*))

;; possible bug in element 1/17/98 (simple-names)
;; perhaps fixed 5/5/98
(defun plot-memb-elt-data-type (plot-list-names-symbol)
  (let* ((plot-list-info (find plot-list-names-symbol *plot-lists-info* :key 'car))
	 (plot-list-info-types (plot-list-info-types plot-list-info)) 
	 (plotted-elements (loop for type in plot-list-info-types ; Parse with ELEMENT using the
								  ; TYPE arg.
				 when (element (symbol-value plot-list-names-symbol) type)
				 do (return (element (symbol-value plot-list-names-symbol) type))))
	 (element-type-symbol (type-of (car plotted-elements)))
	 (data-type (plot-list-info-structure-slot plot-list-info))
	 k-elements na-elements ca-elements other-elements)
    (loop for element in plotted-elements do
	  (case (and (case element-type-symbol
		       (channel *plot-channels-by-major-ion*)
		       (synapse *plot-synapses-by-major-ion*))
		     (element-major-ion element element-type-symbol))
	    (k (push element k-elements))
	    (na (push element na-elements))
	    (ca (push element ca-elements))
	    (t  (push element other-elements))))
    (loop for elements in (list k-elements na-elements ca-elements other-elements)
	  for ion in '("K+ " "Na+ " "Ca++ " "")
	  when elements do
	  (surf-plot (retrieve-plot-data (list (list elements data-type)))
		     elements 
		     (format nil "~A~A" ion (slot-descriptor-string (get-type-hash-table element-type-symbol) data-type))
		     :element-type element-type-symbol
		     :y-min (case data-type
			      (conductance 0.0)
			      (t nil))
		     :x-label "ms"
		     :y-label (case data-type
				(conductance (if *SAVE-CONDUCTANCES-normalized*
						 *normalized-gbar-label* 
					       (plot-list-info-units-label plot-list-info)))
				(t (plot-list-info-units-label plot-list-info)))))))

(defun plot-particles ()
  (plot-particles-core *plot-particles*)
  (plot-particles-core *plot-markov-particles* t))

(defun plot-particles-core (particles-to-plot &optional markov)
  (let (k-particles na-particles ca-particles other-particles)
    (loop for particle-name in particles-to-plot do
	  (case (and *plot-channels-by-major-ion* (element-major-ion particle-name 'particle))
	    (k (push particle-name k-particles))
	    (na (push particle-name na-particles))
	    (ca (push particle-name ca-particles))
	    (t  (push particle-name other-particles))))
    (loop for particles in (list k-particles na-particles ca-particles other-particles)
	  for ion in '("K+ " "Na+ " "Ca++ " "")
	  when particles do
	  (loop for prt in (element particles 'particle)
		when (and markov (eq :markov (particle-type-class (element-type prt))))
		collect prt into markov-particles
		else
		unless markov
		collect (element prt 'particle) into two-state-particles
		finally
		(cond-every
		 (two-state-particles
		  (surf-plot (retrieve-plot-data (list (list two-state-particles `state)))
			     two-state-particles 
			     (format nil "~aChannel Particle States" ion)
			     :element-type 'particle
			     :x-label "ms" :y-label "State" :y-min 0 :y-max 1))
		 (markov-particles
		  (plot-markov-particles markov-particles)))))))

(defun plot-markov-particles (markov-particles)
  (loop for prt in markov-particles do
	(let ((prt (element prt 'particle)))
	  (when t ;(element-parameter prt 'plot-markov-states)
	    (loop for state-index fixnum from 0 to (nb-states-1 prt)
		  for label in (element-parameter (element-type prt) 'markov-state-labels)
		  collect (find-markov-particle-state-data prt state-index) into data-lists
		  collect (format nil "~A ~A" (massage-element-plot-label prt 'particle) label)
		  into label-list
		  finally
		  (plot-timed-data data-lists label-list
				   *sim-plot-time-list*
				   :title (format nil "~a Markov Particle" (channel-name (particle-channel prt)))
				   :x-label "ms" :y-label "State" :y-min 0 :y-max 1))))))

(defun plot-conc-particles ()
  (let (k-conc-particles na-conc-particles ca-conc-particles other-conc-particles)
    (loop for conc-particle-name in *plot-conc-particles* do
	  (case (and *plot-channels-by-major-ion* (element-major-ion conc-particle-name 'conc-particle))
	    (k (push conc-particle-name k-conc-particles))
	    (na (push conc-particle-name na-conc-particles))
	    (ca (push conc-particle-name ca-conc-particles))
	    (t  (push conc-particle-name other-conc-particles))))
    (loop for conc-particles in (list k-conc-particles na-conc-particles ca-conc-particles other-conc-particles)
	  for ion in '("K+ " "Na+ " "Ca++ " "")
	  when conc-particles do
	  (surf-plot (retrieve-plot-data (list (list (element conc-particles 'conc-particle) `state)))
		     conc-particles 
		     (format nil "~aChannel Concentration Particle States" ion)
		     :element-type 'conc-particle
		     :x-label "ms"
		     :y-label "State" :y-min 0 :y-max 1))))

(defun plot-vsource-currents ()
  (surf-plot (retrieve-plot-data (list (list *plot-vsource-currents-structures* `current)))
	      *plot-vsource-currents*
	     "Voltage Clamp Currents" :element-type 'vsource :y-label "nA" :x-label "ms"))

(defun plot-vsource-voltages ()
  (surf-plot (retrieve-plot-data (list (list *plot-vsource-voltages-structures* `voltage)))
	      *plot-vsource-voltages*
	     "Voltage Clamp Voltages" :element-type 'vsource :y-label "mV" :x-label "ms"))


(defun plot-custom-windows ()
  (loop for custom-plot-list in *custom-plot-lists* do
	(surf-plot (retrieve-plot-data custom-plot-list)
		   (caar custom-plot-list)
		   "Data Plot"
		   :y-label (let ((units (element-data-units (caar custom-plot-list))))
			      (typecase units
				(cons (car units))
				(t units)))
		   :x-label "ms")))

(defun plot-field-potentials ()
  (surf-plot
   (retrieve-plot-data (list (list *plot-extracellular-electrodes-structures* 'field-potential)))
   *plot-field-potentials*
   "Extracellular Field Potentials"
   :x-label "ms" :y-label "mV" :group-by-cell nil :group-by-cell-type nil))





(defun plot-soma-voltages ()
  (surf-plot
   (retrieve-plot-data (list (list *plot-soma-nodes-structures* `voltage)))
    *plot-soma-nodes* 
   (format nil "~AVoltage" (if *plot-separate-soma-window "Soma " ""))
   :element-type 'soma
   :waterfall *voltage-plot-waterfall*
   :waterfall-x-offset *voltage-plot-waterfall-x-offset*
   :waterfall-y-offset *voltage-plot-waterfall-y-offset*
   :x-label "ms" :y-label "mV"
   :y-min (if (not *automatic-voltage-plot-scaling) *soma-voltage-plot-min)
   :y-max (if (not *automatic-voltage-plot-scaling) *soma-voltage-plot-max)))

(defun plot-soma-derivatives ()
  (surf-plot (retrieve-plot-data (list (list *plot-soma-node-derivatives-structures* `voltage-derivative)))
	     *plot-soma-node-derivatives*
	     (format nil "~AVoltage Derivative" (if *plot-separate-soma-window "Soma " ""))
	     :x-label "ms" :y-label "mV/ms"))

(defun plot-node-voltages ()
  (surf-plot
   (retrieve-plot-data (list (list *plot-soma-nodes-structures* `voltage) (list *plot-nodes-structures* `voltage)))
   (concatenate 'list *plot-soma-nodes* *plot-nodes*)
   "Voltages" :y-label "mV"  :element-type '(segment soma) :x-label "ms"
   :waterfall *voltage-plot-waterfall*
   :waterfall-x-offset *voltage-plot-waterfall-x-offset*
   :waterfall-y-offset *voltage-plot-waterfall-y-offset*
   :y-min (if (not *automatic-voltage-plot-scaling) *voltage-plot-min)
   :y-max (if (not *automatic-voltage-plot-scaling) *voltage-plot-max)))

(defun plot-path-node-voltages ()
  (let ((cells-in-data (delete-duplicates (element-cell *plot-path-nodes-structures*))))
    (loop for cell in cells-in-data do
	  (let* ((this-path (reverse (loop for thing in *plot-path-nodes-structures*
					   when (eq cell (element-cell thing)) collect thing)))
		 (*traces-per-plot* (length this-path))
		 (*auto-waterfall-y-trace-overlap* 0.5))
	    (surf-plot
	     (retrieve-plot-data (list (list this-path `voltage)))
	     (element-name this-path)
	     (format nil "Voltages From ~A to ~a Soma" (element-name (car this-path)) (cell-name cell))
	     :y-label "mV" :x-label "ms"
	     :waterfall-x-offset *voltage-plot-waterfall-x-offset* :waterfall-y-offset *voltage-plot-waterfall-y-offset*
	     :waterfall t :label-waterfall t
	     :y-min (if (not *automatic-voltage-plot-scaling) *voltage-plot-min)
	     :y-max (if (not *automatic-voltage-plot-scaling) *voltage-plot-max))))))

(defun plot-node-voltage-derivatives ()
  (surf-plot
   (retrieve-plot-data (list (list *plot-soma-node-derivatives-structures* `voltage-derivative)
			     (list *plot-node-derivatives-structures* `voltage-derivative)))
   (concatenate 'list *plot-soma-node-derivatives* *plot-node-derivatives*)
   "Voltage Derivatives" :y-label "mV/ms" :x-label "ms"))

(defun plot-axon-voltages ()
  (surf-plot
   (retrieve-plot-data (list (list *plot-axons-structures* `voltage)))
   *plot-axons* 
   "Axon Output Voltages" :element-type 'axon :y-label "mV" :x-label "ms"
   :y-min (if (not *automatic-voltage-plot-scaling) *voltage-plot-min)
   :y-max (if (not *automatic-voltage-plot-scaling) *voltage-plot-max)))

(defun plot-isource-currents ()
  (surf-plot (retrieve-plot-data (list (list *plot-isource-currents-structures* `current)))
	     *plot-isource-currents* "Current Sources" :element-type 'isource  :y-label "nA" :x-label "ms"))

(defun plot-pump-currents ()
  (surf-plot (retrieve-plot-data (list (list *plot-pumps-structures* `current)))
	     *plot-pump-currents* "Membrane Ion Pumps"
	      :element-type 'pump :y-label "mM/ms" :x-label "ms"))

(defun plot-channel-and-soma-dendrite-currents ()
  (cond (*plot-channels-by-major-ion*
	 (cond-every
	  (*plot-channel-currents-p (plot-channel-currents))
	  (*plot-soma-dendrite-currents-p
	   (surf-plot (retrieve-plot-data (list (list *plot-soma-dendrite-currents-structures* `dendrite-current)))
		      *plot-soma-dendrite-currents* "Soma-Dendrite Currents" :y-label "nA" :x-label "ms"))))
	((and *plot-channel-currents-p *plot-soma-dendrite-currents-p)
	 (surf-plot
	  (retrieve-plot-data (list (list *plot-channel-currents-structures* `current)
				    (list *plot-soma-dendrite-currents-structures* `dendrite-current)))
	  (concatenate 'list *plot-channel-currents* *plot-soma-dendrite-currents*)
							
	  (concatenate-strings
	   (when *plot-channel-currents* "Channel ")
	   (when (and *plot-channel-currents* *plot-soma-dendrite-currents*) "and ")
	   (when *plot-soma-dendrite-currents* "Soma-Dendrite ")
	   "Currents")
	  :y-label "nA" :x-label "ms"))
	(*plot-channel-currents-p (plot-channel-currents))
	(*plot-soma-dendrite-currents-p
	 (surf-plot (retrieve-plot-data (list (list *plot-soma-dendrite-currents-structures* `dendrite-current)))
		    *plot-soma-dendrite-currents*
		    "Soma-Dendrite Currents" :y-label "nA" :x-label "ms"))))


;;; SURF-PLOTTER 
(defun surf-plotter ()
  (when *save-simulation-data*
    (when *plot-custom-windows* (plot-custom-windows))
    (when *plot-standard-windows*
      (cond-every
       (*plot-field-potentials-p (plot-field-potentials))
       ((and *plot-soma-voltage-p (or *plot-separate-soma-window (not *plot-node-voltages-p))) (plot-soma-voltages))
       ((and *plot-soma-voltage-derivative-p (or *plot-separate-soma-window (not *plot-node-voltage-derivatives-p)))
	(plot-soma-derivatives))
       (*plot-node-voltages-p (plot-node-voltages))
       (*plot-path-node-voltages-p (plot-path-node-voltages))
       (*plot-node-voltage-derivatives-p (plot-node-voltage-derivatives))
       (*plot-axon-voltages-p (plot-axon-voltages))
       (*plot-isource-currents-p (plot-isource-currents))
       (*plot-pump-currents-p (plot-pump-currents))
       ((or *plot-channel-currents-p *plot-soma-dendrite-currents-p) (plot-channel-and-soma-dendrite-currents))
       (*plot-synapse-currents-p (plot-synapse-currents))
       (*plot-vsource-currents-p (plot-vsource-currents))
       (*plot-vsource-voltages-p (plot-vsource-voltages))
       
       ((or *plot-particles-p *plot-markov-particles-p) (plot-particles))
       (*plot-conc-particles-p (plot-conc-particles))	      
       (*plot-channel-conductances-p (plot-channel-conductances))
       (*plot-channel-reversal-potentials-p (plot-channel-reversal-potentials))
       (*plot-synapse-reversal-potentials-p (plot-synapse-reversal-potentials))
       (*plot-synapse-conductances-p (plot-synapse-conductances))
       (*PLOT-TOTAL-CONDUCTANCES-P (total-conductance-plotter))
       ((or *plot-shell-1-concentrations-p *plot-shell-2-concentrations-p
	    *plot-shell-3-concentrations-p *plot-concentrations-p)
	(concentration-plotter))
       ((or *plot-synapse-events-p *plot-axon-events-p) (event-plotter))))    
    (setq *CREATE-NEW-SIMULATION-PLOTS* nil)
    (when *store-plot-results-to-folder* (traces-to-folder t))
    (when *hard-copy-screen* (HARD-COPY-SCREEN))))

(defun concentration-plotter ()
  (let ((data-keys (list (no-nils (list (when *plot-shell-1-concentrations-p
					  (list *plot-conc-1-ints-structures* `concentration-1))
					(when *plot-shell-2-concentrations-p
					  (list *plot-conc-2-ints-structures* `concentration-2))
					(when *plot-shell-3-concentrations-p
					  (list *plot-conc-3-ints-structures* `concentration-3))
					(when (and *plot-concentrations-p (not *PLOT-TOTAL-CONCS-SEPARATELY*))
					  (list *plot-conc-ints-structures* `total-concentration))))
			 (when (and *plot-concentrations-p *PLOT-TOTAL-CONCS-SEPARATELY*)
			   (list (list *plot-conc-ints-structures* `total-concentration)))))
	(labels (list (nconc (when *plot-shell-1-concentrations-p
			       (loop for name in *plot-conc-1-ints* collecting (conc-int-shell-1-trace-label name)))
			     (when *plot-shell-2-concentrations-p
			       (loop for name in *plot-conc-2-ints* collecting (conc-int-shell-2-trace-label name)))
			     (when *plot-shell-3-concentrations-p
			       (loop for name in *plot-conc-3-ints* collecting (conc-int-shell-3-trace-label name)))
			     (when (and *plot-concentrations-p (not *PLOT-TOTAL-CONCS-SEPARATELY*))
			       (loop for name in *plot-conc-ints* collecting (conc-int-total-trace-label name))))
		      (when (and *plot-concentrations-p *PLOT-TOTAL-CONCS-SEPARATELY*)
			(loop for name in *plot-conc-ints* collecting (conc-int-total-trace-label name)))))
	(only-ion
	 (loop for conc-int in
	       (flatten-no-nils-list (when *plot-shell-1-concentrations-p *plot-conc-1-ints-structures*)
				     (when *plot-shell-2-concentrations-p *plot-conc-2-ints-structures*)
				     (when *plot-shell-3-concentrations-p *plot-conc-3-ints-structures*)
				     (when (and *plot-concentrations-p (not *PLOT-TOTAL-CONCS-SEPARATELY*))
				       *plot-conc-ints-structures*))
	       collect (conc-int-type-species (conc-int-type conc-int)) into ions
	       collect (conc-int-type-valence (conc-int-type conc-int)) into valences
	       finally (return
			(let ((all-species (delete-duplicates ions)))
			  (if (= (length all-species) 1)
			      (format nil "~A~d " (car all-species)
				      (char-seq-to-string
				       (loop for symbol-count from 1 to (abs (round (car valences)))
					     collect (if (< (round (car valences)) 0) "-" "+"))))
			    "")))))
	(title-suffix '("" " (Total)")))
    (loop for data-key-list in data-keys
	  for label-list in labels
	  for suffix in title-suffix do
	  (let ((plot-data (retrieve-plot-data data-key-list)))
	    (when plot-data
	      (surf-plot plot-data label-list
			 (format nil "~AConcentrations~A" only-ion suffix)
			 :y-label "mM" :x-label "ms" :y-min 0))))))

(defun total-conductance-plotter ()
  (when *PLOT-TOTAL-CONDUCTANCES-P
    (let (*group-plot-data-by-cell-type* *group-plot-data-by-cell*)
      (surf-plot (loop for data-list in *total-conductances-data*
		       collect (reverse data-list))
		 (plot-total-conductances-plot-labels)
		 "Total Conductances" :y-min 0.0 :y-label "uS" :x-label "ms"))))


(defun LABEL-CELL-TYPE-IN-*PLOT-LISTS-CELL-TYPES* (label)
  (cdr (typecase label
	 (string (assoc label *plot-lists-cell-types* :test 'equal))
	 (t (assoc label *plot-lists-cell-types*)))))

(defun label-cell-in-*plot-lists-cells* (label)
  (cdr (typecase label
	 (string (assoc label *plot-lists-cells* :test 'equal))
	 (t (assoc label *plot-lists-cells*)))))


(defun surf-plot-time-incs (max-time)
  (cond
    ((< max-time 5) 0.5)
    ((< max-time 10) 1.0)
    ((< max-time 20) 2.0)
    ((< max-time 40) 5.0)
    ((< max-time 100) 10.0)))

(defvar *add-trace-analysis-to-plot* nil)
(defvar *remove-initial-value-from-plot-trace-analysis-integrals* t)

(defun surf-plot-trace-analysis-strings (data-lists labels time-list)
  (loop for data in data-lists
	for label in labels
	collect (PLOT-TRACE-ANALYSIS-STRING data (when (> (length labels) 1) label) time-list
					    (if
					     *remove-initial-value-from-plot-trace-analysis-integrals* (car data) 0.0)))) 


;;;; SURF-PLOT - DATA-LISTS is a list of individual data lists. This generates multiple windows of
;;;; type PLOT-PANE-TYPE (actually, this is just a name), with a subscript, so that no more than
;;;; *TRACES-PER-PLOT* traces out of DATA-LISTS is plotted in a given window. Very neat, not crowded.
(defun surf-plot (top-data-lists top-label-list plot-pane-type &key
				 element-type
				 y-label y-min y-max (x-label "ms")
				 (enable-labels *enable-plot-labels*) (use-same-line-style *use-same-line-style*)
				 (group-by-cell *group-plot-data-by-cell*)
				 (group-by-cell-type *group-plot-data-by-cell-type*)
				 (waterfall-x-offset 0.0) (waterfall-y-offset 0.0) waterfall label-waterfall
				 (x-min *user-start-time*) (x-max *user-stop-time*) (overlay *overlay-all-plots*)
				 (update *update-plots*) (resurrect *resurrect-plots*))
  (setq overlay	(and (not (or *create-new-plot-windows* *CREATE-NEW-SIMULATION-PLOTS*)) *overlay-all-plots*))
  (let* ((*create-new-plot-windows* (or *create-new-plot-windows* *CREATE-NEW-SIMULATION-PLOTS*))
	 (*hide-output-windows* (or *hide-output-windows* *HIDE-plot-WINDOWS*))
	 (no-labels (or (not enable-labels) (not top-label-list)))
	 (top-label-list (or (coerce-to-list (element-name top-label-list))
			     (coerce-to-list top-label-list)
			     (loop for i from 1 to (length top-data-lists) collect nil)))
	 (cells-in-data (CLEAN-UP-LIST (mapcar 'label-cell-in-*plot-lists-cells* top-label-list)))
	 (cell-types-in-data (CLEAN-UP-LIST (mapcar 'LABEL-CELL-TYPE-IN-*PLOT-LISTS-CELL-TYPES* top-label-list)))
	 (*traces-per-plot* (or *traces-per-plot* 0))
	 (pane-counter (when (and (not (= 0 *traces-per-plot*))
				  (or (when group-by-cell (> (length cells-in-data) 1))
				      (> (length top-label-list) *traces-per-plot*)))
			 1)))
    (loop for cell-or-type in (or (if group-by-cell cells-in-data cell-types-in-data) (cell-types)) do
	  (loop for label in top-label-list
		for data-list in top-data-lists
		when (or (and (not group-by-cell) (not group-by-cell-type))
			 (and group-by-cell (eq cell-or-type (label-cell-in-*plot-lists-cells* label)))
			 (and group-by-cell-type (eq cell-or-type (label-cell-type-in-*plot-lists-cell-types* label))))
		collect label into label-list and collect data-list into data-lists
		finally 
		(when (>= (length data-lists) 1) ; Make sure that there is data to plot.
		  (let (pane-data-list pane-label-list)
		    (loop for data-list in data-lists
			  for label in label-list
			  for count from 1
			  for total-count from 1
			  do (push data-list pane-data-list) (push label pane-label-list)
			  when (or (= total-count (length data-lists)) (= count *traces-per-plot*))
			  do (let* ((sub-plot-pane-type
				     (if pane-counter
					 (concatenate-strings plot-pane-type (format nil " ~A" pane-counter))
					 plot-pane-type))
				    (name (concatenate-strings *simulation-name* ": " sub-plot-pane-type))
				    ;;Kill the overlay flag if there is no window to overlay.
				    (plot-pane (get-plot-window (if waterfall :waterfall :xy) sub-plot-pane-type
								overlay ; :name name
								:session-name *session-name*)))
			       (s-value plot-pane :title (GET-win-TITLE-STRING name))
			       (s-value plot-pane :scatter-symbol-units-in-pixels t)
			       (s-value plot-pane :y-symbol-width 10)
			       (s-value plot-pane :x-symbol-width 10)
			       (let* (*create-new-plot-windows* ; Suppress this since the earlier
					; call to get-plot-window will reflect this.
				      (labels (unless no-labels
						(massage-element-plot-labels (reverse pane-label-list) element-type)))
				      (data-lists (reverse pane-data-list))
				      (time-list (current-sim-plot-time-list))
				      (*simulation-plot-window-comment*
				       (if *add-trace-analysis-to-plot*
					   (concatenate-string-list
					    (cons *simulation-plot-window-comment*
						  (surf-plot-trace-analysis-strings data-lists labels time-list))
					    :lf-count 1)
					   *simulation-plot-window-comment*)))

				 (plot-timed-data data-lists
						  labels
						  time-list
						  :win plot-pane :overlay overlay
						  :connect-data-points *connect-data-points
						  :comment *simulation-plot-window-comment*
						  :comment-position *simulation-plot-window-comment-position*
						  :label-traces
						  (and (not (or waterfall no-labels))
						       *label-surf-plots*
						       (not (and (numberp *max-num-traces-for-plot-trace-labels*)
								 (> (length pane-label-list)
								    *max-num-traces-for-plot-trace-labels*))))
						  :session-name *session-name*
						  :waterfall waterfall
						  
						  :x-trace-offset (if waterfall waterfall-x-offset 0.0)
						  :y-trace-offset (if waterfall waterfall-y-offset 0.0)
						  
						  :use-same-line-style use-same-line-style
						  :label-waterfall label-waterfall
						  :auto-wf-setup (and waterfall *AUTO-PLOT-WATERFALL*)
						  :timed-data t
						  :y-min y-min :y-max y-max :y-label (string y-label)
						  :x-max x-max :x-min x-min :x-label x-label
						  :x-inc (SURF-PLOT-TIME-INCS (or x-max *user-stop-time*) )
						  :update update :resurrect resurrect))
			       (setq count 0
				     pane-data-list '()
				     pane-label-list '())
			       (when pane-counter (setq pane-counter (1+ pane-counter))))))))
	  (unless (or group-by-cell-type group-by-cell) (return)))))


(defun phase-plots (element-pairs &key title y-label x-label x-min y-min x-max y-max (prompt-for-overlay t))
  "The argument ELEMENT-PAIRS is one of the following:

   (element-1 element-2) -> The data types for each element are defaults from DEFAULT-DATA-TYPE

Or data types may be specified for any of the elements:

   ((element-1 data-type) element-2)
   (element-1 (element-2 data-type))
   ((element-1 data-type) (element-2 data-type))

Or, a list of element pairs (with optional data types) may be given, e.g.:

   (((element-1 data-type) element-2) (element-3 element-4) (element-4 (element-5 data-type)) ... )

"
  (let ((data-lists '())
	(label-list '())
	actual-x-label actual-y-label)
    (loop for element-pair in (if (consp (car element-pairs)) element-pairs (list element-pairs))
	  for pair-count from 1
	  do (let* ((first-element (if (consp (first element-pair)) (car (first element-pair)) (first element-pair)))
		    (second-element (if (consp (second element-pair)) (car (second element-pair)) (second element-pair)))
		    (data-type-1 (if (consp (first element-pair)) (cadr (first element-pair))
				     (DEFAULT-DATA-TYPE first-element)))
		    (data-type-2 (if (consp (second element-pair)) (cadr (second element-pair))
				     (DEFAULT-DATA-TYPE second-element)))
		  
		    (data-1 (element-data first-element data-type-1))
		    (data-2 (element-data second-element data-type-2))
		    (trace-label (format nil "~A ~A vs ~A ~A"
					 (element-name first-element)
					 data-type-1
					 (element-name second-element)
					 data-type-2)))
	       (when (and data-1 data-2)
		 (if (= pair-count 1)
		     (setq actual-x-label (or x-label (plot-y-label-from-data-type data-type-1))
			   actual-y-label (or y-label (plot-y-label-from-data-type data-type-2)))
		     (setq actual-x-label x-label
			   actual-y-label y-label))
		 (push (list data-1 data-2) data-lists)
		 (push trace-label label-list)))
	  finally 
	  (plot-xy-data
	   data-lists label-list
	   :prompt-for-overlay prompt-for-overlay
	   :x-min x-min :x-max x-max :x-label actual-x-label
	   :y-min y-min :y-max y-max :y-label actual-y-label 
	   :title (or title (car label-list))))))
