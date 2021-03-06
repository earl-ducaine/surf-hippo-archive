;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: menus.lisp

(in-package "SURF-HIPPO")

;;;************* MENUS AND SETUP FUNCTIONS **************

;;;; MAIN-MENU Sets up all the parameters for the current run. Returns with either a circuit loaded
;;;; and ready to go, or NIL to *quit*. Note that this function is called even with an automatic run
;;;; since it does some of the circuit setup.

(defun main-menu-text (&optional text)
  (concatenate-strings
   (when (and (stringp text) (> (length text) 0)) text)
   (when (and (stringp text) (> (length text) 0)) (format nil "~%"))
   (when *circuit-loaded* (format nil "Current circuit is ~a" *circuit*))
   (when (and *circuit-loaded* *simulation-finished*) (format nil "~%"))
   (when *simulation-finished* (format nil "Last run: ~a" *simulation-name*))
   (when *circuit-loaded* (format nil "~%"))
   "Hit OK only to quit"))




(defun main-menu ()
  (let (dummy1 dummy2 dummy3 dummy4 dummy5 dummy8 dummy9 dummy10 dummy7 dummy11 dummy12 dummy13 dummy14 help-string)
    (loop while t do
	  (setq dummy1 nil dummy2 nil dummy3 nil dummy4 nil dummy5 nil
		dummy7 nil dummy8 (and *circuit-loaded* (no-input-p)) dummy12 nil dummy14 nil)
	  (CHOOSE-VARIABLE-VALUES
	   `(,(when *circuit-loaded* `(dummy10 "Run simulation (immediately)" :boolean))
	     (dummy1 ,(format nil "Overall parameters, load circuit~%or files") :boolean)
	     ,(when *circuit-loaded* `(dummy2 "Histology" :boolean))
	     (dummy3 "Edit circuit elements" :boolean)
	     ,(when (are-there-sources) '(*modify-stimulus* "Edit clamp stimulus" :boolean))
	     (dummy7 "Information management" :boolean)
	     (dummy5 ,(concatenate-strings "Edit plot parameters"
		       (when *archive-variable-list* " - plot loaded archive data")
		       (when (or *HIDE-plot-WINDOWS* *overlay-all-plots*) (format nil "~%"))
		       (cond ((and *HIDE-plot-WINDOWS* *overlay-all-plots*) "(plots hidden, overlay set)")
			     (*HIDE-plot-WINDOWS* "(plots hidden)")
			     (*overlay-all-plots* "(plot overlay set)"))) :boolean)
	     ,(when (are-there-light-synapses) '(dummy14 "Edit light inputs" :boolean))
	     (dummy12 "Surf-Hippo help" :boolean))
	   :image (create-instance nil *main-menu-image*)
	   :text (main-menu-text help-string) :label "Surf-Hippo Main Menu")
	  (setq help-string nil)
	  unless (or *automatic-run* dummy10 dummy1 dummy2 dummy3 *modify-stimulus* dummy7 dummy5 dummy14 dummy12)
	  do (return nil)		
	  when dummy12
	  do (setq help-string (format nil "Look at ~A,~%or read the User Manual in the ~A directory.~%"
				       (concatenate-strings *surf-home* "doc/old-doc-files/running.doc")
				       (concatenate-strings *surf-home* "doc/")))
	  (format t "~A" help-string)
	  when dummy7 do (quick-info-menu)
	  when (and (not dummy12) (not *circuit-loaded*)) do (setq dummy10 nil)
	  ;; If we want to run the simulation (assuming the circuit is loaded) then don't run these menus.
	  unless dummy10 do
	  (setq dummy11 nil)
	  (cond-every (dummy1 (overall-parameter-menu))
		      (dummy3 (cell-menu))
		      (dummy5 (plot-parameters-menu))
		      (dummy14 (menu-for-light-stimulus))
		      (dummy2 (drawing-menu)))
	  ;; Another chance to edit sources, since sources can be added from histology windows.
	  (when (and nil
		     (not *modify-stimulus*) ; dummy8
		     (no-nils (mapcar 'source-stimulus-p (sources))))
	    (choose-variable-values '((*modify-stimulus* "Edit clamp stimulus" :boolean)) :label "More ??...."))
	  do
	  (when dummy10 (setq *modify-stimulus* nil))
	  ;; Set up the circuit sources.
	  (when *modify-stimulus*
	    (EDIT-SOURCE-STIMULI)
	    (setq *modify-stimulus* nil))
	  ;; Propagate parameters to the circuit elements. The optional T arg is to consider
	  ;; *RECHECK-CIRCUIT-ELEMENTS-PARAMETERS* .
	  (when (or dummy1 dummy3)
	    (set-circuit-elements-parameters t)
					; (choose-plot-data)
	    )
	  when (or *automatic-run* (and *circuit-loaded* dummy10)) do (return t))))


      
;;;; OVERALL-PARAMETER-MENU Set up the overall simulation parameters. Loads circuit.
(defun overall-parameter-menu ()
  (let (dummy1 dummy2 dummy3 dummy4 dummy6 dummy5 dummy6 dummy7 dummy8 dummy9 dummy10 dummy11  
	       (dummy12 *ignore-q10*))
    (setq dummy2 nil ; (not *circuit-loaded*)
	  dummy6 nil 
	  dummy5  *Temp-celcius* )	; Show the temperature in degress Celcius.
    (choose-variable-values
     `((dummy2 "Load circuit function/file" :boolean)
       (dummy8 "Load Lisp file (e.g. .elts file)" :boolean)
       (*user-stop-time* "Length of simulation [ms]" :float)
       (dummy1 "Time Step and Numerical Integration Parameters" :boolean)
       (dummy10 "Colorize menu" :boolean)
       
       (dummy5 "Temperature [deg C]" :number)
       (dummy12 "Ignore particle and gbar Q10s" :boolean)
       (dummy3 ,(format nil "Menu for voltage and~%concentration integrator initializations") :boolean)
       (dummy6 "Edit simulator global variables" :boolean)
       (dummy7 "Load archived data" :boolean)
       ,(when (vsources) '(dummy11 "Run linear corrected voltage clamp sequence" :boolean)))	
     :label "Overall Parameters, Load Circuit or Files")
    (setq *SESSION-NAME* (REPLACE-SPACE-WITH-UNDERSCORE *SESSION-NAME*))
    (setq *Temp-celcius* dummy5)
    (update-temperature (not (equal dummy12 *ignore-q10*)))
    (setq *ignore-q10* dummy12)
    (cond-every
     (dummy3 (initialization-menu))
     (dummy7 (data-file-browser))
     (dummy8 (file-browser-loader '("lisp" "sparcf" "elts") "Lisp File Browser"))
     (dummy1 (NUMERICAL-PARAMETERS-MENU))
     (dummy6 (globals-menu))
     (dummy2 (load-circuit-menu))
     (dummy10 (colorize-menu))     
     (dummy11 (linear-corrected-vclp :use-menu t)))))


(defun simulation-note-string-menu ()
  (setq *simulation-annotation*
	(SIMPLE-TEXT-MENU ""
			  "Simulation Annotation"
			  (if (> (length *simulation-annotation*) 0)
			      *simulation-annotation*
			      (format nil "Note:~%~%"))
			  200 500
			  (if (> (length *simulation-name*) 0)
			      (format nil "Edit note above for ~a" *simulation-name*)
			      "Edit *SIMULATION-ANNOTATION*"))))





(defun histology-wins-with-current-cells ()
  (loop for win in (WINDOWS-OF-MODE :histology)
	when (find-current-cells-in-graphics-win win)
	collect win))

(defun initialization-menu ()
  (let (dummy1 dummy2 dummy3 dummy4)
    (choose-variable-values
     `((dummy1 "Grab the current node voltages and store in *NODE-VOLTAGE-INITIALIZATIONS*" :boolean)
       (*use-node-voltage-initializations*
	"Use *NODE-VOLTAGE-INITIALIZATIONS* to set node voltages at start of simulation" :boolean)
       ,(when (conc-ints)
	  `(dummy2 "Grab the current concentration integrator values and store in *CONC-INT-INITIALIZATIONS*" :boolean))
       ,(when (conc-ints)
	  `(*use-conc-int-initializations*
	    "Use *CONC-INT-INITIALIZATIONS* to set concentration integrators at start of simulation" :boolean))

       (dummy3 "Set virtual holding potentials for nodes with v-dep particles" :boolean)
       (dummy4 "Clear any virtual holding potentials for nodes with v-dep particles" :boolean))
     :label "Setting Up Initializations")
    (when dummy4 (loop for node in (nodes)
		       when (node-has-v-dep-element node) do (element-parameter node 'holding-potential nil)))
    (when dummy3 (menu-for-virtual-holding-potentials))
    (when dummy1 (SET-*NODE-VOLTAGE-INITIALIZATIONS*))
    (when dummy2 (set-*conc-int-initializations*))))

(defun menu-for-virtual-holding-potentials ()
  (loop for node in 
	(choose-list-values-from-keys
	 (loop for node in (nodes) when (node-has-v-dep-element node) collect (list (node-name node) node))
	 nil :label "Select nodes to set virtual holding potential")
	do
	(let ((dummy1 (s-flt (or (element-holding-potential node) (element-resting-potential node)))))
	  (choose-variable-values
	   `((dummy1 ,(format nil "Virtual holding potential [mV] for ~A" (node-name node)) :float))
	   :label "Setting Virtual Holding Potentials")
	  (element-holding-potential node dummy1))))




(defvar *store-plot-results-to-folder* nil)



(defun info-menu ()
  (choose-variable-values
   '((*store-plot-results-to-folder* "Store plotted data to Data Folder" :boolean)
     (*print-numerical-details* "Include parameters of numerical method in print outs" :boolean)
     (*print-out-to-lisp* "Print simulation information to Lisp Window" :boolean)
     (*simulation-print-detail* "Print details for every simulation:"
      :choose (:none :terse :medium :full :FULL_With_SEGMENTS :specific_elements) :vertical :rank-margin 3)
     (*print-out-to-info-window* "Print simulation information to Information Window" :boolean)
     (*create-new-info-window* "Create new Information Window" :boolean)
     (*save-simulation-data-to-file* "Save simulation data to file" :boolean)
     (*save-simulation-info* "Save simulation information to file" :boolean)
     (*save-full-simulation-info* "Save all the simulation information to file" :boolean)
     (*make-circuit-subdir* "Make subdirectory for circuit's output files" :boolean)
     (*data-directory* "Edit data directory" :string))
   :label (if *last-simulation-file-path*
	      (format nil "Simulation Info - last file was for ~S" *last-simulation-file-path*)
	      "Simulation Info")))


(defun quick-info-menu ()
  (let* ((dummy1 :none)
	 dummy2 dummy3 dummy4
	 (dummy5 *simulation-print-detail*) 
	 (dummy6 *print-numerical-details*)
	 dummy7 dummy8 dummy9 dummy11
	 (dummy12 *include-simulation-annotation*) dummy13 (dummy14 *include-events-in-element-documentation-code*)
	 (menu-list
	  `((*simulation-description-destination* "Write text simulation description now to:"
		     :choose (:none :File :Lisp_Window :Info_Window :Log_file) :rank-margin 5)
	    (dummy5 "Text output detail:"
		    :choose (:none :terse :medium :full :FULL_With_SEGMENTS :specific_elements) :horizontal :rank-margin 3)
	    (dummy6 "Include numerical parameters in this output" :boolean)
	    (:comment "Writing Lisp Files of Current Data and Circuit")
	    (dummy3 "Write current plot data" :boolean)
	    (dummy1 "Dump loadable circuit element definitions:" :choose (:none :prompt_for_elements :all_in_circuit))
	    (dummy14 "Include events in element documentation code" :boolean)
	    (dummy8 "Dump cell geometry" :boolean)
	    (dummy11 "Dump current plot spec settings" :boolean)
	    (:comment "General Annotation Comment String")
	    (dummy12 "Edit annotation string for dumped files" :boolean)
	    (*include-simulation-annotation*
	     ,(concatenate-strings
	       (format nil "Include current simulation annotation string:")
	       (if (= 0 (length *simulation-annotation*)) " (Comment is empty)" (format nil ":~%~A" *simulation-annotation*)))
	     :boolean)
	    (:comment "Miscellaneous...")
	    (*dump-analysis-to-file* "Dump analysis results to session file" :boolean)
	    (*DOCUMENT-all-new-VARIABLES* "Document all new variables" :boolean)
	    (dummy13 "Dump documented variables to loadable .vars file now" :boolean)
	    (*create-new-info-window* "Create new Information Window" :boolean)
	    (dummy7 "Store data from last simulation into Data Folder" :boolean)
	    (dummy9 "Specify automatic information output" :boolean))))
    (choose-variable-values menu-list :label
			    (if *last-simulation-file-path*
			      (format nil "Simulation Information Output - Last File Was For ~S"
				      *last-simulation-file-path*)
			      "Simulation Information Output"))
    (when dummy12 (simulation-note-string-menu))
    (when dummy11 (dump-all-plot-lists))
    (when dummy9 (info-menu))
    (when dummy8 (dump-tree-menu))
    (when dummy7 (traces-to-folder))
    (let ((*print-numerical-details* dummy6))
      (when (and dummy3 *simulation-finished* (not (and *save-simulation-data-to-file* (eq :file *simulation-description-destination*))))
	(write-element-data (when (unless *automatic-run* (go-ahead-menu "Prompt for each data list to save")) :menu))
					; (dump-data-file (unless *automatic-run* (go-ahead-menu "Prompt for each data list to save")))
	)
      (case dummy5
	(:specific_elements (information-for-elements-menu nil *simulation-description-destination*))
	(t (case *simulation-description-destination*
	     (:file (dump-simulation-files nil t dummy5))
	     (:lisp_window (print-circuit dummy5))
	     (:info_window (print-circuit-to-info-window dummy5))
	     (:log_file (update-surf-log-file 'print-circuit (list dummy5)))))))
    (when dummy13 (dump-DOCUMENTED-USER-VARIABLES-file))
    (let ((*include-events-in-element-documentation-code* dummy14))
      (case dummy1
	(:all_in_circuit (dump-all-circuit-elements-file))
	(:prompt_for_elements (dump-elements-file t))))))


	 
(defun default-comment-font-menu ()
  (setq *plot-axis-font* (font-menu *plot-axis-font* "Choose default plot axis font"))
  (when (go-ahead-menu "Change current plot windows' font")
    (loop for win in *output-windows* do (s-value win :plot-axis-font *plot-axis-font*))))

;; Check to make sure there are either shell 2 or shell 3 integrators to plot.
(defun conc-int-types-shell-2-p ()
  (loop for type in (conc-int-types)
	when (and (element-in-circuit type)
		  (conc-int-type-shell-2-p type))
	do (return t)))

(defun conc-int-types-shell-3-p ()
  (loop for type in (conc-int-types)
	when (and (element-in-circuit type)
		  (conc-int-type-shell-3-p type))
	do (return t)))

(defun enable-plot-variable-types-menu ()
  ;; Let the user change the flags if desired.
  (choose-variable-values
   (loop for plot-list-info in *plot-lists-info*
	 when (and (>= (length (plot-list-info-tables plot-list-info)) 1)
		   (plot-list-info-enable-var plot-list-info)
		   (loop for table in (plot-list-info-tables plot-list-info) unless (HASH-TABLE-EMPTY table) do (return t))
		   (case (plot-list-info-structure-slot plot-list-info)
		     (total-concentration (or (conc-int-types-shell-2-p) (conc-int-types-shell-3-p)))
		     (concentration-2 (conc-int-types-shell-2-p))
		     (concentration-3 (conc-int-types-shell-3-p))
		     (t t)))
	 collecting
	 (list (plot-list-info-enable-var plot-list-info)
	       (concatenate-strings "Plot "
			    (when (eq '*PLOT-PATH-NODES* (plot-list-info-names plot-list-info)) "Path ")
			    (slot-descriptor-string (car (plot-list-info-tables plot-list-info))
						    (plot-list-info-structure-slot plot-list-info))
			    "s") `:boolean))
   :label "Enable variables to plot"))

;;; PLOT-PARAMETERS-MENU Set up some plotting parameters.
(defun plot-parameters-menu ()
  (when nil ;; (xor *overlay-simulations-last *overlay-simulations) ; for backward compatibility.
    (setq *OVERLAY-ALL-PLOTS* *overlay-simulations
	  *overlay-simulations-last *overlay-simulations))
  (let (dummy1 dummy2 dummy3 dummy4 dummy6 dummy7 dummy8 dummy9)
    (loop while t do
	  (setq dummy4 nil dummy2 nil dummy6 nil dummy7 nil dummy9 nil)
	  (let ((menu-list '((dummy7 "Enable/disable plotted variable types" :boolean)
			     (dummy9 "Clear some plot lists" :boolean)
			     (dummy1 "Edit plotted elements" :boolean)
			     (*CREATE-NEW-SIMULATION-PLOTS* "Create a new set of plot windows" :boolean)
			     (*OVERLAY-ALL-PLOTS* "Overlay data (unless creating new windows)" :boolean)
			     (*ACCOMODATE-ALL-OVERLAYS* "Adjust plots to accomodate all overlays" :boolean)
			     (*preserve-plot-layout* "Preserve layout of existing plots" :boolean)
			     (dummy2 "Replot all data now" :boolean)
			     (dummy4 "More plot details" :boolean))))
	    (when *archive-variable-list* (push '(dummy6 "Plot loaded archive data" :boolean) menu-list))
	    (when *data-folder* (push '(dummy8 "Plot data from Data Folder" :boolean) menu-list))
	    (choose-variable-values menu-list :label "Setting Up Plot Parameters"))
	  (cond-every
	   ((and *overlay-simulations *create-new-plot-windows*) (setq *overlay-simulations nil))
	   (dummy7 (enable-plot-variable-types-menu))
	   (dummy6 (PLOT-ARCHIVE-VARS))
	   (dummy8 (plot-folder-traces))
	   (dummy9 (menu-to-clear-plot-lists))
	   (dummy4 (plot-details-menu))
	   (dummy1 (choose-plot-elements-menu))
	   ((and *simulation-finished* dummy2) (surf-plotter)))
	  when (not (or dummy6 (and *simulation-finished* dummy2) dummy4)) do (return t)))
  (choose-plot-data)
  nil)

(defun plot-details-menu ()
  (let (dummy5
	(dummy10 (cond (*group-plot-data-by-cell* :by_cell)
		       (*GROUP-PLOT-DATA-BY-CELL-TYPE* :by_cell_type)
		       (t :no_grouping))))
    (choose-variable-values
     `((*save-simulation-data* "Enable saving of simulation data" :boolean)
       (*plot-standard-windows* "Enable plotting" :boolean)
       (*HIDE-plot-WINDOWS* "Hide plots, even if they're created." :boolean)
       (*hard-copy-screen* "Hardcopy screen after simulation" :boolean)
       ,(when (or *plot-synapse-events-p *plot-axon-events-p) '(*plot-events* "Plot rasters of all events" :boolean))
       ,(when (or *plot-synapse-events-p *plot-axon-events-p)
	  '(*plot-event-generators* "Plot rasters of event generators" :boolean))
       (*plot-channels-by-major-ion* "Plot currents by major ion" :boolean)
       (*SAVE-CONDUCTANCES-normalized* "Save and plot conductances as percents of total" :boolean)
       (*PLOT-TOTAL-CONCS-SEPARATELY* "Plot total integrator concentrations separately" :boolean)
;;       (*PLOT-shell-CONCS-SEPARATELY* "Plot integrator shell concentrations separately" :boolean)
       (*save-data-step* "Time steps/data point" :integer)
       (*MASSAGE-ELEMENT-PLOT-LABELS* "Massage simple name element plot labels" :boolean)
       (dummy10 "Data grouping:" :choose (:by_cell :by_cell_type :no_grouping))
       (*traces-per-plot* "Maximum number of traces/plot" :integer)
       (*label-surf-plots* "Show trace labels on plots" :boolean)
;       (dummy5 "Wipe out the list of plot windows" :boolean)
       (*plot-line-style* "Plot line style (new windows only):"
	:choose (:Thick-Colors :Thin-Colors :Thick-Dashed-lines :Dashed-lines))
       (*voltage-plot-waterfall* "Plot voltages in waterfall format" :boolean)
       (*voltage-plot-waterfall-x-offset* "X offset [ms] for voltage waterfalls" :float)
       (*voltage-plot-waterfall-y-offset* "Y offset [mV] for voltage waterfalls" :float)
       (*AUTO-PLOT-WATERFALL* ,(concatenate-strings
				(format nil "When waterfall for plots (e.g. voltage) enabled,~%")
				(format nil "X offset is 0 and Y offset is automatically~%")
				"adjusted (overrides above offsets).") :boolean))
     :label "Setting Up Some More Plot Parameters")
;    (when dummy5 (clear-plot-windows))
    (case dummy10
      ((:group_plots_by_cell :by_cell)
       (setq *group-plot-data-by-cell* t
	     *GROUP-PLOT-DATA-BY-CELL-TYPE* nil))
      ((:group_plots_by_cell_type :by_cell_type)
       (setq *group-plot-data-by-cell* nil
	     *GROUP-PLOT-DATA-BY-CELL-TYPE* t))
      (t (setq *group-plot-data-by-cell* nil
	       *GROUP-PLOT-DATA-BY-CELL-TYPE* nil)))
    (if *automatic-voltage-plot-scaling
	(setq *voltage-plot-min nil *voltage-plot-max nil)
	(progn
	  (cond-every
	   ((not *voltage-plot-min) (setq *voltage-plot-min -90.0))
	   ((not *voltage-plot-max) (setq *voltage-plot-max -40.0)))
	  (dendrite-plot-parameters-menu)))))
  
;;; DENDRITE-PLOT-PARAMETERS-MENU Set up some plotting parameters.
(defun dendrite-plot-parameters-menu ()
  (choose-variable-values
    '((*voltage-plot-min "Voltage plot minimum [mV]" :number)
      (*voltage-plot-max "Voltage plot maximum [mV]" :number))
    :label "Voltage Plot Scaling"))

;;; SOMA-PLOT-PARAMETERS-MENU Set up some plotting parameters.
(defun soma-plot-parameters-menu ()
  (choose-variable-values
    '((*soma-voltage-plot-min "*soma-voltage-plot-min [mV]" :number)
      (*soma-voltage-plot-max "*soma-voltage-plot-max [mV]" :number))
    :label "Soma Voltage Plot Scaling"))


(defvar *last-edited-synapse-type* nil)
(defvar *last-edited-channel-type* nil)

;;; CELL-MENU Sets up all the parameters for the current run.
(defun cell-menu ()
  (unless (element ; -in-circuit
	   *last-edited-channel-type*) (setq *last-edited-channel-type* nil))
  (unless (element ; -in-circuit
	   *last-edited-synapse-type*) (setq *last-edited-synapse-type* nil))
  (let (dummy1 dummy2 dummy3 dummy4 dummy5
	       ; (dummy6 (not *active*))
	       (dummy7 (unless *active* :global_block))
	       (dummy8 t) dummy9 dummy10 dummy11 dummy12 dummy13 dummy14 dummy15 dummy16
	       (dummy17 (unless *ENABLE-SYNAPSES* :global_block))
	       dummy18 dummy19
	       dummy20 dummy21
	       dummy22
	       last-edited-channel-sym
	       last-edited-synapse-sym
	       (menu-list '((dummy15 "Edit names of circuit objects" :boolean))))	
    (loop while dummy8 do
	  (setf
	   last-edited-channel-sym (when *last-edited-channel-type* (intern (element-name *last-edited-channel-type*)))
	   last-edited-synapse-sym (when *last-edited-synapse-type* (intern (element-name *last-edited-synapse-type*)))
	   dummy1 nil dummy2 nil dummy3 nil dummy5 nil  dummy8 nil dummy9 nil
	   ; dummy6 (not *active*)
		dummy7 (unless *active* :global_block)
		dummy17 (unless *ENABLE-SYNAPSES* :global_block)
		dummy18 
		dummy19
		dummy20 nil dummy21 nil
		menu-list '((dummy15 "Edit names of circuit objects" :boolean))
		dummy10 nil dummy11 nil dummy12 nil dummy13 nil dummy14 nil dummy15 nil
		dummy16 nil)
	  (push '(dummy14 "Move cell(s) around" :boolean) menu-list)
	  (push '(dummy9 "Edit global ionic concentrations" :boolean) menu-list)
	  (push '(dummy10 "Examine/modify distribution of electrotonic lengths" :boolean) menu-list)
	  (push '(dummy22 "Revamp channel types w/ current library parameters" :boolean) menu-list)

	  (push `(dummy16 "Edit the following elements or types:"
		  :x-choose
		  ,(no-nils (list
			     (when *cell-type* :cell_types)
			     (when *soma* :somas)
			     (when (or *vsource* *isource*) :sources)
			     :synapses
			     (when *synapse* :synapses_in_circuit)
			     (when *last-edited-synapse-type* last-edited-synapse-sym)
			     :channels
			     (when *channel* :channels_in_circuit)
			     (when *last-edited-channel-type* last-edited-channel-sym)
			     (when *conc-int* :conc_ints)
			     (when *pump* :pumps)
			     (when *electrode* :electrodes)
			     (when *axon* :axon_types)))
		  :rank-margin 3)
		menu-list)
	  (push '(dummy8 "Plot somatic steady-state IV characteristics" :boolean) menu-list)
	  (when *channel*
	    (push '(dummy20 "Edit blocking of channels by major ion type" :boolean) menu-list)
	    (push '(dummy7 "Edit blocking of channels by type:"
			   :choose (:global_block :reset_edited_blocks :edit_block))
		  menu-list)
	    (when (blocked-channel-types)
	      (push `(:comment ,(format nil "Blocked types: ~A"
				 (concatenate-string-list (coerce-to-list (element-name (blocked-channel-types)) )
				  :string-spacer " " :string-count-to-add-linefeed 4)))				 
		    menu-list)))
	  (when (non-unity-channel-type-gbar-modulation-p) 
	    (push `(dummy18 ,(format nil "Manipulate channel type GBAR modulation~A:"
			      (when (channel-TYPES-WITH-NON-UNITY-GBAR-MODULATION)
				(format nil "~%[Currently modulated types ~A]" 
					(loop for type in (channel-TYPES-WITH-NON-UNITY-GBAR-MODULATION)
					      collect (element-name type)))))
		    :choose (:apply-modulation-to-gbar :reset-modulation :nothing)
		    :vertical) menu-list))
	  (when (non-unity-synapse-type-gbar-modulation-p) 
	    (push `(dummy19 ,(format nil "Manipulate synapse type GBAR modulation~%~A:"
			      (loop for type in (synapse-TYPES-WITH-NON-UNITY-GBAR-MODULATION)
			       collect (element-name type)))
		    :choose (:apply-modulation-to-gbar :reset-modulation :nothing)) menu-list))
	  
	  (when *synapse*
	    (push '(dummy21 "Edit blocking of synapses by major ion type" :boolean) menu-list)
	    (push '(dummy17 "Block all or edit blocking of synapses by type:"
		    :choose (:global_block :reset_edited_blocks :edit_block))
		  menu-list)
	    (when (blocked-synapse-types)
	      (push `(:comment ,(format nil "Blocked types: ~A"
				 (concatenate-string-list (coerce-to-list (element-name (blocked-synapse-types)))
				  :string-spacer " " :string-count-to-add-linefeed 4)))
		    menu-list)))
	  ; (when *channel* (push '(dummy6 "Global channel block" :boolean) menu-list))
	  (choose-variable-values menu-list :label "More Questions....")
	  (when dummy22 (revamp-channel-type-parameters))
	  ; (setq *active* (not dummy6))
	  (case dummy7
	    (:global_block (setq *active* nil))
	    (:reset_edited_blocks (unblock-all-channel-types)
				  (setq *active* t)))
	  (when dummy20 (menu-for-channel-blocking-by-ion-type))
	  (case dummy7 (:edit_block (menu-for-channel-blocking)))


	  (case dummy17
	    (:global_block (setq *ENABLE-SYNAPSES* nil))
	    (:reset_edited_blocks (unblock-all-synapse-types)
				  (setq *ENABLE-SYNAPSES* t)))
	  (when dummy21 (menu-for-synapse-blocking-by-ion-type))
	  (case dummy17 (:edit_block (menu-for-synapse-blocking)
				     (setq *ENABLE-SYNAPSES* t)))




	  (case dummy18
	    (:apply-modulation-to-gbar (transfer-type-gbar-modulation-to-gbars 'channel-type))
	    (:reset-modulation (RESET-NON-UNITY-CHANNEL-TYPE-GBAR-MODULATION)))
	  (case dummy19
	    (:apply-modulation-to-gbar (transfer-type-gbar-modulation-to-gbars 'synapse-type))
	    (:reset-modulation (RESET-NON-UNITY-synapse-TYPE-GBAR-MODULATION)))

	  (cond-every
	   (dummy15 (EDIT-all-THINGs-NAME-MENU))
	   (dummy14 (menu-to-move-cells nil *standard-graphics-output*))
	   (dummy10 (consolidate-cells-tree))
	   
	   ((member :somas dummy16) (menu-for-somaS))
	   ((member :cell_types dummy16) (menu-for-cell-TYPES))
	   ((member last-edited-channel-sym ; :last-edited-channel
		    dummy16)
	    (menu-for-channel-types *last-edited-channel-type*))
	   ((member :channels_in_circuit dummy16)
	    (let ((types (get-current-channel-types-menu "Edit Channel Types In Circuit")))
	      (when types (menu-for-channel-types types))))
	   ((member :pumps dummy16) (menu-for-pumps))
	   ((member :channels dummy16) (menu-for-channel-parameters))
	   ((member :conc_ints dummy16) (menu-for-conc-ints))
	   ((member :axon_types dummy16) (MENU-FOR-AXON-TYPES))
	   ((member last-edited-synapse-sym ; :last-edited-synapse
		    dummy16) (menu-for-synapse-types *last-edited-synapse-type*))
	   ((member :synapses_in_circuit dummy16)
	    (let ((types (get-current-synapse-types-menu "Edit Synapse Types In Circuit")))
	      (when types (menu-for-synapse-types types))))
	   ((member :synapses dummy16) (menu-for-synapse-parameters))
	   ((member :electrodes dummy16) (edit-electrodes))
	   ((member :sources dummy16)  (sources-menu))
	   (dummy8 (MENU-FOR-UPDATE-AND-PLOT-IV))
	   (dummy9 (MENU-FOR-CONCENTRATIONS))))))
	   


(defun MENU-FOR-CHANNEL-BLOCKING-BY-ION-TYPE ()
  (let ((dummy1)) 
    (choose-variable-values
     '((dummy1 "Channel type ionic types:" :x-choose ("NA" "CA" "K" "CL")))
     :text "Blocking Channel Types"
     :label "Select ions to be blocked")
    (loop for ion in dummy1 do
	  (loop for type in (channel-types)
		when (and (instance-in-cell type)
			  (member (read-from-string ion) (channel-type-ion-permeabilities type) :key 'car))
		do (setf (channel-type-block type) t)))))

(defun MENU-FOR-SYNAPSE-BLOCKING-BY-ION-TYPE ()
  (let ((dummy1)) 
    (choose-variable-values
     '((dummy1 "Synapse type ionic types:" :x-choose ("NA" "CA" "K" "CL")))
     :text "Blocking Synapse Types"
     :label "Select ions to be blocked")
    (loop for ion in dummy1 do
	  (loop for type in (synapse-types)
		when (and (instance-in-cell type)
			  (member (read-from-string ion) (synapse-type-ion-permeabilities type) :key 'car))
		do (setf (synapse-type-block type) t)))))


(defun MENU-FOR-CHANNEL-BLOCKING ()
  (let ((blocked-types (select-hash-values-menu
			(CHANNEL-TYPE-HASH-TABLE) "Select Channel Types To Block"
			:selection-key 'channel-type-block
			:rank-margin 2 :do-all-at-once t :inclusion-key 'instance-in-cell)))
    (loop for type being the hash-value of (CHANNEL-TYPE-HASH-TABLE) 
	  do (setf (channel-type-block type) (true-p (or (member (channel-type-name type) blocked-types)))))))


(defun MENU-FOR-SYNAPSE-BLOCKING ()
  (let ((blocked-types (select-hash-values-menu
			(SYNAPSE-TYPE-HASH-TABLE) "Select Synapse Types To Block"
			:selection-key 'synapse-type-block
			:rank-margin 2 :do-all-at-once t :inclusion-key 'instance-in-cell)))
    (loop for type being the hash-value of (SYNAPSE-TYPE-HASH-TABLE) 
	  do (setf (synapse-type-block type) (true-p (or (member (synapse-type-name type) blocked-types)))))))


(defun menu-for-concentrations ()
  (let ((dummy1 (if *fix-e-na* :fixed_value :nernst))
	(dummy2 (if *fix-e-k* :fixed_value :nernst))
	(dummy3 (if *fix-e-ca* :fixed_value :nernst))
	(dummy4 (if *fix-e-cl* :fixed_value :nernst))

	)
    (choose-variable-values
     `((dummy1 "Global E-Na dependence:" :choose (:fixed_value :nernst) :label-left)
       (*e-na* "Fixed Na+ Reversal Potential [mV]" :float)
       (*na-conc-extra* "[Na+]out [mM]" :float)
       (*na-conc-intra* "[Na+]in [mM]" :float)
       (dummy2 "Global E-K dependence:" :choose (:fixed_value :nernst) :label-left)
       (*e-k* "Fixed K+ Reversal Potential [mV]" :float)
       (*k-conc-extra* "[K+]out [mM]" :float)
       (*k-conc-intra* "[K+]in [mM]" :float)
       (dummy3 "Global E-Ca dependence:" :choose (:fixed_value :nernst) :label-left)
       (*e-ca* "Fixed Ca++ Reversal Potential [mV]" :float)
       (*ca-conc-extra* "[Ca++]out [mM]" :float)
       (*ca-conc-intra* "[Ca++]in [mM]" :float)
       (dummy4 "Global E-Cl dependence:" :choose (:fixed_value :nernst) :label-left)
       (*e-cl* "Fixed Cl- Reversal Potential [mV]" :float)
       (*cl-conc-extra* "[Cl-]out [mM]" :float)
       (*cl-conc-intra* "[Cl-]in [mM]" :float))
     :text (concatenate-strings
	    (format nil "Global values for concentrations and reversal potentials:~%")
	    (format nil "note that specific cells, channels, synapses, concentration~%")
	    (format nil "integrators or their types may use their own reference."))
     :label (format nil "Global Ionic Concentrations"))
    (setq *fix-e-na* (eq dummy1 :fixed_value)
	  *fix-e-k* (eq dummy2 :fixed_value)
	  *fix-e-ca* (eq dummy3 :fixed_value)
	  *fix-e-cl* (eq dummy4 :fixed_value))
	  
    (setq *recheck-circuit-elements-parameters* t)
    (update-temperature-dependent-parameters nil t)))








(defun plot-voltage-sequence (sequence v-increment start-v label title &key prompt-for-overlay)
  (if (arrayp sequence) (setq sequence (array-to-list sequence)))
  (plot-xy-data (list (list (loop for volt from start-v by v-increment for i from 0 to (length sequence) collect volt)
			    sequence)) 
		'("")
		:title title
		:x-min start-v :x-origin-tick t :y-origin-tick nil
		:y-label-vertical-position :upper-right
		:y-label label :x-label "mV"
		:prompt-for-overlay prompt-for-overlay
		:width 450 :height 350))

(defun names-from-*parameters* (*parameters*)
  (delete-duplicates (loop for type-list in *parameters* collect (string (car type-list))) :from-end t))

(defun find-ion-perms-reference (type-list types-parameters)
  (or (cdr-assoc 'ion-permeabilities (cdr type-list))
      (when (assoc 'parent-type (cdr type-list))
	(let ((parent-type (cdr-assoc 'parent-type (cdr type-list))))
	  (loop for new-type-list in types-parameters
		when (eq parent-type (car new-type-list))
		return (cdr-assoc 'ion-permeabilities (cdr new-type-list)))))))


(defun menu-for-type (type-sym &key already-chosen-types label text exclude-types)
  (let* ((model (type-symbol-model type-sym))
	 (types-parameters (MODEL-PARAMETER-type-LIBRARY model))
	 (names-of-exclude-types (loop for type in (coerce-to-list exclude-types)
				       collect (format nil "~A" (or (element-name type) type))))
	 (type-name (case type-sym
		      (buffer-type "Buffer")
		      (pump-type "Pump")
		      (axon-type "Axon")
		      (channel-type "Channel")
		      (synapse-type "Synapse"))))
    (loop for name in
	  (let ((dummy1 "ALL") (dummy2 :all)(dummy3 "ALL"))
	    (choose-variable-values
	     `((dummy1 ,(format nil "~a ionic types:" type-name) :choose ("NA" "CA" "K" "CL" "ALL"))
	       ,(when (eq type-sym 'synapse-type)
		  `(dummy3 "Synapse control types:" :choose ("LIGHT-AUTO" "LIGHT" "AUTO" "VOLTAGE" "CHANNEL" "ALL")))
	       (dummy2 "All types or only those currently in circuit:" :choose (:all :in_cells)))
	     :label (format nil "Choosing ~A Types" type-name))
	    (choose-list-values
	     (loop for type-name in
		   (delete-duplicates
		    (let ((symbols-of-loaded-types
			   (loop for type in (list-of-all-things type-sym) collect (element-name type))))
		      (nconc
		       (loop for type in (list-of-all-things type-sym)
			     when (and (or (eq dummy2 :all)
					   (instance-in-cell (element-name type)))
				       (or (equal dummy1 "ALL")
					   (equal dummy1 (string (element-slot 'species type)))
					   (loop for ion-perm in (element-slot 'ion-permeabilities type)
						 when (equal dummy1 (string (car ion-perm))) do (return t)))
				       (or (equal dummy3 "ALL")
					   (equal dummy3 (string (element-slot 'control type)))))
			     collect (element-name type))
		       (loop for type-list in (delete-duplicates types-parameters :key 'car :from-end t)
			     when (and
				   (not (member (string (car type-list)) symbols-of-loaded-types))
				   (or (eq dummy2 :all)
				       (instance-in-cell (string (car type-list))))
				   (or (equal dummy1 "ALL")
				       (loop for ion-perm in (find-ion-perms-reference type-list types-parameters)
					     when (equal dummy1 (string (car ion-perm)))
					     do (return t)))
				   (or (equal dummy3 "ALL")
				       (equal dummy3
					      (string (get-synapse-type-control-from-synapse-type-parameter-library
						       type-list)))))
			     collect (string (car type-list)))))
		    :from-end t :test 'equal)
		   unless (member type-name names-of-exclude-types :test 'equal)
		   collect type-name)
	     (loop for type in already-chosen-types collect (element-name type))
	     :do-all-at-once t :rank-margin 3 :direction :horizontal :PUNT-IF-ONLY-ONE-ENTRY nil
	     :text text :label (or label (format nil "Choose ~A Types" type-name))))
	  collect 
	  (case type-sym
	    (pump-type (create-pump-type name))
	    (buffer-type (create-buffer-type name))
	    (axon-type (create-axon-type name))
	    (channel-type (create-channel-type name))
	    (synapse-type (create-synapse-type name))))))

(defun edit-ion-permeabilities (elt)
  (let* (ion-permeabilities-ok
	 bad-values
	 (element (element-type elt)) 
	 (first-time t)
	 (ion-permeabilities (typecase element
				       (synapse-type (synapse-type-ion-permeabilities element))
				       (channel-type (channel-type-ion-permeabilities element))))
	 (dummy1 (loop for ion-perm in ion-permeabilities
		       when (eq (car ion-perm) 'na) do (return (cadr ion-perm)) finally (return 0.0)))
	 (dummy2 (loop for ion-perm in ion-permeabilities
		       when (eq (car ion-perm) 'k) do (return (cadr ion-perm)) finally (return 0.0)))
	 (dummy3 (loop for ion-perm in ion-permeabilities
		       when (eq (car ion-perm) 'ca) do (return (cadr ion-perm)) finally (return 0.0)))
	 (dummy4 (loop for ion-perm in ion-permeabilities
		       when (eq (car ion-perm) 'cl) do (return (cadr ion-perm)) finally (return 0.0)))
	 dummy5)
    (loop until ion-permeabilities-ok do
	  (choose-variable-values
	   '((dummy1 "Na+ Permeability" :float)
	     (dummy2 "K+ Permeability" :float)
	     (dummy3 "Ca++ Permeability" :float)
	     (dummy4 "Cl- Permeability" :float)
	     (dummy5 "Keep original values" :boolean))
	   :text (if first-time
		     "Permeabilities must add to <= 1.0, and each must be between 0.0 and 1.0."
		   (if bad-values
		       (concatenate-strings
			(format nil "Permeabilities must add up <= 1.0, and each must be between 0.0 and 1.0.~%")
			(format nil "Permeability for ~A is out of range" (cond ((not (<= 0.0 dummy1 1.0)) "Na+")
										((not (<= 0.0 dummy2 1.0)) "K+")
										((not (<= 0.0 dummy3 1.0)) "Ca++")
										((not (<= 0.0 dummy4 1.0)) "Cl-"))))
		     (format nil "Permeabilities must add up to <= 1.0 - now they sum to ~A."
			     (+ dummy1 dummy2 dummy3 dummy4))))

	   :label (format nil "Edit Ion Permeabilities For ~A ~A" (type-of element) (element-name element)))
	  (setq ion-permeabilities-ok (or dummy5 (and (<= 0.0 dummy1 1.0)
						      (<= 0.0 dummy2 1.0)
						      (<= 0.0 dummy3 1.0)
						      (<= 0.0 dummy4 1.0)
						      (<= (+ dummy1 dummy2 dummy3 dummy4) 1.0)))
		bad-values (not (and (<= 0.0 dummy1 1.0)
				     (<= 0.0 dummy2 1.0)
				     (<= 0.0 dummy3 1.0)
				     (<= 0.0 dummy4 1.0)))
		first-time nil))
    (setq ion-permeabilities (clean-up-list (list (when (> dummy1 0) (list 'na dummy1))
						  (when (> dummy2 0) (list 'k dummy2))
						  (when (> dummy3 0) (list 'ca dummy3))
						  (when (> dummy4 0) (list 'cl dummy4)))))
    (unless dummy5
      (typecase element
		(synapse-type (setf (synapse-type-ion-permeabilities element) ion-permeabilities))
		(channel-type (setf (channel-type-ion-permeabilities element) ion-permeabilities))))))

(defun update-and-plot-iv (channel-type)
  (let (*recheck-circuit-elements-parameters*)
    (set-circuit-elements-parameters)
    (plot-ivs (loop for cell being the hash-value of (CELL-HASH-TABLE)
		    when (find-channel-type-in-cell cell channel-type)
		    collect (cell-name cell)))))

(defun menu-for-update-and-plot-iv ()
  (let (*recheck-circuit-elements-parameters*)
    (let ((cell-names (select-hash-values-menu (CELL-HASH-TABLE) "Select Cells to Plot Somatic Steady-State IV Curves"
					       :punt-if-only-one-entry t)))
      (when cell-names
	(set-circuit-elements-parameters)
	(plot-ivs cell-names)))))


;;  MENU-FOR-UPDATE-AND-PLOT-IV Now PLOT-IVS has its own menu.
(defun menu-for-update-and-plot-iv ()
  (let (*recheck-circuit-elements-parameters*)
    (set-circuit-elements-parameters)
    (plot-ivs)))

(defun find-channel-type-in-cell (cell channel-type)
  (loop for elt in (node-elements (soma-node (cell-soma cell)))
	when (and (channel-p elt) (equal (channel-type elt) channel-type))
	do (return t)
	finally (return nil)))

(defun menu-for-channel-type-particle-types (channel-type)
  (let ((channel-type (element channel-type 'channel-type)))
    (when channel-type
      (setf (channel-type-particle-types-and-powers channel-type)
	    (loop for prt-type-and-power in (channel-type-particle-types-and-powers channel-type)
		  collecting
		  (multiple-value-bind (prt-type order)
		      (edit-particle-type (car prt-type-and-power) channel-type (cdr prt-type-and-power))
		    (cons prt-type order)))))))

(defun menu-for-channel-type-conc-particle-types (channel-type)
  (let ((channel-type (element channel-type 'channel-type)))
    (when channel-type
      (setf (channel-type-conc-particle-types-and-powers channel-type)
	    (loop for prt-type-and-power in (channel-type-conc-particle-types-and-powers channel-type)
		  collecting
		  (multiple-value-bind (prt-type order)
		      (edit-conc-particle-type (car prt-type-and-power) channel-type (cdr prt-type-and-power))
		    (cons prt-type order)))))))


;;; PULSE-NUMBER-FROM-MENU Returns integer for number of pulses.
(defun pulse-number-from-menu (pulse-lists source-name)
  (get-integer (length pulse-lists) 10 0 "Number of pulses:" (format nil "# Pulses For ~a" source-name)))

#|
(defun munge-elt-on-*SYNAPSE-NAMES-TO-DO-FIRST* (target-elt dummy6)
  (setf
   *SYNAPSE-NAMES-TO-DO-FIRST*
   (unless (eq dummy6 :PUsh_onto_cleared_list)
     (string-remove (node-name (element-physical-node target-elt)) *SYNAPSE-NAMES-TO-DO-FIRST*)))
  (unless (eq dummy6 :remove) (push (node-name (element-physical-node target-elt)) *SYNAPSE-NAMES-TO-DO-FIRST*)))
|#


(defun plot-segments-to-soma-menu (target-elt)
  (plot-segments-to-soma target-elt
			 (get-integer 0 100 0
				      (format nil "From ~A (path total number: ~A)"
					      target-elt
					      (length (segments-to-soma target-elt)))
				      "Segment to Soma Path Skip")
			 (when *plot-path-nodes* (go-ahead-menu "Clear previous plotted path nodes"))))

(defun constant-current-menu (cell-elements)
  (loop for constant-current-element in 
	(choose-list-values-from-keys
	 (loop for cell-element in cell-elements
	       when cell-element
	       collect (list (element-name cell-element) cell-element))
	 nil :label "Select Cell Elements" :text "Edit constant current injection")
	do (constant-current-element-menu constant-current-element)))

  
(defun constant-current-element-menu (target-elt)
  (let ((dummy1 (ELEMENT-CONSTANT-CURRENT target-elt))
	(dummy2 (or (ELEMENT-CONSTANT-CURRENT target-elt) 0.0)))
    (choose-variable-values
     '((dummy1 "Add/remove constant current source" :boolean)
       (dummy2 "Constant current value [nA]" :float))
     :label (format nil "Edit Constant Current @ ~A" (element-name target-elt)))
    (if dummy1 (add-constant-current-to-element target-elt dummy2) (CLEAR-ELEMENT-CONSTANT-CURRENT target-elt))))


;; This is for somas or segments. 
(defun OVERALL-ELEMENT-MENU (target-elt &optional win)
  (when (cell-element-p target-elt)
    (let* (dummy1
	   dummy2 dummy3 dummy4 (dummy5 :none) dummy6 dummy7 dummy8 dummy9 dummy10 dummy11 dummy12
	   dummy13	dummy14	dummy15 dummy23
	   (target-elt (element target-elt))
	   (target-element-string
	    (string-downcase (string (type-of target-elt))))
	   (menu-list `((dummy1 "Edit plotting information" :boolean)
			,(when (segment-p target-elt) '(dummy12 "Plot segments on path to soma" :boolean))
			(dummy2 "Add/Remove:" :x-choose (:sources :channels :synapses :axons :electrodes)
			 :rank-margin 3)
			(dummy23 "Mark specific branches, disable/enable branch marking"  :boolean)
			(dummy10 ,(format nil "Move cell ~A" (element-name (element-cell target-elt))) :boolean)
			(dummy8 ,(format nil "Unchoose this ~a" (string-downcase (string (type-of target-elt)))) :boolean)
			(dummy14 "Edit constant current at this node" :boolean)
			(dummy5 ,(format nil "Print parameters of ~a and its elements:" target-element-string)
			 :choose (:none :File :Lisp_Window :Info_Window :Log_file)
			 :rank-margin 3)
			(dummy13 "Edit specific elements associated with this node" :boolean))))
      (when nil ; (and (soma-or-segment target-elt) (ARE-THERE-LIGHT-SYNAPSES target-elt))
	(setq dummy6 (when (member (element-name target-elt) *SYNAPSE-NAMES-TO-DO-FIRST*) :remove :push))
	(when nil
	  (setq menu-list
		(nconc
		 menu-list
		 `((dummy6 "Push this node onto the front of the *SYNAPSE-NAMES-TO-DO-FIRST* list:"
		    :choose (:PUsh_onto_cleared_list :PUsh :remove) :vertical))))))
      (choose-variable-values
       menu-list
       :label (format nil "Modifying ~a ~A" (string-downcase (string (type-of target-elt))) (element-name target-elt)))
      (when dummy12 (plot-segments-to-soma-menu target-elt))
      (when dummy10 (MENU-TO-MOVE-CELLS (element-cell target-elt) win))
      (when dummy14 (constant-current-element-menu target-elt))
      ;; (when dummy6 (munge-elt-on-*SYNAPSE-NAMES-TO-DO-FIRST* target-elt dummy6))
      (loop for thing in dummy2 do (case thing
				     (:channels (menu-for-adding-channel-types target-elt))
				     (:synapses (MENU-FOR-ADDING-SYNAPSE-TYPES target-elt))
				     (:electrodes (menu-for-adding-electrodes target-elt))
				     (:sources (sources-for-target-element-menu target-elt))
				     (:axons (axons-for-target-element-menu target-elt))	))

      (cond-every
       ((not (eq :none dummy5)) (information-for-elements-menu target-elt dummy5))
       ((or dummy4 dummy3 dummy7 dummy2 dummy9) (set-circuit-elements-parameters))
       (dummy1 (plot-elements-of-target-element-menu target-elt))
       (dummy13 (edit-elements-of-target-element target-elt))
       (win (cond-every
	     (dummy1 (mark-plotted-nodes win t))
	     ((or dummy4 dummy7 dummy3)
	      (mark-elements :win win :draw-channels (g-value win :draw-marked-channels)
			     :draw-synapses (g-value win :draw-marked-synapses)))
	     (dummy2 (cond-every ((member :electrodes dummy2)
				  (draw-electrodes win (g-value win :draw-electrodes)))
				 ((or (member :electrodes dummy2)
				      (member :sources dummy2))
				  (draw-sources win t))))
	     (dummy8 (unchoose-chosen-ones win))
	     (dummy23 (mark-segment-chains win t t))))))))

;; not used
#|
(defun synapses-for-target-element-menu (target-elt)
  (let* ((no-light-synapses-yet (not (are-there-light-synapses)))
	 (original-synapses (loop for syn in (get-node-elements-of-type target-elt 'synapse)
				  when (eq (synapse-type-control (synapse-type syn)) 'light)
				  collect syn))
	 (new-synapse-type-names
	  (choose-list-values
	   (loop for type-list in *parameters-of-light-synapse-types collect (string (car type-list)))
	   (loop for syn in original-synapses collect (synapse-type-name (synapse-type syn)))
	   	   :do-all-at-once t
	   :rank-margin 5 :direction :vertical
	   :label (format nil "Choose Light Synapse Types to Remove/Add to ~a" (element-name target-elt)))))
    (loop for syn in original-synapses
	  when (not (string-member (synapse-type-name (synapse-type syn)) new-synapse-type-names))
	  do (erase-element syn)
	  else do (setq new-synapse-type-names
			(string-remove (synapse-type-name (synapse-type syn)) new-synapse-type-names)))
    (create-synapses target-elt new-synapse-type-names)
    (when (and no-light-synapses-yet (are-there-light-synapses))
      (choose-variable-values
       '((*light-stimulus-plane* "Light stimulus XY plane maps to cell plane:" :choose (:xy :xz)))
       :label "Specify light stimulus orientation for these new light synapses"))))
|#

(defun channels-for-target-element-menu (target-elt)
  (let* ((original-channels (get-node-elements-of-type target-elt 'channel))
	 (new-type-names
	  (choose-list-values
	   (loop for type-list in (model-parameter-type-library (type-symbol-model 'channel-type))
		 collect (string (car type-list)))
	   (loop for ch in original-channels collect (channel-type-name (channel-type ch)))
	   :do-all-at-once t :rank-margin 5 :direction :vertical
	   :label (format nil "Choose Channel Types to Remove/Add to ~a" (element-name target-elt)))))
    (loop for ch in original-channels
	  when (not (string-member (channel-type-name (channel-type ch)) new-type-names))
	  do (erase-element ch)
	  else do (setq new-type-names (string-remove (channel-type-name (channel-type ch)) new-type-names)))
    (create-channels target-elt new-type-names)))

(defun axons-for-target-element-menu (target-elt)
  (let* ((original-axons (get-node-elements-of-type target-elt 'axon))
	 (new-type-names
	  (choose-list-values
	   (loop for type-list in (model-parameter-type-library (type-symbol-model 'axon-type))
		 collect (string (car type-list)))
	   (loop for ch in original-axons collect (axon-type-name (axon-type ch)))
	   :do-all-at-once t :rank-margin 5 :direction :vertical
	   :label (format nil "Choose Axon Types to Remove/Add to ~a" (element-name target-elt)))))
    (loop for ch in original-axons
	  when (not (string-member (axon-type-name (axon-type ch)) new-type-names))
	  do (erase-element ch)
	  else do (setq new-type-names (string-remove (axon-type-name (axon-type ch)) new-type-names)))
    (create-axons target-elt new-type-names)))

#|
(defun light-synapses-for-target-element-menu (target-elt)
  (let* ((no-light-synapses-yet (not (are-there-light-synapses)))
	 (original-synapses (loop for syn in (get-node-elements-of-type target-elt 'synapse)
				  when (eq (synapse-type-control (synapse-type syn)) 'light)
				  collect syn))
	 (new-type-names
	  (choose-list-values
	   (loop for type-list in *parameters-of-light-synapse-types collect (string (car type-list)))
	   (loop for syn in original-synapses collect (synapse-type-name (synapse-type syn)))
	   :do-all-at-once t :rank-margin 5 :direction :vertical
	   :label (format nil "Choose Light Synapse Types to Remove/Add to ~a" (element-name target-elt)))))
    (loop for syn in original-synapses
	  when (not (string-member (synapse-type-name (synapse-type syn)) new-type-names))
	  do (erase-element syn)
	  else do (setq new-type-names (string-remove (synapse-type-name (synapse-type syn)) new-type-names)))
    (create-synapses target-elt new-type-names)
    (when (and no-light-synapses-yet (are-there-light-synapses))
      (menu-for-light-stimulus)
;      (choose-variable-values
;       '((*light-stimulus-plane* "Light stimulus XY plane maps to cell plane:" :choose (:xy :xz)))
;       :label "Specify light stimulus orientation for these new light synapses")
      )))
|#

(defun non-light-synapses-for-target-element-menu (target-elt)
  (let* ((original-synapses (loop for syn in (get-node-elements-of-type target-elt 'synapse)
				  when (not (eq (synapse-type-control (synapse-type syn)) 'light))
				  collect syn))
	 (new-type-names
	  (choose-list-values
	   (loop for type-list in (model-parameter-type-library (type-symbol-model 'synapse-type))
		 collect (string (car type-list)))
	   (loop for syn in original-synapses collect (synapse-type-name (synapse-type syn)))
	   :do-all-at-once t :rank-margin 5 :direction :vertical
	   :label (format nil "Choose Non-Light Synapse Types to Remove/Add to ~a" (element-name target-elt)))))
    (loop for syn in original-synapses
	  when (not (string-member (synapse-type-name (synapse-type syn)) new-type-names))
	  do (erase-element syn)
	  else do (setq new-type-names
			(string-remove (synapse-type-name (synapse-type syn)) new-type-names)))
    (create-synapses target-elt new-type-names)))


(defun sources-for-target-element-menu (target-elt)
  (let ((dummy5 (get-node-elements-of-type target-elt 'isource))
	(dummy6 (get-node-elements-of-type target-elt 'vsource)))
    (choose-variable-values
     '((dummy5 "Include current source" :boolean)
       (dummy6 "Include voltage source" :boolean))
     :label (format nil "Modifying Sources For ~a ~A" (string-downcase (string (type-of target-elt)))
		    (element-name target-elt)))
    (cond-every ((and (get-node-elements-of-type target-elt 'isource) (not dummy5))
		 (erase-elements (get-node-elements-of-type target-elt 'isource)))
		((and (get-node-elements-of-type target-elt 'vsource) (not dummy6))
		 (erase-elements (get-node-elements-of-type target-elt 'vsource)))
		((and (not (get-node-elements-of-type target-elt 'isource)) dummy5)
		 (edit-source-stimulus (add-source target-elt 'isource)))
		((and (not (get-node-elements-of-type target-elt 'vsource)) dummy6)
		 (edit-source-stimulus (add-source target-elt 'vsource))))))



(defun choose-associated-elements-types-for-info (associated-elements)
  (choose-list-values
   (no-nils
    (loop for type-symbol in *object-type-symbols*
	  when (and (type-symbol-model type-symbol) (model-print-routine (type-symbol-model type-symbol)))
	  collect (if associated-elements
		      (loop for elt in associated-elements when (eq type-symbol (type-of elt)) return type-symbol)
		      (when (loop for thing in (list-of-all-things type-symbol)
				  when (element-in-circuit thing) do (return t)) type-symbol))))
   nil :do-all-at-once t :label "Element classes for information"))


(defun choose-associated-elements-types-for-documentation (&optional associated-elements)
  (choose-list-values
   (no-nils
    (loop for type-symbol in *object-type-symbols*
	  when (and (type-symbol-model type-symbol) (model-document-routine (type-symbol-model type-symbol)))
	  collect (if associated-elements
		      (loop for elt in associated-elements when (eq type-symbol (type-of elt)) return type-symbol)
		      (when (loop for thing in (list-of-all-things type-symbol)
				  when (element-in-circuit thing) do (return t)) type-symbol))))
   nil :do-all-at-once t :label "Element classes for documentation"))

(defun choose-associated-elements-types-for-edit (&optional associated-elements)
  (choose-list-values
   (no-nils
    (loop for type-symbol in *object-type-symbols*
	  when (and (type-symbol-model type-symbol) (model-edit-routine (type-symbol-model type-symbol)))
	  collect (if associated-elements
		      (loop for elt in associated-elements when (eq type-symbol (type-of elt)) return type-symbol)
		      (when (loop for thing in (list-of-all-things type-symbol)
				  when (element-in-circuit thing) do (return t)) type-symbol))))
   nil :do-all-at-once t :label "Element classes for editing"))

(defun information-for-elements-menu (&optional target-elt (destination :lisp_window))
  (let ((associated-elements (get-associated-elements target-elt t t t)))
    (loop for type-symbol in (choose-associated-elements-types-for-info associated-elements) do
	  (loop for elt in (choose-list-values-from-keys
			    (loop for elt in (if associated-elements
					       (no-nils (loop for elt in associated-elements when (eq type-symbol (type-of elt)) collect elt))
					       (list-of-all-things-in-circuit type-symbol))
				  collect (list (format nil "~A: ~A" (if (electrode-p elt) "ELECTRODE" (type-of elt)) (element-name elt))
						elt))
			    nil :label (format nil "Choose ~a Elements for Information" type-symbol))
		do (case destination
		     (:File (add-element-doc-to-info-file elt))
		     (:Log_file (update-surf-log-file 'print-element (list elt)))
		     (:Info_Window (string-to-info-win
				    (let ((*standard-output* (make-string-output-stream)))
				      (print-element elt)
				      (get-output-stream-string *standard-output*))))
		     (t (print-element elt)))))))    


(defun get-associated-elements (target-elt &optional (include-target t) include-cell include-types)
  (let* ((target-elt (element target-elt))
	 (node-elements (concatenate 'list
				     (when include-cell (list (element-cell target-elt)))
				     (when include-target (list target-elt))
				     (when (typecase target-elt (soma t) (segment t))
				       (node-elements (element-physical-node target-elt)))))
	 (node-element-types (when include-types (loop for elt in node-elements collect (element-type elt)))))
    (no-nils (delete-duplicates (nconc node-element-types node-elements) :test #'equal))))


(defun edit-elements-of-target-element (target-elt)
  (let ((associated-elements (get-associated-elements target-elt t t t)))
    (loop for type-symbol in (choose-associated-elements-types-for-edit associated-elements) do
	  (loop for elt in
		(choose-list-values-from-keys
		 (loop for elt in (if associated-elements
				      (no-nils (loop for elt in associated-elements
						     when (eq type-symbol (type-of elt)) collect elt))
				      (list-of-all-things-in-circuit type-symbol))
		       collect (list
				(format nil "~A: ~A" (if (electrode-p elt) "ELECTRODE" (type-of elt)) (element-name elt))
				elt))
		 nil :label (format nil "Choose ~a Elements for Editing" type-symbol))
;
;
;        (choose-list-values-from-keys
;         (loop for elt in (get-associated-elements target-elt t t t)
;               when (model-edit-routine (element-model elt))
;               collect (list (format nil "~A: ~A" (if (electrode-p elt) "ELECTRODE" (type-of elt)) (element-name elt)) elt))
;         nil :label "Edit instances of these element types"
;         :text (format nil "Elements associated with ~a ~A"
;                       (if (electrode-p target-elt) "ELECTRODE" (type-of target-elt)) (element-name target-elt)) 
;         :do-all-at-once t :rank-margin 5 :direction :vertical)

	do (edit-element elt)
	))))


(defun check-for-soma-with-tree-for-dendrite-current-plot (element plot-list-info)
  (or (not (equal 'dendrite-current (plot-list-info-structure-slot plot-list-info)))
      (let ((element (element element)))
	(and (soma-p element)
	     (cell-segments (soma-cell element))))))
	 

(defun check-particle-data-slot-for-markov (element plot-list-info)
  (typecase (element-type element)
    (particle-type
     (case (plot-list-info-structure-slot plot-list-info)
       (markov-state (eq (particle-type-class (element-type element)) :markov))
       (state t)))
    (t t)))

(defun plot-elements-of-target-element-menu (target-elt)
					; If there is a synapse here, allow plotting of the synapse origin, if it exists.
  (let (element-list pre-synaptic-flags post-synaptic-flags)
    (loop for elt in (get-associated-elements target-elt)
	  do 
	  (when (synapse-p elt)
	    (if (not (equal (synapse-cell-element elt) target-elt)) ; target-elt must be pre-synaptic to someone
	      (progn
		(push (synapse-cell-element elt) element-list)
		(push nil pre-synaptic-flags)
		(push t post-synaptic-flags))
	      (when (cell-element-p (synapse-pre-synaptic-element elt))
		(push (synapse-pre-synaptic-element elt) element-list)
		(push t pre-synaptic-flags)
		(push nil post-synaptic-flags))))
	    
	  (push elt element-list)
	  (push nil post-synaptic-flags)
	  (push nil pre-synaptic-flags))

    (let* ((type-list (delete-duplicates (loop for elt in element-list collect (if (electrode-p elt) 'ELECTRODE (type-of elt))) :test #'equal))
	   info-list)
      (loop for type in (choose-list-values type-list nil :label "Edit plotting for these element types"
					    :do-all-at-once t :rank-margin 5 :direction :vertical)
	    do
	    (setq info-list nil)
	    (loop for elt in element-list
		  for pre-synaptic-flag in pre-synaptic-flags
		  for post-synaptic-flag in post-synaptic-flags
		  when (equal (if (electrode-p elt) 'ELECTRODE (type-of elt)) type)
		  collect elt into filtered-element-list
		  and collect pre-synaptic-flag into filtered-pre-synaptic-flags
		  and collect post-synaptic-flag into filtered-post-synaptic-flags
		  finally 
		  (loop for element in filtered-element-list
			for pre-synaptic-flag in filtered-pre-synaptic-flags
			for post-synaptic-flag in filtered-post-synaptic-flags
			do
			(let ((element-table (get-hash-table element)))
			  (loop for matched-plot-list-info in
				(loop for plot-list-info in *plot-lists-info*
				      when (and
					    (check-for-soma-with-tree-for-dendrite-current-plot element plot-list-info)
					    (check-particle-data-slot-for-markov element plot-list-info)
						(= (length (plot-list-info-tables plot-list-info)) 1)
						(loop for table in (plot-list-info-tables plot-list-info) 
						      when (and (eq element-table table)
								(or t (not (eq element-table (SYNAPSE-HASH-TABLE)))
								    (not (eq (plot-list-info-structure-slot plot-list-info) 'Event))
								    (and (eq 'auto (synapse-type-control (synapse-type element)))
									 (eq 'voltage (synapse-type-control (synapse-type element))))))
						      do (return t)))
				      collect plot-list-info)
				do (push (list (format nil "~A: ~A~A"
						       (element-name element)
						       (slot-descriptor-string
							(car (plot-list-info-tables matched-plot-list-info))
							(plot-list-info-structure-slot matched-plot-list-info))
						       (cond
							(pre-synaptic-flag (format nil " (pre-synaptic to ~A)" (element-name target-elt)))
							(post-synaptic-flag (format nil " (post-synaptic to ~A)" (element-name target-elt)))
							(t "")))
							       
					       (list (plot-list-info-names matched-plot-list-info) ; the *plot-??* global
						     element))
					 info-list)))))
	    (let ((already-chosen (loop for elt-info in info-list
					when (string-member (element-name (cadadr elt-info)) (symbol-value (caadr elt-info)))
					collect (cadr elt-info))))

	      (loop for elt-info in info-list
		    do (setf (symbol-value (caadr elt-info)) (string-remove (element-name (cadadr elt-info)) (symbol-value (caadr elt-info)))))

	      (loop for var-elt in (choose-list-values-from-keys info-list already-chosen
								 :rank-margin 1 :punt-if-only-one-entry nil :do-all-at-once t
								 :rank-margin 2 :direction :horizontal
								 :label (format nil "~A Choose Plotting Related to ~A" (element-name target-elt) type))
		    do (push (element-name (cadr var-elt)) (symbol-value (car var-elt))))
	      (when (cell-element-p target-elt)
		(REMOVE-ENTRY-FROM-*PLOT-NODE-ELEMENTS* (element-name target-elt))))
	    (choose-plot-data)))))
	  
(defun numerical-parameters-menu ()
  (let ((dummy1 (cond (*use-time-list* :use_step_list)
		      (*use-fixed-step* :use_fixed_step)
		      (t :variable_time_step)))
	(dummy2 (if *eval-all-synapses-every-step* :Evaluate_synapses_@_every_step
		    :Enable_synapse_evaluation_skip))
	(first-time t))
    (loop until (and (not first-time) (<= *pick-time-step-fudge* 1.0)) do
	  (choose-variable-values
	   `((dummy1 "Choose method to determine time step:" :choose (:variable_time_step :use_fixed_step :use_step_list))
	     ("Parameters for Variable (LTE-based) Time Step" :comment)
	     (*absolute-voltage-error* "Absolute voltage error [mV] for LTE-based time step:" :number)
	     (*CONSIDER-PARTICLE-ERROR* "Estimate particle state error for LTE-based time step" :boolean)
	     (*absolute-particle-error* "Absolute particle error [state]:" :number)
	     (*CONSIDER-conc-int-ERROR*
	      ,(concatenate 'string
		(format nil "Estimate concentration integrator error for LTE-based time step~%")
		(format nil "(only when integrators evaluated in inner loop)"))
	      :boolean)
	     (*absolute-conc-int-error* "Absolute concentration integrator error [mM]:" :number)
	     (*eval-conc-ints-in-inner-loop* "Evaluate conc-ints in inner loop" :boolean)

	     (*PUNT-LTE-WHEN-MIN-STEP-REACHED* "Ignore LTE estimate if below minimum step" :boolean)
	     (*user-min-step* "Minimum time step [ms] for variable step:" :number)
	     (*user-max-step* "Maximum time step [ms] for variable step:" :number)
	     (*pick-time-step-fudge* "Fudge factor for LTE-based time step (<= 1.0):" :number)
	     (*INCLUDE-VSOURCE-NODES-IN-NODE-ERROR-EST* "Include vsource nodes in node error estimate" :boolean)
	     ("Parameters for Fixed Time Step" :comment)
	     (*user-step* "Fixed time step [ms]:" :number) 

	     ("Parameters for Using Time Step List" :comment)
	     (*auto-refresh-last-sim-reverse-time-list* "Update time step reference list using last simulation" :boolean)

	     ;; ("Synapse evaluation parameters" :comment)
	     ;; (*use-constant-element-matrix-contribution* "Enable element evaluation step skipping" :boolean)
	     ;; (dummy2 "Synapses evaluation criteria:"
	     ;; :choose (:Evaluate_synapses_@_every_step :Enable_synapse_evaluation_skip))
	     ;; (*eval-all-synapses-every-step* "Evaluate the synapses at every iteration/time step" :boolean)
	     ;; (*synapse-evaluation-step* "When skipping enabled, step for synapse evaluations [ms]" :float)

	     ("General Parameters" :comment)
	     (*save-data-step* "Plotting Resolution - Time steps/data point:" :integer)
	     (*print-numerical-details* "Include parameters of numerical method in print outs" :boolean))
	   :text (if (<= *pick-time-step-fudge* 1.0)
		     "Integration method for concentration integrators set from concentration integrator's menu."
		     (format nil "Time step fudge factor must be <= 1.0!"))
	   :label "Time Step and Numerical Integration Parameters")
	  (setq first-time nil))
    (case dummy2 (:Evaluate_synapses_@_every_step
		  (setq *eval-all-synapses-every-step* t)))
    (case dummy1
      (:use_step_list
       (setq *use-time-list* t
	     *use-fixed-step* nil))
      (:use_fixed_step
       (setq *use-time-list* nil
	     *use-fixed-step* t))
      (t (setq *use-time-list* nil
	       *use-fixed-step* nil)))))

;;; GLOBALS-MENU This allows changing of some global flags, such as for debugging.
(defun globals-menu ()
  (let (dummy2 dummy3 dummy4 dummy5 dummy6 dummy7)
    (choose-variable-values
     '((dummy3 "GC Parameters" :boolean)
       (dummy4 "Miscellaneous Parameters" :boolean)
       (dummy5 "Debugging Parameters" :boolean))
     :label "Edit Simulator Global Variables")
    (cond-every
     (dummy2 (numerical-parameters-menu))
     (dummy3 (choose-variable-values
	      '((*beep-after-gc* :boolean)
		(*GC-ANNOUNCE-text* :boolean)
		(*BEEP-AFTER-SURF* :boolean)
		(*use-gc-announce-window* :boolean)
		;; (*print-out-gc* :boolean)
		(*log-gc-to-file* :boolean))
	      :label "GC Parameters"))
     (dummy5 (choose-variable-values
	      `((*use-max-iterations* :boolean)
		(*max-iterations* :integer)
		
		(*break-on-every-step* :boolean)
		(*debug-voltage-error-step-change* :boolean)
		(*debug-particle-error* :boolean)
		(*debug-particle-error-step-change* :boolean)
		(*debug-conc-int-error* :boolean)
		(*debug-conc-int-error-step-change* :boolean)
		
		(*count-error-step-change* :boolean)
		(*debug-time-trace*  :boolean)
		(*debug-backup*  :boolean)
		(*debug-lte :boolean)
		(*debug-at-time-steps*  :boolean)
		(*debug-all-iterations*  :boolean)
		(*debug-node-name*
		 ,(format nil "For printing node states, just use this node~%(if no entry, then all nodes printed)")
		 :string)
		(*debug-hines* :boolean)
		(*print-matrix*  :boolean)
		(*DEBUG-SEGMENTS* :boolean)
		(*DEBUG-NUMERICAL* :boolean)
					; (*announce-consing-segments* :boolean)
		(*debug-use-time-list* :boolean)
					;                (*debug-diag* :boolean)
					;                (*debug-dc* :boolean)
					;                (*debug-consing :boolean)
					;                (*debug-conc-ints :boolean)
					;                (*debug-var-e-ca :boolean)
					;                (*debug-eval-channel-1 :boolean)
					;                (*debug-eval-channel-2 :boolean)
					;                (*debug-eval-channel-3 :boolean)
					;                (*debug-hines-step :boolean)
					;                (*debug-save-data :boolean)
					;                (*debug-set-sources :boolean)
					;                (*debug-init-all-nodes :boolean)
					;                (*debug-eval-all-elements :boolean)
					;                (*debug-eval-all-nodes :boolean)
					;                (*debug-eval-elements :boolean)
					;                (*DEBUG-EVAL-SEGMENT :boolean)
					;                (*DEBUG-EVAL-channel :boolean)
					;                (*DEBUG-EVAL-particle :boolean)
					;                (*DEBUG-EVAL-soma :boolean)
		)
	      :label "Debugging Parameters"))
     (dummy4 (let ((dummy1 *particle-look-up-table-precision*))
	       (choose-variable-values
		`(       (*printer* "Specify printer" :string)
		  ;; (*print-simulator-time* "Print run and user time stats after simulation" :boolean)
		  (*show-time-remaining* "Show time remaining during simulation" :boolean)
		  (*show-time-remaining-update-time* "Time (actual) in seconds between time window updates."
		   :integer)
		  (*session-name* "Name this session (spaces will map to _)" :string)
		  (*circuit* "Circuit name" :string)
		  (*SIMULATION-PLOT-WINDOW-COMMENT* "Comment for plot windows" :string)
		  (*username* "User name" :string)
		  (*always-add-host-name-to-windows* :boolean)
		  (*displayed-host-name* :string)
		  (*kill-extra-messages* "Kill extra messages" :boolean)
		  (*KILL-ALL-OUTPUT* "Suppress all screen output" :boolean)
		  (*always-clear-models* :boolean) (*always-clear-types* :boolean)
		  (*use-node-voltage-initializations* :boolean)
		  (*FIND-STEADY-STATE* :boolean)
		  (*interpolate-particle-arrays* :boolean)
		  (*particle-look-up-table-precision* :number)
		  (*estimate-particle-error-with-full-time-step*
		   "Estimate particle LTE over 1/2 back step and full forward step"  :boolean)
		  (*print-linear-analysis* :boolean)
		  (*print-nonlinear-analysis* :boolean)
		  (*integral-base* "Reference voltage for integrating plot traces" :number)
		  (*average-integrals* "Average integrals of analyzed nodes" :boolean)
		  (*print-axon-spike-times* :boolean)
		  (*print-synapse-event-times* :boolean)
		  (*print-synapse-total-events* :boolean)
		  (*raise-output-windows* :boolean)
		  (*deiconify-output-windows* :boolean)
		  (*only-load-passive* :boolean)
		  ; (*show-time-divisor* "Number of timer window updates per simulation run." :integer)
		  ; (*motif-gui-p* "Motif look-and-feel" :boolean)
		  )
		:label "Miscellaneous Parameters")
	       ;; Redo particle arrays since the precision has changed.
	       (unless (= dummy1 *particle-look-up-table-precision*) (make-needed-v-particle-arrays t)))))))
	       
(defun circuit-file-browser ()
  (or *automatic-run*
      (let ((path (file-browser "Circuit File Browser"
				(or *circuit-directory* *surf-home*)
				'("asc" "fix"
				  "lisp" "sparcf" "fasl" "circuit")
					;                                (case *circuit-file-type*
					;                                  (:neurolucida '("asc" "fix"))
					;                                  ((:ntscable :surf-functions :lisp) '("lisp" "sparcf" "fasl" "circuit")))
				)))
	(if path
	  (let ((file (file-namestring path)))
	    (setq *circuit-file* file
		  *circuit-filename* (namestring path)
		  *filename* (namestring path)
		  *circuit-directory* (directory-namestring path)
		  *circuit* (pathname-name file))
	    (setq *simulation-name* *circuit*
		  *circuit-parts* (list (pathname-name file))))
	  :cancel))))

(defun data-file-browser ()
  (let ((path (file-browser "Data File Browser" (or *data-directory* *surf-home*) '("dat" "elts"))))
    (when path
      (setq *data-directory* (directory-namestring path))
      (load path))))

(defvar *browser-directory* nil)

(defun file-browser-loader (extensions-list label)
  (let ((path (file-browser label (or *lisp-file-directory* *surf-home*) extensions-list)))
    (when path
      (setq *lisp-file-directory* (directory-namestring path))
      (load path))))
	
(defun doc-file-browser ()
  (let ((path (file-browser "Documentation File Browser"
			    (if (> (length *doc-file*) 0) (pathname *doc-file*) (concatenate-strings *surf-home* "doc/"))
			    '("doc" "tex" "info"))))
    (when path (setq *doc-file* (namestring path)))))

(defun EDIT-all-THINGs-NAME-MENU ()
  (loop for type in
	(choose-list-values (loop for type in *OBJECT-TYPE-SYMBOLS*
				  when (and (not (eq type 'node)) (> (hash-table-count (get-type-hash-table type)) 0))
				  collect type)
			    nil :do-all-at-once t :rank-margin 5 :direction :vertical :label (format nil "Select types of objects to change names"))
	do (edit-thing-name-menu type)))

(defun edit-thing-name-menu (type)
  (loop for thing in (things-of-names
		      (choose-list-values (names-of-things (LIST-OF-ALL-THINGS type)) nil
				  :do-all-at-once t :rank-margin 5 :direction :vertical :label (format nil "Select ~a names to change" type))
		      type)
	do (let ((dummy1 (element-name thing type)))
	     (choose-variable-values `((dummy1 ,(format nil "Edit ~a name:" type) :string)))
	     (SET-ELEMENT-NAME thing dummy1))))

(defun choose-specific-elements (elements &optional selected-elements &key
					  (label "Choose Some Items") text
					  (punt-if-only-one-entry t) only-one-choice do-all-at-once
					  (max-per-menu 10) rank-margin (direction :vertical) (max-height 440))
  (let ((list-of-key-values (loop for elt in elements collect (list (element-name elt) (element elt))))
	(selected-values (element selected-elements)))
    (choose-list-values-from-keys list-of-key-values selected-values
				  :label label :text text
				  :punt-if-only-one-entry punt-if-only-one-entry :only-one-choice only-one-choice :do-all-at-once do-all-at-once
				  :max-per-menu max-per-menu :rank-margin rank-margin :direction direction :max-height max-height)))
