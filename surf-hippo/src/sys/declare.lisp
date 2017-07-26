;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;   -*-
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


;;; SYS Source file: declare.lisp

;; ****************************************
;;; Various declarations of global variables.
;;;
;;; Other files may define globals as well.
;; ****************************************


(in-package "SURF-HIPPO")


;; ****************************************
;;
;; Types
;;
;; ****************************************

(deftype vec-bool () '(simple-array boolean *))

(deftype vec-fix () '(simple-array fixnum *))

(deftype element-fixnums () '(simple-array fixnum *))

(deftype vec-flt () '(simple-array single-float *))

(deftype 1d-flt () '(simple-array single-float (*)))

(deftype 1d-df () '(simple-array double-float (*)))

(deftype vec-df () '(simple-array double-float *))

(deftype element-floats () '(simple-array single-float *))

(deftype element-double-floats () '(simple-array double-float *))

(deftype 2dfloat () '(simple-array single-float (* *)))

(deftype 2ddfloat () '(simple-array double-float (* *)))

(deftype matrix-float () '(SIMPLE-ARRAY double-FLOAT 1))

(deftype markov-particle-transition-array () '(simple-array (or null double-float function vec-df) (* *)))

(deftype ub32 () '(unsigned-byte 32))



;; ****************************************
;;
;; The (not strict) naming convention for the variables is -
;;  
;;   VARIABLE-NAME   = local variable
;;   *VARIABLE-NAME  = global variable
;;   *VARIABLE-NAME* = global variable
;;
;; However, there are still some globals without asterisks.
;;
;; ****************************************

;; At initialization, this is a list of all the variables defined in the SURF package.
;; This list is used to determine if a given symbol has been defined (e.g. by the user)
;; after initialization.

(defvar *original-surf-variable-symbols* nil)


;; ****************************************
;;
;;  Temperature and Temperature Dependent Parameters
;;
;; ****************************************

(defvar *Temperature* 300.0 "Temperature of the simulation in degrees Kelvin. In general this
variable should not be set by the user to set the temperature - set *TEMP-CELCIUS* instead.")
(defvar *Temp-celcius* 27.0 "Temperature of the simulation in degrees Celcius. This is the global
variable that should be changed by the user for setting the temperature.")
(defvar *last-simulation-temperature* nil) ; just to make changing the temp easier
(defvar *LAST-SIMULATION-TEMP-celcius* 27.0)

(defvar *update-temperature-dependent-parameters* nil)


(defvar *ignore-q10* nil)		; When this flag is true then the only temperature dependence in the
					; (:HH-EXT class) particle kinetics (rate constant calculation) occurs via
					; the 1/T factor in the exponential argument.



;; ****************************************
;;
;;  General Simulator Variables
;;
;; ****************************************

(defvar  *USERNAME* "")
(defvar *comment-string* "")		; Comment with formatting characters
(defvar *circuit* "" "The name of the circuit function or file (string) (w/o file extension).")
(defvar *circuit-function* "" "The name of the just loaded circuit function.")

(defvar *initialize-before-next-circuit* t
  "Initialize and clear loaded circuit before next circuit.  If NIL, then new circuit will be added
to existing one.")

(defvar *multiple-source-circuit* nil "When more than one file or function defined the current circuit.")

(defvar *circuit-loaded* nil "As soon as any circuit is loaded, this is T.")
(defvar *circuit-drawn* nil "As soon as any circuit is drawn, this is T.")
(defvar *circuit-processed* nil) ; To avoid nested calls to PROCESS-CIRCUIT-STRUCTURE.
(defvar *simulation-name* "" "Automatically generated name for the current simulation.")
(defvar *auto-update-sim-name* t)

(defvar *simulation-initialized* nil "T when circuit has been initialized for simulation.")
(defvar *simulation-in-progress* nil "T while a simulation is running.")
(defvar *simulation-finished* nil "T as soon as a new circuit simulation is finished.")
;(defvar *current-simulation-finished* nil "When the current simulation is finished, this is T -
;during a simulation it is nil.")  






(defvar *simulation-annotation* "")

(defvar *include-simulation-annotation* nil)


(defvar *session-name* "" "A session name can be used to delinate a series of experiments.")
(defvar *surf-interactive* t  "True if the program is being run interactively.")
(defvar *load-only* nil)
(defvar *WRITE-LOG-FILE* nil "Keep a running log file of all output to the Lisp window.")
(defvar *kill-extra-messages* nil "For text-wise silent simulations.")
(defvar *suppress-element-creation-messages* nil "Suppresses various non-fatal messages generated during element creation.")


(defvar *kill-all-output* nil "For suppressing all output.")
(defvar *beep-after-surf* t "Beep after every simulation.")
(defvar *print-simulator-time* nil)

(defvar *log-gc-to-file* nil "When true [default NIL], GC messages will be written to a text file.")

(defvar *use-gc-announce-window* nil)	; problems with save-image version.

(defvar *GC-ANNOUNCE-text* nil)

(defvar *beep-after-gc* t "When true [default], GC will beep when done. Useful signal for long
simulations to verify machine is breathing.")


(defvar *show-time-remaining* t
  "Display a simulation timer window. Best not used when running series of fast simulations.")

(defvar *last-show-time-remaining-time* 0)

(defvar *show-time-remaining-update-time* 1 "Time (actual) in seconds between time window updates [integer>0].")

; (defvar *show-time-divisor* 50 "Number of timer window updates per simulation run.")

(defvar *simulation-actual-time* 0)	; Keeps track of the actual time as given by GET-UNIVERSAL-TIME.

;; ****************************************
;;
;; Related to Circuit Loading and Definition
;;
;; ****************************************

(defvar *always-clear-models* nil "Wipe out the previous list of models whenever a circuit is read in.") 
 
(defvar *always-clear-types* nil "Wipe out the previous element type hash tables - synapse, channel,
particle, etc. - whenever a circuit is read in.") 


(defvar *input-is-function* t "Whether the circuit description is a compiled function, or the name
of file. Normally set automatically.")

;; Set by files generated by ntscable
(defvar *process-ntscable-list* nil)

(defvar *cell-name-suffix* nil "When non-NIL, is automatically added as a suffix to the name of a
cell created by CREATE-CELL.")
(defvar *next-cell-name* nil "If non-NIL, then this is used for the name of the next created cell,
overriding any other specfication. This is cleared after CREATE-CELL is called.")

(defvar *add-cell-name-to-segs* nil "Add cell name to segment name, unless *USE-SIMPLE-NAMES* is T.")
(defvar *add-cell-name-to-segs-for-tree-dump* t)

(defvar *circuit-file-type* :lisp "Pertaining to the circuit definition file. Values include
:NEUROLUCIDA and :LISP.")


(defvar *circuit-functions* nil "A list of circuit functions that can be selected from the input
menus instead of typing them in.")

(defvar *circuit-parts* nil "If the loaded circuit is composed of more than one function and/or
file then this list includes the names of the components.") 


(defvar *loaded-circuit-parts* nil "These are the circuits that have actually been loaded.")


(defvar *circuit-source* :Catalog_Function "A symbol which says how the circuit was loaded into the
system. Possible values include :Catalog_Function, :Function, or :File.")

(defvar *use-simple-names* nil "When true, membrane element and segment names are just integers.")
(defvar *cell-element-simple-name-counter* 0)
(defvar *synapse-simple-name-counter* 0)
(defvar *channel-simple-name-counter* 0)
(defvar *particle-simple-name-counter* 0)
(defvar *conc-particle-simple-name-counter* 0)
(defvar *axon-simple-name-counter* 0)
(defvar *pump-simple-name-counter* 0)
(defvar *buffer-simple-name-counter* 0)

(defvar *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES* t "Prompt for alternate element names of duplicate elements.")
(defvar *enable-automatic-cell-names* t)
(defvar *allow-duplicate-synaptic-connections* t "Allow duplicate synaptic connections.")

(defvar *allow-duplicate-elements* t "When false, only one synapse, channel or other membrane
element of a given type can be added to the same cell element.")

;; Tree consolidation
(defvar *maximum-electrotonic-length* 0.25)
(defvar *neuron-tree-consolidated* nil)

(defvar *announce-consing-segments* nil)


;; Steady state determination.
(defvar *advance-sources* t)
(defvar *find-steady-state* nil)
(defvar *minimal-capacitance* 1.0e-6 "Temporary capacitance value used for cell elements during low
capacitance steady-state determination [nF].") 
;(defvar *minimal-capacitance* 1.0e-12) ; nF
(defvar *plot-steady-state* nil)
(defvar *pseudo-steady-state-time* 3.0)
(defvar *low-cap-pseudo-steady-state-time* 0.1)
(defvar *run-reg-cap-for-ss* nil)



;; *** Miscellaneous ***



(defvar *cool-hippo-window* nil)

(defvar *cool-hippo-window-p* nil "Enables an advertisement.")


(defvar *main-menu-image*
  (create-instance nil opal:bitmap
		   (:filling-style (create-instance nil opal:filling-style (:fill-style :stippled)))
		   (:image (opal:read-image
			    (namestring (concatenate-strings *SURF-HOME* "lib/pix/cool-hippo.small.bmp"))))))

;; (defvar *break-during-simulation* nil "Useful for debugging)

(defvar *active* t "Enables evaluation of all channels, axons, pumps, and concentration integrators.")

(defvar *are-there-synapses* nil)

(defvar *only-load-passive* nil "When T, both channel and synapse creation is blocked when loading a new circuit.")

; (defvar *synapses-active t)		; When nil, all synapse are blocked in some circuit functions

;; This is used to modulate all cell synapse and channel conductances by a common factor.
(defvar *global-membrane-conductance-factor* 1.0)

;; These may be nil in cases where there is no change in the appropriate type of element between simulations.
(defvar *setup-elements* t)
(defvar *setup-axons* t)
(defvar *setup-synapses* t)
(defvar *setup-channels* t)
(defvar *setup-conc-ints* t)
(defvar *setup-pumps* t)
(defvar *setup-conc-particles* t)
(defvar *setup-sources* t)
(defvar *setup-particles* t)


(defvar *enable-reorder-conc-particles-of-type*
  t
  "Disable this when making or destroying a large number of conc-particles of a given type, making sure to
run REORDER-CONC-PARTICLES-OF-TYPE explicitly afterwards.")

(defvar *enable-reorder-particles-of-type*
  t
  "Disable this when making or destroying a large number of particles of a given type, making sure to
run REORDER-PARTICLES-OF-TYPE explicitly afterwards.")

(defvar *enable-reorder-synapses-of-type*
  t
  "Disable this when making or destroying a large number of synapses of a given type, making sure to
run REORDER-SYNAPSES-OF-TYPE explicitly afterwards.")

(defvar *enable-reorder-channels-of-type*
  t
  "Disable this when making or destroying a large number of channels of a given type, making sure to
run REORDER-CHANNELS-OF-TYPE explicitly afterwards.")



;; When these flags are true, the calculation of the dendritic tree input impedance stores locally.
;; values of the input impedance in each segment.
(defvar *store-segment-z-cable-in* nil)
(defvar *store-segment-z-discrete-in* nil)


(defvar *advance-autonomous-elements* t) ; When nil, autonomous elements are always evaluated at their initial state.



;; ****************************************
;;
;; File Related
;;
;; ****************************************

;; These 3 vars are actually defined in surf-hippo/loaders/surf-hippo-loader.lisp
;; (defvar *SURF-USER-DIR* "" "Directory for simulation data, set from the Unix environment variable $SURFUSERHOME." )
;; (defvar *SURF-USER-HOME* "" "User home directory, set from the Unix environment variable $HOME." )
;; (defvar *SURF-HOME* "" "Top-level Surf-Hippo directory, set from the Unix environment variable $SURFHOME." )


(defvar *circuit-filename* nil)		; The full namestring of the circuit file.
(defvar *circuit-file* "")		; The filename string (w/o path, and with extension) of the circuit file.
(defvar *circuit-directory* "" "Default is given by value of *SURF-USER-DIR*.")


(defvar *filename* nil)
(defvar *ADD-SIMULATION-TO-FILENAMES* nil "For files written by the simulator.")

(defvar *lisp-file-directory* nil)	; For various lisp files to be loaded from the menus.
(defvar *data-directory* "" "Default given by concatenation of *SURF-USER-DIR* and /data/.")
(defvar *doc-file* "")
(defvar *plot-directory* "" "Default given by concatenation of *SURF-USER-DIR* and /plot/.")

(defvar *make-circuit-subdir* t)
(defvar *update-*plot-directory* t)
(defvar *update-*data-directory* t)
(defvar *last-simulation-file-path* nil "The name of the last file written (w/o type).")


;; ****************************************
;;
;; Hash Tables for Circuit Elements
;;
;; ****************************************


(defvar *model-hash-table* nil) ; A table of all model models by name. When this is NIL on
				; startup, CREATE-MODELS is called. Now, models.lisp calls this on load.
(defvar *type-model-names*
  '("buffer-type" "pump-type" "channel-type" "conc-int-type" "particle-type" "conc-particle-type"
    "synapse-type" "axon-type" "cell-type")) 

(defvar *type-model-symbols* (loop for string in *type-model-names* collect (read-from-string string)))

(defvar *instance-model-names*
  '("segment" "soma"  "channel" "synapse"
    "cell"
    "buffer" "pump" "isource"     "conc-int"  "particle"  "conc-particle"
      "axon"  "vsource" "extracellular-electrode")) 

(defvar *instance-model-symbols* (loop for string in *instance-model-names* collect (read-from-string string)))

;; Everything that should have direct user access.
(defvar *object-type-symbols*
  (append
   (copy-list *instance-model-symbols*)
   (copy-list *type-model-symbols*)
   '(electrode)				; This isn't really a type, just a hash-table "type"
   '(node)
   )
  "The symbols for all neuron circuit object types. The order of types in this list sets the search priority for the ELEMENT function."
  )

;; Put NODE at end of this list
(defun strings-from-object-type-symbols ()
  (loop for sym in (append (remove 'node *object-type-symbols*) '(node))
	collect (string-downcase (format nil "~A" sym))))

(defvar *object-type-symbol-strings* (strings-from-object-type-symbols))

#|
(defun add-object-type (name
			&key
			parameter-type-library
			save-output-data-routine
			output-data-structure-variables
			output-data-keys
			edit-routine
			eval-routine
			print-routine
			short-print-routine
			document-routine
			create-routine)
  "For adding type NAME to the element system."
  (let ((type-sym (typecase name
		    (string (read-from-string name))
		    (t name)))
	(type-name (string-downcase (format nil "~A" type-sym))))
    (push type-sym *object-type-symbols*)
    (setq *object-type-symbol-strings* (strings-from-object-type-symbols))
    (create-model type-name
		  :parameter-type-library parameter-type-library
		  :save-output-data-routine save-output-data-routine
		  :output-data-structure-variables output-data-structure-variables
		  :output-data-keys output-data-keys
		  :edit-routine edit-routine
		  :eval-routine eval-routine
		  :print-routine print-routine
		  :short-print-routine short-print-routine
		  :document-routine document-routine
		  :create-routine create-routine)
    (defmacro (read-from-string (format nil "~A-HASH-TABLE" type-name))
	`(model-hash-table (gethash ,type-name *model-hash-table*)))))

|#


;; ***************************************
;;
;; Element Type Parameter Lists are stored in each model's :PARAMETER-TYPE-LIBRARY slot. The
;; *-TYPE-DEF macros are for adding entries to these lists.
;;
;; ***************************************

(defmacro delete-all-cars (body a-list)
  `(loop for form in ,a-list unless (eql (car ,body) (car form)) collect form))


#|
(defun nconc-current-file-to-body (body)
  (nconc body (list (cons 'source (when (eval *load-truename*) (namestring (eval *load-truename*)))))))


;; convert all elements to dotted lists.
(defun dot-all-elements (body)
  (cons (car body)
	(loop for element in (cdr body)
	      collect
	      (if (or (atom (cdr element)) ; dotted pair already
		      (> (length element) 2)) ; dotted list already
		  element		
		  (cons (car element)
			(nth 1 element))))))


(class . :mm)
(class :mm)
(class . (:mm))
      
|#

(defun nconc-current-file-to-body (body)
  (if *load-truename*
      (nconc body (list (cons 'source (namestring *load-truename*))))
      body))

#|
(defmacro element-type-def (type-symbol body)
  `(let ((model (get ,type-symbol `model) ; (gethash ,element-name *model-hash-table*)
	  )
	 (body (nconc-current-file-to-body ,body)))
    (setf (model-parameter-type-library model) (cons body (delete-all-cars body (model-parameter-type-library model))))
    nil))


(defun parse-two-elt-list-to-dotted (list)
  (cond ((or (not (proper-list-p list))	; Dotted pair detector. '(PROPERTY-A . 34.567)
	     (> (length list) 2))	; Dotted list with atom and sublist with more than one elt.
					; `(property-b . (a b c d))
	 list)			
	((and (= (length list) 2)	; '(property-b (a b c d))) or '(property-c (boo))
	      (consp (nth 1 list)))
	 (cons (car list) (cadr list))))
	      



(defmacro element-type-def (type-symbol body)
  `(let* ((model (get ,type-symbol `model))
	  (body (nconc-current-file-to-body ,body))
	  (new-type (car body))) 
    (setf (model-parameter-type-library model)
     (cons body (delete new-type (model-parameter-type-library model) :key 'car :test 'equal)))
    nil))

|#

(defmacro element-type-def (type-symbol body)
  `(let* ((model (get ,type-symbol `model))
	  (body (nconc-current-file-to-body ,body))
	  (new-type (car body)))
    (delete new-type (model-parameter-type-library model) :key 'car :test 'equal)
    (push body (model-parameter-type-library model))
    nil))


(defmacro cell-type-def (body)
  "Parameter wrapper for cell type definitions."
  `(element-type-def `cell-type ,body))
(defmacro axon-type-def (body)
  "Parameter wrapper for axon type definitions."
  `(element-type-def `axon-type ,body))
(defmacro channel-type-def (body)
  "Parameter wrapper for channel type definitions."
  `(element-type-def `channel-type ,body))
(defmacro particle-type-def (body)
  "Parameter wrapper for particle type definitions."
  `(element-type-def `particle-type ,body))
(defmacro conc-particle-type-def (body)
  "Parameter wrapper for concentration particle type definitions."
  `(element-type-def `conc-particle-type ,body))
(defmacro synapse-type-def (body)
  "Parameter wrapper for synapse type definitions."
  `(element-type-def `synapse-type ,body))
(defmacro conc-int-type-def (body)
  "Parameter wrapper for concentration integrator type definitions."
  `(element-type-def `conc-int-type ,body))
(defmacro buffer-type-def (body)
  "Parameter wrapper for buffer type definitions."
  `(element-type-def `buffer-type ,body))
(defmacro pump-type-def (body)
  "Parameter wrapper for pump type definitions."
  `(element-type-def `pump-type ,body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Some useful pointers, typically to the latest created instance of the element type.

(defvar *SOMA* nil "The last created SOMA.")
(defvar *Segment* nil "The last created segment.")
(defvar *cell* nil "The last created cell.")
(defvar *cell-type* nil "The last created cell-type.")
(defvar *vsource* nil "The last created vsource.")
(defvar *isource* nil "The last created isource.")
(defvar *axon* nil "The last created axon.")
(defvar *axon-type* nil "The last created axon type.")
(defvar *synapse* nil "The last created synapse.")
(defvar *synapse-type* nil "The last created synapse type.")
(defvar *conc-particle* nil "The last created conc particle.")
(defvar *conc-particle-type* nil "The last created conc particle type.")
(defvar *particle* nil "The last created particle.")
(defvar *particle-type* nil "The last created particle type.")
(defvar *channel* nil "The last created channel.")
(defvar *channel-type* nil "The last created channel type.")
(defvar *pump* nil "The last created pump.")
(defvar *pump-type* nil "The last created pump type.")
(defvar *conc-int* nil "The last created concentration integrator.")
(defvar *conc-int-type* nil "The last created concentration integrator type.")
(defvar *buffer* nil ; "The last created buffer."
  )
(defvar *buffer-type* nil ; "The last created buffer type."
  )
(defvar *electrode* nil "The last created electrode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some isource and vsource variables.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *minimum-source-transition-time* 1e-3 "Minimum pulse transition time in milliseconds for PWL sources. If this is too small then source
waveforms can be distorted.")

(defvar *evaluate-fixed-node-voltages* nil)

(defvar *isource*nodes* nil
  "A list of cell elements which will all receive an identical current input as defined by *ISOURCE*
[the last created current source of the sources in the circuit]. For example,

       (setq *isource*nodes* (somas))

Now all somas in the circuit will receive the current waveform specified for *ISOURCE*. Note that if
*ISOURCE*NODES* is not NIL, then the original cell element assigned to the *ISOURCE* will only
receive the current if that element is a member of *ISOURCE*NODES*.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These arrays mirror the corresponding hash-tables for quicker access. NOT ALL OF THE FOLLOWING
;; VARIABLES ARE USED.

(defvar *soma-array* nil) 

(defvar *segment-guts-list* nil)
(defvar *segment-node-2-floats-list* nil)

(defvar *channel-type-list* nil)

;; These lists contain only synapse types that have associated synapses which are to be
;; evaluated in the current simulation.

;; (defvar *USE-PARTICLE-ARRAY* t)

(defvar *particle-type-list* nil)

(defvar *conc-int-type-list* nil)
(defvar *pump-type-list* nil)
(defvar *conc-particle-type-list* nil) 
(defvar *synapse-type-list* nil)
(defvar *axon-type-list* nil) 
(defvar *vsource-array* nil)
(defvar *fixed-vsource-array* nil) 
(defvar *isource-array* nil)
(defvar *non-ideal-vsource-list* nil)	; Activated non-ideal vsources
(defvar *fixed-vsource-list* nil)	; Activated ideal vsources
(defvar *isource-list* nil)

(defvar *vsources* nil)			; This is useful - list of all current vsources.

(defvar *soma-array-length* 0)
(defvar *segment-array-length* 0)

;; Whenever the circuit structure is changed (elements added/removed), the appropriate variable
;; below is set so that the array which points to the type of the changed element can be updated.

(defvar *make-needed-v-particle-arrays* nil)
(defvar *make-needed-v-pump-arrays* nil)
(defvar *make-needed-v-buffer-arrays* nil)
(defvar *make-needed-conc-particle-arrays* nil)


(defvar *make-segment-lists* t)
(defvar *make-node-w/elements-array* t)
(defvar *lte-node-criterium* :all
  "Determines which circuit nodes will be considered for the LTE estimate. Options include :ALL
\(default\), :SOMAS, :CHANNELS, :SYNAPSES, :VSOURCES, :ISOURCES, :AXONS, or a list of circuit
elements that may or may not include the afore-mentioned keywords. If :ALL, then include all circuit
nodes with externally-driven elements (e.g. sources or synapses or channels). If :SOMAS, then
include only somas. If :CHANNELS, :SYNAPSES, :VSOURCES, :ISOURCES or :AXONS, include only those
nodes with the appropriate elements.")

(defvar *last-lte-node-criterium* nil)	; For updating *NODE-W/ELEMENTS-ARRAY* whenever
					; *LTE-NODE-CRITERIUM* has been changed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ****************************************
;;
;; Geometry of the Anatomy
;;
;; ****************************************

(defvar *calculate-absolute-locations* nil)



;; ****************************************
;;
;; Lists for ordering the circuit branches
;;
;; ****************************************

(defvar *branch-list* '())
(defvar *branch-array*)
(defvar *last-seg-branch-array*)
(defvar *reverse-branch-array*)
(defvar *branch-array-limit*)


;; ****************************************
;;
;; Arrays for the Circuit Matrix and Nodes and Related Variables 
;;
;; ****************************************


(defvar *diag-double* (make-array '(1) :element-type 'double-float)) ; The diagonal of the matrix.
(defvar *lower-diag-double* (make-array '(1) :element-type 'double-float)) ; The lower diagonal of the matrix.
(defvar *upper-diag-double* (make-array '(1) :element-type 'double-float)) ; The upper diagonal of the matrix.
(defvar *v-at-half-step-double* (make-array '(1) :element-type 'double-float)) ; The upper diagonal of the matrix.
(defvar *rhs-double* (make-array '(1) :element-type 'double-float)) ; The upper diagonal of the matrix.

;; An array of the core nodes. These are the nodes that need evaluating.
(defvar *core-node-array* nil)

(defvar *CORE-NODE-ARRAY-LENGTH*) 
(defvar *core-node-array-length-1*)

(defvar *ALL-NODE-ARRAY*)
(defvar *ALL-NODE-ARRAY-LENGTH*)
(defvar *ALL-NODE-ARRAY-LENGTH-1*)


(defvar *node-w/elements-array*) ; An array of nodes with membrane elements - for evaluating LTE.
(defvar *node-w/elements-array-length* 0)
;; The number of unknowns in the circuit.
(defvar *num-unknowns* 0)
(defvar *num-nodes* 0 "The number of electrical circuit nodes in the circuit. Not set by user.")

(defvar *ground-node* nil) ; The ground node, if it has been defined. It has to be defined before the simulation can start.


;; ****************************************
;;
;; Variables which are used for dumping and referencing simulation results stored in files.
;;
;; ****************************************

(defvar *archive-variable-list* '()
  "This is a list of sublists, each of which reference variable symbol names from specific
simulations that have been loaded from a data file. The format is:

      (....
       (circuit-and-simulation-name time-variable-symbol
        (element-data-variable-symbol data-type)
        ...
        (element-data-variable-symbol data-type))
       ....
      )
")

(defmacro archive-variable-list-simulation-name (simulation-var-info-list)
  `(string (nth 0 ,simulation-var-info-list)))
(defmacro archive-variable-list-simulation-time (simulation-var-info-list)
  `(symbol-value (nth 1 ,simulation-var-info-list)))
(defmacro ARCHIVE-VARIABLE-LIST-SIMULATION-symbols-and-data-types (simulation-var-info-list)
  `(car (last ,simulation-var-info-list)))

(defmacro ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL (symbol-and-data-type)
  `(car ,symbol-and-data-type))

(defmacro ARCHIVE-VARIABLE-LIST-SIMULATION-SYMBOL-value (symbol-and-data-type)
  `(symbol-value (car ,symbol-and-data-type)))

(defmacro ARCHIVE-VARIABLE-LIST-SIMULATION-data-type (symbol-and-data-type)
  `(cadr ,symbol-and-data-type))


(defvar *ARCHIVE-SESSION-RESULTS* '() "Set by results files (see analysis.doc). The contents of this
list is typically results of analysis done on circuit data (waveforms) at the end of a simulation,
rather than the raw data.") 



(defvar *file-output-variable-list* '()
  "This is a list of the variables and their properties. Each entry has the following format:

      (VAR-SYMB CIRCUIT-ELEMENT DATA-SLOT)

"
)



;; ****************************************
;;
;; Details for Printing Out Simulation Information
;; and Saving Simulation Data and Info to Files.
;;
;; ****************************************

(defvar *simulation-description-destination* :none)

(defvar *simulation-print-detail* :terse
  "Description level for PRINT-CIRCUIT at the start of every simulation. Possibilities include :NONE
:TERSE :MEDIUM :FULL and :FULL_WITH_SEGMENTS.")


(defvar *simulation-file-name* nil)
(defvar *save-simulation-data* t "Enables saving of simulation data.")
(defvar *save-simulation-data-to-file* nil "Enables saving of simulation data to file.")
(defvar *save-simulation-info* nil)
(defvar *save-full-simulation-info* nil)

;; This is set to some form by the user which is then written to the results file, along with
;; *SIMULATION-NAME* to supply convenient tags for later data analysis.

(defvar *simulation-results-addendum* nil)

;; A list of symbols which are printed out with their values by PRINT-CIRCUIT.

(defvar *documented-user-variables* nil "An explicit list of global variables that will printed out by PRINT-CIRCUIT. See also
*DOCUMENT-ALL-NEW-VARIABLES*.")

(defvar *DOCUMENT-all-new-VARIABLES* nil "When T, PRINT-CIRCUIT will print out any bound variables that were either defined after initialization (in
SURF package), or in *DOCUMENTED-USER-VARIABLES*.")

(defvar *enable-print-DOCUMENTED-USER-VARIABLES* t "Enable printing of *DOCUMENTED-USER-VARIABLES*.")

; Enables the short analysis printout at end of simulation.
(defvar *print-out-to-info-window* nil)	; To Info window.
(defvar *print-out-to-lisp* t)		; To lisp window.

(defvar *print-full-to-info-window* nil)

(defvar *print-numerical-details* nil)
(defvar *dump-analysis-to-file* nil)

;; These three are old, better to use *SIMULATION-PRINT-DETAIL*
(defvar *print-full-to-lisp* nil)
(defvar *print-mini-to-lisp* nil)
(defvar *include-segments-w-full-description* nil)


;; ****************************************
;;
;;; Simulation/Circuit Modification Monitoring
;;
;; ****************************************

;;; Whenever the following functions are called, the state of *MONITOR-CIRCUIT-MODIFICATIONS*
;;; determines whether a record of the call is printed out to the lisp listener. This record can
;;; then be saved later for helping reconstruct circuits. 
;;;
;;; ADD-CHANNEL-TO-LOCATIONS
;;; ADD-SYNAPSE-TO-LOCATIONS
;;; ADD-CHANNEL-TO-CELLS
;;; ADD-SYNAPSE-TO-CELLS

(defvar *monitor-circuit-modifications* t)

;; ****************************************
;;
;; Graphics and Information Windows
;;
;; ****************************************

(defvar *colorizing-simulation* nil)
(defvar *colorize-simulation* nil "Enable colorization of simulation in some or all histology windows.")
(defvar *monitor-simulation* nil)
(defvar *colorize-simulation-step* 0.5)
(defvar *enable-colorize-time* nil "Enable time display in colorized histology windows.")
(defvar *enable-colorize-scale* nil "Enable color scale display in colorized histology windows.")

(defvar *sparse-data-times* nil)

(defvar *reverse-sparse-data-times* nil)

(defvar *enable-sparse-data* nil "When non-NIL, data from all the circuit elements are stored on a (typically sparse) time grid stored in
*SPARSE-DATA-TIMES*.")

(defvar *create-new-histology-window* nil)	; Create a new histology window.
(defvar *clear-histology-windows nil)	; Wipe out *histology-windows*.

(defvar *create-new-info-window* nil)	; Create a new info window.
(defvar *clear-info-windows nil)	; Wipe out *info-windows*.

(defvar *draw-cells t)

(defvar *include-synapse-name-with-label nil)
(defvar *include-channel-name-with-label nil)

(defvar *include-channel-type-comment-in-particle-plots* t)
(defvar *update-plotted-nodes nil)
(defvar *update-stimuli nil)

(defvar *histology-window-min-height 200)
(defvar *histology-window-min-width 200)
(defvar *minimum-cell-histo-x-span 0.0) ; microns
(defvar *minimum-cell-histo-y-span 0.0) ; microns

(defvar *exaggerate-plotted-node nil)
(defvar *plot-node-pixel-radius 6)

(defvar *scale* 3.0) ; microns/screen pixel
(defvar *label-channels nil)
(defvar *label-synapses nil)

(defvar *label-nodes nil)
(defvar *label-sources nil)
(defvar *label-plotted-nodes t)
(defvar *update-stimulus-graphic nil)
(defvar *where-synapse-stimulus-goes* :back)
(defvar *viewing-phi 0.0)
(defvar *viewing-theta (* (/ 3.0 2.0) (COERCE user::PI 'SINGLE-FLOAT)))	;These values map x'=x and y'=y.



(defvar *show-light-synapse-rf-centers* t)
(defvar *synapse-type-colors* nil)
(defvar *channel-type-colors* nil)
(defvar *node-graphics-coefficient* 2.0)
(defvar *axon-graphics-diameter* 1.0) ; microns
(defvar *synapse-cxn-graphics-diameter*  1.0) ; um
(defvar *syn-rf-connection-thickness* 1)
(defvar *syn-rf-connection-shading* 100)
(defvar *syn-rf-connection-dash* 4)
(defvar *syn-rf-shape-shading* 25)

(defvar *motion-snapshots* 5)		; Number of moving stimulus snapshots
(defvar *label-stimulus-times* t)	; If stimulus drawn, label times of stimulus snapshots

(defvar *stimulus-graphic-shading-percent* 20)

(defvar *histology-window-stimulus-time-distance-from-top* 10)
(defvar *histology-window-stimulus-time-distance-from-left* 15)
(defvar *histology-window-stimulus-time-length* 200)

(defvar *histology-scale-bar-unit-string* "um" "Histology scale bar units")

(defvar *histology-comment* "")

(defvar *draw-soma-outline* t)
(defvar *soma-points* )
(defvar *soma-outline* )
(defvar *draw-soma-polylines* t)

(proclaim '(single-float *viewing-phi *viewing-theta *node-graphics-coefficient* *scale*))

;; ****************************************
;;
;; Things Having To Do With Channel Particles
;;
;; ****************************************

;; (defvar *use-particle-look-up-tables* t)
;; (defvar *use-particle-types* t)


;; The nominal particle voltage range is from -150 to 50 mV. Be sure to change the following
;; variables in a consistent manner.

(defvar *particle-look-up-table-min-voltage* -150.0)
(defvar *particle-look-up-table-min-voltage-double* -150.0d0)
(defvar *particle-look-up-table-voltage-range* 200)
(defvar *particle-look-up-table-precision* 0.1) ; mV
(proclaim '(single-float  *particle-look-up-table-precision*))
(defvar *PARTICLE-LOOK-UP-TABLE-LENGTH* (round (/ *particle-look-up-table-voltage-range* *particle-look-up-table-precision*)))

(proclaim '(type single-float *particle-look-up-table-min-voltage*))
(proclaim '(type double-float *particle-look-up-table-min-voltage-double*))

(proclaim '(type fixnum *particle-look-up-table-length* *particle-look-up-voltage-range*))

(defvar *constant-field-equation-exponential-term-array* nil)
(defvar *use-cf-exp-lookup* nil)

;; ****************************************
;;
;; Plotting and Saving Plot Data
;;
;; ****************************************


(defvar *hard-copy-screen* nil "Hardcopy screen after each simulation.")
(defvar *create-new-simulation-plots* nil "Create a new set of plot windows for each simulation.")

(defvar *massage-element-plot-labels* T "When non-NIL, elements with simple integers as names are
referenced in data plots with elaboration.")


(defvar *plot-standard-windows* t "Enable the standard plotting.")
(defvar *HIDE-plot-WINDOWS* nil "Hide simulation plots, even if they're created.")

(defvar *SAVE-CONDUCTANCES-normalized* nil "Save element conductances in normalized form.")

(defvar *plot-channels-by-major-ion* t "Plot channels by major ion.")
(defvar *plot-synapses-by-major-ion* t	; "Plot synapses by major ion."
  )

;; not used
;;(defvar *plot-currents-by-major-ion* t ; "Plot currents by major ion."
;;  )


;; (defvar *position-plots-by-hand nil)

(defvar *enable-plot-labels* t)
(defvar *use-same-line-style* nil)

(defvar *group-plot-data-by-cell* t "For a given type of data \(e.g. voltage, conductance\), all
traces associated with each cell in the circuit are displayed in their own window or windows.")

(defvar *GROUP-PLOT-DATA-BY-CELL-TYPE* t "For a given type of data \(e.g. voltage, conductance\),
all traces associated with each cell type in the circuit are displayed in their own window or windows.")

(defvars-w-value
    (*plot-voltages-solid nil)
    (*overlay-simulations nil)		; This is now supersed by *OVERLAY-ALL-PLOTS*, defined in plot-hack.lisp
  (*overlay-simulations-last nil)
  (*custom-plot-lists* '())
  (*plot-custom-windows* t))


(defvar *simulation-plot-window-comment* nil "When a string, comment added to all simulation plot windows.")
(defvar *simulation-plot-window-comment-position* :lower-right
  "Position for *SIMULATION-PLOT-WINDOW-COMMENT* that is added to all simulation plot windows.")

(defvar *traces-per-plot* 6 "Unless equal to 0, constrains the number of traces per plot window.") 

(defvar *max-num-traces-for-plot-trace-labels* 6 "When non-nil, if the number of traces in a
simulation plot is more than this number, then the plot trace labels will be suppressed.")

(defvar *particle-kinetics-plot-width* 450) ; pixels
(defvar	*particle-kinetics-plot-height* 350) ; pixels

(defvar *automatic-voltage-plot-scaling t)
(defvar *voltage-plot-min -75.0)
(defvar *voltage-plot-max nil)
(defvar *automatic-soma-voltage-plot-scaling t)
(defvar *soma-voltage-plot-min -75.0)
(defvar *soma-voltage-plot-max 30.0)



(defvar *save-data-step* 2 "Data and time are saved onto the appropriate lists every *SAVE-DATA-STEP* time steps.") 
(defvar *save-data-step-count*)		;Keeps track of when to save the data.


(defvar *label-surf-plots* t)
(defvar *update-plots* t)
(defvar *resurrect-plots* nil)
(defvar *visible-plots* t)




(defvar *plot-total-concs-separately* t "For plotting out concentration integrator total concentrations.")
;; (defvar *plot-shell-concs-separately* t "For plotting out concentration integrator concentrations.")

;; These flags enable/disable plotting windows of various types.

(defvar *plot-event-generators* nil)
(defvar *plot-events* t)

(defvar *plot-node-elements* '() "A reference or list of reference, for each everything on the
assoicated cell node will be plotted.")

(defvar *plot-separate-soma-window nil)


;; (defvar *plot-concentrations nil)
;; (defvar *transformed-plot nil)

(defvar *plot-lists-cells* '())
(defvar *plot-lists-cell-types* '())

(defvar *total-conductances-data* nil)
(defvar *plot-total-conductances-p nil "Whether to collect and plot total conductances as defined in
*PLOT-TOTAL-CONDUCTANCES*.")

(defvar *debug-total-conductances* nil)

(defvar *plot-total-conductances* nil
  "Summed conductances over cells and/or membrane element types are plotted when this is non-NIL. The
format is a list of atoms or lists, as follows:

      :ALL
      TYPE
      CELL
      (CELL TYPE TYPE ...)
      (CELL :ALL)

where TYPE refers to a synapse or channel type and CELL refers to a cell or cell type. If a cell or
cell type is indicated, then the total linear membrane conductance is saved for that cell or for all
cells of the cell type, as appropriate. If a cell or cell type is in a list followed by synapse or
channel types, then the linear membrane conductance is summed with the conductance of all the
synapses or channels of the indicated types. If a cell or cell type is in a list followed by :ALL,
then the summation is taken over all synapse and channel types in a given cell or cells of a cell
type. In general, use the function SETUP-PLOT-TOTAL-CONDUCTANCES to set up this variable. If the
only entry is :ALL, then the total conductance of all cells in the circuit will be plotted.")

(defvar *plot-total-conductance-specifications* nil)


#|

#|

These flags enable various plots:

*PLOT-SOMA-VOLTAGE-P
*PLOT-SOMA-VOLTAGE-DERIVATIVE-P
*PLOT-NODE-VOLTAGES-P ...


If one of the following globals is set to 'ALL, then the function CHOOSE-PLOT-DATA will set
it to a list of all the instances of the appropriate circuit element. Otherwise, the lists
should contain the names (strings) of the elements to be plotted:

*PLOT-NODES*
*PLOT-NODE-DERIVATIVES*
*PLOT-AXONS*
*PLOT-AXON-EVENTS* ...

These mirror the previous lists, but hold the associated structures:

*PLOT-NODES-STRUCTURES*
*PLOT-NODE-DERIVATIVES-STRUCTURES*
*PLOT-AXONS-STRUCTURES*
*PLOT-AXON-EVENTS-STRUCTURES* ...

|#


*PLOT-LISTS-INFO* global variable structure:

    global-var-list-of-structure-names
    list-of-tables
    structure-slot
    enable-var
    global-var-list-of-structures
    plot-y-label

|#

(defmacro plot-list-info-names (plot-list-info) `(nth 0 ,plot-list-info))
(defmacro plot-list-info-types (plot-list-info) `(nth 1 ,plot-list-info))
(defmacro plot-list-info-tables (plot-list-info)
  `(loop for sym in (nth 1 ,plot-list-info)
    collect (get-type-hash-table sym)))

;; e.g. 'conductance, 'current, 'voltage, etc.
(defmacro plot-list-info-structure-slot (plot-list-info) `(nth 2 ,plot-list-info))

(defmacro plot-list-info-enable-var (plot-list-info) `(nth 3 ,plot-list-info))
(defmacro plot-list-info-structures (plot-list-info) `(nth 4 ,plot-list-info))

;; e.g. "nA", "mV", etc.
(defmacro plot-list-info-units-label (plot-list-info) `(nth 5 ,plot-list-info))

(defvar *plot-lists-info*
  `(;; (*colorize-nodes* (segment soma) "colorize node voltage" )
    (*plot-nodes* (segment) voltage *plot-node-voltages-p *plot-nodes-structures* "mV")
    (*plot-node-derivatives* (segment) dvdt *plot-node-voltage-derivatives-p *plot-node-derivatives-structures* "mV/ms")
    (*plot-path-nodes* (soma segment) voltage *plot-path-node-voltages-p *plot-path-nodes-structures* "mV")
    ;; (*plot-or-analysis-nodes* (segment soma) voltage *plot-node-voltages-p *plot-or-analysis-nodes-structures* "mV")
    (*plot-axons* (axon) voltage *plot-axon-voltages-p *plot-axons-structures* "mV")
    (*plot-axon-events* (axon) event *plot-axon-events-p *plot-axon-events-structures* "Event")
    (*plot-synapse-events* (SYNAPSE) event *plot-synapse-events-p *plot-synapse-events-structures* "Event")
    (*plot-soma-nodes* (soma) voltage *plot-soma-voltage-p *plot-soma-nodes-structures* "mV")
    (*plot-soma-node-derivatives* (soma) dvdt *plot-soma-voltage-derivative-p *plot-soma-node-derivatives-structures* "mV/ms")
    (*plot-soma-dendrite-currents* (soma) dendrite-current *plot-soma-dendrite-currents-p *plot-soma-dendrite-currents-structures* "nA")
    (*plot-channel-currents* (channel) current *plot-channel-currents-p *plot-channel-currents-structures* "nA")
    (*plot-channel-conductances* (channel) conductance *plot-channel-conductances-p *plot-channel-conductances-structures* "uS")
    (*plot-channel-reversal-potentials* (channel) reversal-potential *plot-channel-reversal-potentials-p *plot-channel-reversal-potentials-structures* "mV")
    (*plot-synapse-currents* (synapse) current *plot-synapse-currents-p *plot-synapse-currents-structures* "nA")
    (*plot-synapse-conductances* (synapse) conductance *plot-synapse-conductances-p *plot-synapse-conductances-structures* "uS")
    (*plot-synapse-reversal-potentials* (synapse) reversal-potential *plot-synapse-reversal-potentials-p *plot-synapse-reversal-potentials-structures* "mV")
    (*plot-conc-1-ints* (conc-int) 1 *plot-shell-1-concentrations-p *plot-conc-1-ints-structures* "mM")
    (*plot-conc-2-ints* (conc-int) 2 *plot-shell-2-concentrations-p *plot-conc-2-ints-structures* "mM")
    (*plot-conc-3-ints* (conc-int) 3 *plot-shell-3-concentrations-p *plot-conc-3-ints-structures* "mM")
    (*plot-conc-ints* (conc-int) total *plot-concentrations-p *plot-conc-ints-structures* "mM")
    (*plot-buffers* (buffer) concentration *plot-buffer-concentrations-p *plot-buffers-structures* "mM")
    (*plot-conc-particles* (conc-particle) state *plot-conc-particles-p *plot-conc-particles-structures* "State")
    (*plot-particles* (particle) state *plot-particles-p *plot-particles-structures* "State")
    (*plot-markov-particles* (particle) markov-state *plot-markov-particles-p *plot-markov-particles-structures* "State")
    (*plot-pump-currents* (pump) current *plot-pump-currents-p *plot-pumps-structures* "mM/ms")
    (*plot-isource-currents* (isource) current *plot-isource-currents-p *plot-isource-currents-structures* "nA")
    (*plot-vsource-currents* (vsource) current *plot-vsource-currents-p *plot-vsource-currents-structures* "nA")
    (*plot-vsource-voltages* (vsource) voltage *plot-vsource-voltages-p *plot-vsource-voltages-structures* "mV")
    (*plot-field-potentials* (extracellular-electrode) field-potential *plot-field-potentials-p *plot-extracellular-electrodes-structures* "mV")))



(loop for plotted-class-list in *plot-lists-info*
      do (eval (cons 'defvars-w-value
		     (list (list (car plotted-class-list) nil)
			   (list (nth 3 plotted-class-list) nil)
			   (list (nth 4 plotted-class-list) nil)))))

(defvar *all-save-voltage-nodes* nil)
(defvar *all-save-dvdt-nodes* nil)


;; ****************************************
;;
;; Simulation-related variables.
;;
;; ****************************************


(defvar *recheck-circuit-elements-parameters* nil) ;  Used in the menu loop


(defvars-w-value
    (*enable-segment-membrane-parameter-update* t)
    (*enable-soma-membrane-parameter-update* t)
  (*enable-conc-integrator-membrane-parameter-update* t)
  (*enable-buffer-membrane-parameter-update* t)
  (*enable-pump-membrane-parameter-update* t)
  (*enable-channel-type-membrane-parameter-update* t)
  (*enable-channel-membrane-parameter-update* t)
  (*enable-axon-membrane-parameter-update* t)
  (*enable-synapse-membrane-parameter-update* t))


(defvar *interpolate-particle-arrays* nil)

;; (defvar *eval-all-elements-with-hash t)
;; (defvar  *use-new-part-formula t)
;; (defvar *evaluate-particles-with-tau-inf t)


;; ****************************************
;;
;; Current and Voltage Sources
;;
;; ****************************************


;; (defvar *source-pulse-lists* '())	; For both voltage and current sources.

(defvar *source-resistance-lists* '())

(defvar *default-waveform-step* 1.0 "Default value for waveform time steps, in milliseconds.")


(defvar *pwl-isource-di-dt* 100.0 "The transition slope for pwl isources \(nA/msec\).")
(defvar *isource-electrode-resistance* 0.0 "Mohms")

(defvar *vclamp-default-magnitude* -70.0 "mV")
(defvar *vclamp-default-magnitude-double -70.0d0)

(defvar *steady-state-linear-vclamp-enable* t "When true, cells with single eligible voltage sources
will be initialized with a linear single-step steady-state method [STEADY-STATE-LINEAR-VOLTAGE-CLAMP].")
 
(defvar *pwl-vsource-dv-dt* 1000.0 "The transition slope for pwl vsources (mV/msec).")
 
(defvar *vsource-resistance* 0.001 "Series resistance, in Mohms, for the non-ideal vclamp. Must be >0.")
(defvar *minimum-vsource-resistance* 0.0001) ; Mohms

(defvar  *enable-initial-vsource-slope* t) ; 

(defvar *modify-stimulus* Nil)		; For sources




;; ****************************************
;;
;; Time Step Related Variables, Breakpoints, etc.
;;
;; ****************************************

(defvar *num-nodes-check-break-times* 10)

;;; The business of dealing with time in units of *mrt* is to avoid floating point time
;;; representations during time steps - this may not really gain much now.

(defconstant most-positive-fixnum-float-version (float most-positive-fixnum))

;; The number of bits in a integer used for internal times.
;; Inherited from webber's surf - works ok.
;; This is not guarranteed. The +cmu figure of 28 was estimated by finding the type-of 2^N, and N=28
;; was the largest value that gave a FIXNUM.
(defconstant time-word-length #-cmu 32 #+cmu 28)

;; The maximum integer used for internal times. The word length is subtracted by 2, one to allow for
;; 2's complement math, and one for safety if a simulation runs over the stop time. I don't know if
;; the second one is necessary.
;; Inherited from webber's surf - works ok.

; (defconstant max-integer-time (expt 2 (- time-word-length 2)))

;; Change LBG 8.26.99
;; Quantization error analysis suggests that the correction of "2" above to time-word-length is
;; insufficient, since the conversion of  integer time values to real time values via
;; *mrt* can give the same float value for successive integer values. Empirical testing of this
;; conversion, which is more prone to error at the stop time, show that a correction factor of 5
;; avoids this problem.

(defconstant max-integer-time (expt 2 (- time-word-length 5)))

(defvar *mrt* 1.0) ;; The minimum resolvable time, to convert floating times into integers.
(defvar *user-start-time* 0.0)		;  The start time of the simulation, in milliseconds.
(defvar *start-time* 0) ; The start time in units of *mrt*.
(defvar *user-stop-time* 1.0 "The time to end the simulation, in milliseconds.")
(defvar *last-user-stop-time* 1.0)	; The stop time of the last simulation.
(defvar *int-user-stop-time* 0) ; The integer version of the  time to end the simulation, in milliseconds.
(defvar *stop-time* 0) ; The stop time in units of *mrt*.

(defvar *first-time-step* nil)		; T just for the first time step of the simulation. Useful
					; in various places, such as evaluation of autonomous events.
(defvar *display-time-increment 1.0)


;; just so something is on the list after stop-time ???
(defvar *extra-time-after-stop-time* 1.0)

(defvar *user-max-step* 5.0 "The maximum time step allowed, in milliseconds. When 0, then *MAX-STEP* is
bound by the simulation duration.")
(defvar *max-step* 0) ; The maximum time step in units of *mrt*.

(defvar *user-min-step* 0.0 "The minimum time step allowed, in milliseconds. When 0, then *MIN-STEP* is *MIN-STEP-MRTS.")
(defvar *user-min-step-double* 0.0d0)
(defvar *MIN-STEP-MRTS 4)		; When = 2, the smallest half step is 1 mrt, which will choke since the single
					; float coercion of the sum of the double float equivalent and values whose
					; magnitude is greater than 4 give the original value. This shows up for example
					; as
#|
                         2]]]  (*t-prime[n-prime]*)
                         5.000001
                         2]]]  (*t-prime[n-prime-1]*)
                         5.000001
|#


(defvar *min-step* 0) ; The minimum time step in units of *mrt*.

;; These are fixnums, ie in units of *mrt*
(defvar *sim-time-n+1* 0 "The time for the step currently being computed in units of *mrt*. t(n+1)")
(defvar *sim-time-n*   0 "The time for the step already computed in units of *mrt*. t(n)")
(defvar *sim-time-n-1* 0 "The time for one step back in units of *mrt*. t(n-1)")
(defvar *sim-time-n-2* 0 "The time for two steps back in units of *mrt*. t(n-2)")
(defvar *time-step* 1 "The current time step in units of *mrt*.")
(defvar *last-time-step* 1 "The last time step in units of *mrt*.")

;;; The *USER-MAX-STEP* constraint may be overruled by the evaluation of some element types, in
;;; particular those that are driven by an a-priori waveform. In these cases, the global variable
;;; *ELEMENT-TIME-STEP-MAXIMUM* is set to a non-NIL step value (in units of *mrt*) that matches the
;;; (smallest) time interval of the waveform(s).

(defvar *element-time-step-maximum* nil)

;; When true, at each time step the circuit inputs (e.g. sources, driven synapses) are evaluated at
;; the midpoint of the step - otherwise, the inputs are evaluated for the end of the step (the
;; prediction time).
(defvar *evaluate-inputs-at-midpoint* nil)


;; To avoid consing, the single and double float globals which track the time steps are now stored
;; in arrays:

(defvar *error-double-float-variables* (make-array '(10) :element-type 'double-float))

;; For all gating particles.
(defmacro *maximum-particle-error-numerator* ()
  `(aref (the (simple-array double-float *) *error-double-float-variables*) 0))

(defmacro *markov-particle-state-delta-s-max-time-step* ()
  `(aref (the (simple-array double-float *) *error-double-float-variables*) 2))

(defmacro *maximum-conc-int-error-numerator* ()
  `(aref (the (simple-array double-float *) *error-double-float-variables*) 3))



(defvar *include-vsource-nodes-in-node-error-est* nil)


(defvar *time-single-float-variables* (make-array '(10) :element-type 'single-float))
(defvar *time-double-float-variables* (make-array '(20) :element-type 'double-float))

;; In general, "prime" associated with a time variable name refers to the staggered time grid used
;; for particle evaluations. "prime" associated with the time index, e.g. "n-prime", refers to the
;; index incremented by 1/2.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros (and associated variables) which are referenced in
;; *TIME-SINGLE-FLOAT-VARIABLES*.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Previously called (*real-time*).
(defmacro *t[n+1]* ()
  "Time during simulation (msec), corresponding to the prediction time t_(n+1)."
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 0))
(defvar *real-time* 0.0 "Time during simulation (msec), corresponding to the prediction time t_(n+1).")

(defmacro *fractional-time* ()
  "The fractional part of (*t[n+1]*)."
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 1))

(defmacro *input-time* ()
  "The time reference for inputs (msec)"
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 2))

;; Previously called (*last-real-time*).
(defmacro *t[n]* ()
  "Time during simulation (msec), corresponding to the current time t_n."
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 3))
(defvar *last-real-time* 0.0 "Time during simulation corresponding to the current time (msec)")

(defmacro *t-prime[n+1]* ()
  "Time during simulation (msec) of the staggered grid, corresponding to the prediction time t-prime_(n+1)."
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 4))

(defmacro *t-prime[n]* ()
  "Time during simulation (msec) of the staggered grid, corresponding to the current time t-prime_n."
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 5))

(defmacro *t-prime[n-prime]* ()
  "Time during simulation (msec) of the staggered grid, halfway between (*t-prime[n]*) and (*t-prime[n+1]*)."
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 6))

(defmacro *t-prime[n-prime-1]* ()
  "Time during simulation (msec) of the staggered grid, halfway between (*t-prime[n-1]*) and (*t-prime[n]*)."
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 7))

(defmacro *t[n]-t-prime[n-prime]* ()
  "Difference between (*t[n]*) and (*t-prime[n-prime]*) in msec."
  `(aref (the (simple-array single-float *) *time-single-float-variables*) 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Macros (and associated variables) which are referenced in
;; *TIME-DOUBLE-FLOAT-VARIABLES*.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defmacro *delta-t[n]* ()
  "The current time step [t_(n+1) - t_n], in msec."
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 0))

(defmacro *delta-t[n]-squared* ()
  "msec^2"
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 1))

(defmacro *delta-t[n-1]* ()
  "The last time step [t_n - t_(n-1)], in msec."
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 2))

;; Previously called delta-back
(defmacro *half-delta-t[n-1]* ()
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 3))

(defmacro *delta-t-prime[n]* ()
  "The current time step for the staggered grid, [t-prime_(n+1) - t-prime_n], in msec."
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 4))

(defmacro *delta-t-prime[n]-squared* ()
  "msec^2"
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 5))

(defmacro *half-delta-t-prime[n]* ()
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 6))

(defmacro *delta-t-prime[n-1]* ()
  "The last time step for the staggered grid, [t-prime_(n) - t-prime_(n-1)], in msec."
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 7))

(defmacro *2/delta-t[n]* ()
  "The constant to multiply capacitances by for the trapezoidal rule."
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 8))

(defmacro *markov-time-step* ()
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 9))
  
(defmacro *markov-time-step/2* ()
  `(aref (the (simple-array double-float *) *time-double-float-variables*) 10))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *integer-time* 0 "The integer part of real-time")

(defvar *use-fixed-step* nil "Unless *USE-TIME-LIST* is T and valid, when T, use a fixed step integration. Otherwise use variable time step.xs")
(defvar *use-time-list* nil "When true, take integration time points from *LAST-SIM-REVERSE-TIME-STEP-LIST*. If there was no previous simulation, this
is ignored." )
(defvar *user-step* 0.010 "Fixed time step [ms] for fixed step integration.")
(defvar *user-time-step* 1) ; in units of *mrt*


;; These are lists of time points associated with the plot-data for the simulation. *SIM-REVERSE-PLOT-TIME-LIST* is
;; in reverse order, as are all the plot data lists associated with the circuit structures.

(defvar *sim-reverse-plot-time-list* '() "The list of plotted time points for the simulation in the
reverse order. Updated during the simulation.")  
(defvar *sim-plot-time-list* '() "After a simulation is complete, the list of plotted time points in the correct order.")


;; *SIM-REVERSE-TIME-STEP-LIST* *SIM-REVERSE-TIME-LIST* *LAST-SIM-REVERSE-TIME-LIST* are in reverse time order.

(defvar *sim-reverse-time-step-list* '() "All the time steps in the simulation in reverse time
order. Updated during the simulation.")
(defvar *last-sim-reverse-time-step-list* '() "All the time steps in the last simulation (if saved) in reverse time order.")
(defvar *sim-reverse-time-list* '() "All the time points in the simulation in reverse time order.
Updated during the simulation.")

(defvar *last-sim-reverse-time-list* '() "All the time points in the last simulation (if saved) in reverse time order.")

(defvar *auto-refresh-last-sim-reverse-time-list* nil "Set *LAST-SIM-REVERSE-TIME-LIST* to the last simulation's time list.")

(defvar *fixed-time-steps* '())		; A list in correct time order of fixed time steps (units of
					; *mrt*) that are used to set each time step when
					; *USE-TIME-LIST* is T.

(defvar *use-node-voltage-initializations* nil "Use *NODE-VOLTAGE-INITIALIZATIONS* for setting node voltages.")

(defvar *node-voltage-initializations* '() "A list of dotted pairs, where for each pair the CAR is a
node and the CDR is that node's initial value in mV.") 

(defvar *use-conc-int-initializations* nil "Use *CONC-INT-INITIALIZATIONS* to set initial concentrations.")
(defvar *use-buffer-initializations* nil "Use *BUFFER-INITIALIZATIONS* to set initial buffer states.")
(defvar *use-pump-initializations* nil "Use *PUMP-INITIALIZATIONS* to set initial pump states.")

(defvar *conc-int-initializations* '() "A list of dotted pairs, where for each pair the CAR is a
concentration integrator and the CDR is that integrator's initial value in mM.")

(defvar *buffer-initializations* '() "A list of dotted pairs, where for each pair the CAR is a
buffer and the CDR is that buffer's initial value in mM.") 

(defvar *pump-initializations* '() "A list of dotted pairs, where for each pair the CAR is a
pumpr and the CDR is that pump's initial value in mM.") 

(defvar *breakpoint-list* '()) ; A list of the break points of pwl sources and other inputs.

(defvar *last-breakpoint-list* '()) ; A list of the break points used in the last simulation.

(defvar *user-breakpoint-list* '() "A list of break points that is specified by the user, and then added to the points automatically collected into *BREAKPOINT-LIST*.")
  
(defvar *mrt-breakpoint-list* '())	; Cleaned up break points, in units of *mrt* - this is what is
					; used by PICK-TIME-STEP. "Cleaned up" means that these are sorted, with no duplicates, and bounded by 0 and
					; the sum of *extra-time-after-stop-time* and *user-stop-time*. 

(defvar *enable-dynamic-breakpoint-generation* t "Allow breakpoints to be dynamically generated
during a simulation by event-based elements such as axons and voltage-dependent synapses.")

;; Internal enable for breakpoints to be dynamically generated during a simulation by event-based
;; elements such as axons and voltage-dependent synapses. 
(defvar *dynamic-breakpoint-generation* t)

(defvar *total-num-iterations* 0 "The total number of iterations over all time.")
(defvar *total-num-time-points* 0 "The total number of time points taken for the simulation.")
(defvar *use-max-iterations* nil "Use *MAX-ITERATIONS* to constrain time steps.")
(defvar *max-iterations* 1 "Useful for debugging.")



;; Related specifically to synapse evaluation.

(defvar *enable-auto-synapse-evaluation-method* t)
;; CAR of (element-parameter type 'evaluation-method-parameters) can :be
;;
;;   :convolution :2nd-order-integration
;;
;;
(defvar *synapse-evaluation-step* 0.2)	; ms
(defvar *eval-all-synapses-this-step* t)
(defvar *eval-all-synapses-this-iteration* nil)
(defvar *EVAL-ALL-SYNAPSES-EVERY-STEP* t)
(defvar *synapse-evaluation-times* '())
(defvar *use-constant-element-matrix-contribution* nil)

;; LBG 7/6/99
;; For VOLTAGE synapses whose conductance waveform is a single exponentional of time
;; constant TAU, *and* when a fixed time step dT is used, the synapse evaluations are simplified by
;; the fact that at each time step the new conductance value is given by the previous step value
;; multiplied by a decay factor, exp -dT/TAU. This decay factor is precalculated for a given type
;; and stored as an element parameter 'CONDUCTANCE-DECAY-FACTOR.
(defvar *OPTIMIZE-EXPO-CONDUCTANCE-WAVEFORMS* nil)




;; THESE TWO FLAGS ARE NOT USED NOW.
;; (defvar *pseudo-transient-requested* nil "If t, do a pseudo transient DC solution.")
;; (defvar *dc-solution-computed* nil "Automatically set to t after a DC solution")


;; ****************************************
;;
;; Error Estimation Variables
;;
;; ****************************************

;; When LTE estimate is too large despite using the minimum step, keep going anyway.
(defvar *punt-lte-when-min-step-reached* t)

;; If lte estimate is punted sometime during a simulation, then this will flag it.
(defvar *lte-was-punted* nil)


(defvar *absolute-voltage-error* 0.05 "A reasonable range: 0.01 - 0.1 \(mV\).")
(defvar *relative-voltage-lte* 0.0)	; Ratio of maximum node voltage error / *ABSOLUTE-VOLTAGE-ERROR*.

(defvar *absolute-particle-error* 0.001 "In terms of particle state [0-1].")
(defvar *relative-particle-lte* 0.0)	; Ratio of maximum particle state error / *ABSOLUTE-PARTICLE-ERROR*.

(defvar *absolute-conc-int-error* 0.001 "mM")
(defvar *relative-conc-int-lte* 0.0)	; Ratio of maximum conc-int error / *ABSOLUTE-CONC-INT-ERROR*.

(defvar *debug-update-time-step-vars* nil)
(defvar *debug-voltage-error-step-change* nil)
(defvar *debug-particle-error* nil)
(defvar *debug-particle-error-step-change* nil)
(defvar *debug-conc-int-error* nil)
(defvar *debug-conc-int-error-step-change* nil)
(defvar *particle-w-max-error*)
(defvar *conc-int-w-max-error*)
(defvar *count-error-step-change* t)

(defvar *FULL-ERROR-STEP-CHANGE* nil "Full documentation of error step changes.")

;; Lists of the times in which the error was determined from the appropriate criteria.
(defvar *particle-error-step-changes* '())
(defvar *particle-ERROR-STEP-less-than-min-step* '())

(defvar *conc-int-error-step-changes* '())
(defvar *VOLTAGE-ERROR-STEP-CHANGES* '())


(defvar *relative-particle-error* 0.01)
(defvar *twice-relative-particle-error* 0.02)

(defvar *particles-are-working* nil)

(defvar *conc-particles-are-working* nil)

(defvar *estimate-particle-error-with-full-time-step* t)

(defvar *relative-conc-particle-error* 0.001)
(defvar *twice-relative-conc-particle-error* 0.002)

(defvar *particle-error-max-time-step* 0) ; for both conc and v-dep particles

(defvar *consider-particle-error* t "Consider LTE for particle states.")

(defvar *calculate-particle-error* t "Actually calculate LTE for particle states.")

(defvar *consider-conc-particle-error* t "Consider LTE for particle states.")


(defvar *conc-int-error-max-time-step* 0) 

;(defvar *conc-int-error-factor* 1000.0 "Concentration integrator values may use a different error
;criteria than voltage nodes. This coefficient is not finalized.")

(defvar *relative-conc-int-error* 0.01)
(defvar *twice-relative-conc-int-error* 0.02)

(defvar *consider-conc-int-error* nil "Consider LTE for concentration ints as well, using the factor
above for concentrations.") 
(defvar *calculate-conc-int-error* nil "Actually calculate it.")


(defvar *eval-conc-ints-in-inner-loop* nil) ; In order to allow concentration ints to contribute to
					 ; LTE, we must evaluate them in the DO-TIME-CONTROL inner loop.

(defvar *pick-time-step-fudge* 0.8 "Coefficient for choosing new time step based on lte
estimate. Less than one to speed up time step reduction when lte-ratio = 1")




;; NOT USED NOW
;; LTE estimate is based on voltage vector magnitude, as opposed to the node with the largest
;; voltage error at a given time step. 
;; (defvar *estimate-vector-error* nil)


;; ****************************************
;;
;; Soma Stuff
;;
;; ****************************************

(defvar *default-soma-diameter* 25.0)	;microns

(defvar *soma-shunt* 1e30 "Default value [ohms] for non-specific soma shunt.")		;ohms
(defvar *soma-shunt 1e30)		;ohms, for backward compatibility


;; ****************************************
;;
;; Concentration Integrator Variables
;;
;; ****************************************

;; Note that other relevant variables are defined elsewhere.

(defvar *implicit-conc-int-integration* t "When T, use implicit integration for :MULTI-SHELL integrators.")

(defvar *default-conc-int-type-diffusion-distance* 1.0e1 "microns")

;; ****************************************
;;
;; Electrode shunt resistance (Mohm)
;;
;; ****************************************

(defvar *r-electrode 10000000.0)
(defvar *include-shunt nil)



;; ****************************************
;;
;; Synapse and Axon Global Variables
;;
;; ****************************************

(defvar *include-events-in-element-documentation-code* nil
  "Enables storing of event times in element documentation code, for example for autonymous synapses.")

(defvar *enable-event-generators* t
  "Event generators reduce evaluations for axons and synapses [VOLTAGE, LIGHT and LIGHT-AUTO] whose
control parameters are identical for a given simulation.")

(defvar *SETUP-EVENT-GENERATORS-AND-FOLLOWERS* t
  "Enables the automatic assignment of event element sets at the beginning of every simulation, as long as
*USER-SPECIFIED-EVENT-ELEMENT-SETS* is NIL. This variable may be set to NIL after a simulation for more efficiency
in subsequent simulations, or may always be NIL as long as the function SETUP-ALL-EVENT-GENERATORS-AND-FOLLOWERS is
explicitly called when the circuit is setup or changed.")

(defvar *USER-SPECIFIED-EVENT-ELEMENT-SETS* nil
  "If this flag is T, then the user has the responsibility to setup event generators and followers, e.g. with calls to
USER-SETUP-EVENT-GENERATORS-AND-FOLLOWERS or SETUP-ALL-EVENT-GENERATORS-AND-FOLLOWERS before a simulation.")

(defvar *maximum-synapse-printed-events* 5
  "Maximum number of event times or delays that will be explicitly printed when printing out a synapse's information.")



;; ****************************************
;;
;; Synapse and Light Stimuli Global Variables
;;
;; ****************************************

;; See also SYNAPSE.LISP



(defvar *enable-synapses* t "When nil, all synapses are blocked in some circuit functions.")

(defvar *setup-tonic-synapses* t)
(defvar *setup-voltage-synapses* t)
(defvar *setup-light-synapses* t)
(defvar *setup-auto-synapses* t)


(defvar *adjust-breakpoints-for-auto-synapses* t "Before each simulation, add autonomous synapse
event times to the *BREAKPOINT-LIST* to ensure catching the events.")

;; When T, all auto synapses of a given type on the same cell node are evaluated as one. In this
;; case, plotting the synaptic conductance or current will be incorrect.
(defvar *evaluate-lumped-auto-synapses* nil)

(defvar *reuse-synapse-waveforms* nil)

;; ************* Light Stuff *************

(defvar *convert-light-response-to-events-for-each-synapse* nil
  "When event generators are used for light related synapses, this flag causes the light-response ->
event conversion to be done individually for each synapse.")


(defvar *enable-light-auto-event-update* t "When T, renew :EVENT-TIMES slot for light-auto synapses.")

(defvar *constant-light-input-from-negative-infinity* t "Whatever the light input is calculated to
be at time 0, assume that this is the initial conditions \(otherwise, intial light conditions are
taken as zero state\)." ) 


;; Convolve all light driven synapses individually - otherwise if light input is same for two
;; synapses, then each can reference the same wave.
(defvar *compute-all-light-inputs* nil)

;;;  *FAST-RF-BAR* This may be used when the stimulus is an infinite slit along the y-axis, and the bar sweeps
;;;  across the entire cell. When T, the spatial RF is integrated along the x-axis only.
(defvar *fast-rf-bar* nil)

;;;  *FAST-FULL-FIELD-SPOT This may be used when the stimulus is a full field spot, and therefore all the synapse
;;;  waveforms for each synapse type are identical.
(defvar *fast-full-field-spot* nil)

(defvar *enable-light* t "Let there be light.")

;; Variables to modify the relationship between the input light pattern and light-activated synapses.
(defvar *light-input-offset-distance* 0.0 "um")
(defvar *light-input-offset-angle* 0.0 "radians")
(defvar *light-input-delay* 0.0 "Light input delay between light event at offset location and
synaptic response, in milliseconds.") 

(defvar *light-origin-array* (make-array 1 :element-type 'single-float))


;; Light stimulus related parameters

(defvar *light-speed* 0.0 "Microns per millisecond")
(defvar *bar-width* 0.0 "Microns")
(defvar *bar-length* 0.0 "Microns")
;; * (* 90 2.0 (COERCE user::PI 'SINGLE-FLOAT) (/ 1.0 360))
;; 1.5707964
(defvar *light-theta* 1.5707964 "Radians")

(defvar *light-direction* T "T / nil => movement is in the direction of / opposite to *light-theta*.")
;(defvar *motion-start-time 0.0)			;Time to start bar moving, milliseconds
(defvar *light-start-position-x* 0.0 "Point of center of stimulus at *motion-start-time* in microns")
(defvar *light-start-position-y* 0.0 "Point of center of stimulus at *motion-start-time* in microns")
;(defvar *motion-stop-time* 100000.0)		;Time to stop bar moving, milliseconds
(defvar *grating-temporal-period* 1000000.0 "Milliseconds")
(defvar *grating-spatial-period* 1000000.0 "Microns")
(defvar *use-aperture* nil "Consider aperture.")
(defvar *aperture-radius* 300.0 "Microns")
(defvar *aperture-center-x* 0.0 "Microns")
(defvar *aperture-center-y* 0.0 "Microns")


#|
;; problems with declaring single-float to *LIGHT-THETA* when compiled as an arg to cos or sin with
;; ultra 18a lisp
(proclaim '(single-float *light-theta* *bar-width* *bar-length* *light-speed* *light-start-position-x*
	    *light-start-position-y*))
|#
;; Apparent motion stimulus.
(defvar *bar-a-width* 0.0 "microns" )
(defvar *bar-a-length* 0.0 "microns" )
(defvar *bar-a-intensity* 1.0)
(defvar *bar-a-start-time* 0.0 "milliseconds")
(defvar *bar-a-stop-time* 0.0 "milliseconds")
(defvar *bar-a-position-x* 0.0 "microns")
(defvar *bar-a-position-y* 0.0 "microns")

(defvar *bar-b-width* 0.0 "microns")
(defvar *bar-b-length* 0.0 "microns")
(defvar *bar-b-intensity* 1.0)
(defvar *bar-b-start-time* 0.0 "milliseconds")
(defvar *bar-b-stop-time* 0.0 "milliseconds")
(defvar *bar-b-position-x* 0.0 "microns")
(defvar *bar-b-position-y* 0.0 "microns")

(defvar *syn-rf-normalized-amp* nil)	;If T then synaptic spatial receptive field gaussian amplitude is
					;normalized, else area (volume) is normalized.

(defvar *light-input-waveform* (make-array 1 :element-type 'single-float))
(defvar *first-non-zero-element-light-input-waveform* 0)
(defvar *last-non-zero-element-light-input-waveform* 0)

;;(proclaim '((type simple-array single-float) *light-input-waveform*))
	  
;;; Format for gaussian spatial receptive field parameter list.  (list gaussian
;;; light-input-offset-distance light-input-offset-angle sigma-x sigma-y normalized-amp)

(defvars-w-value (*synapse-g-leak-ratio* 1.0)  (*light-stimulus-start-time* 0.0)
		 (*light-stimulus-stop-time* 100000.0)
		 (*light-stimulus-strength* 1.0) 
		 (*light-background* 0.0))

(defvar *light-stimulus-types* '(:MOVING-SPOT
				 :ANNULUS
				 :ON-SPOT
				 :OFF-SPOT
				 :ON-MOVING-BAR
				 :OFF-MOVING-BAR 
				 :APPARENT-MOTION :ON-BAR :OFF-BAR 
				 :MOVING-BAR)
  ":MOVING-BAR is equivalent to :ON-MOVING-BAR")

;; Future types:
;; :moving-bar-grating :moving-sine-grating :reversing-bar


(defvar *light-stimulus* nil "Can take a value out of *LIGHT-STIMULUS-TYPES*.")

;(defvar *light-stimulus-strength* 1.0)	; for backward compatibility - use *light-stimulus-strength*
;					; instead for new code.

(defvar *spot-outside-diameter* 0.0)
(defvar *spot-inside-diameter* 0.0)

#|
(defvar *synapse-names-to-do-first* nil)	; This is useful for *FAST-RF-BAR simulations in order to first
					; compute synapses with the longest responses.
|#

(defvar *maximum-synaptic-jitter* 20.0)
(defvar *jitter-light-synapse* nil)


(defvar *gaussian-rf-width-in-sigmas* 4)


(defvar *light-stimulus-plane* :XY ":XY for retina, :XZ for radial mount cortical cells.")


;; useful ??
(defvar *excitatory-facilitation-impulse-array)

;; ****************************************
;;
;; Data Analysis
;;
;; ****************************************



(defvar *print-linear-analysis* nil)
(defvar *print-nonlinear-analysis* nil)
(defvar *print-analysis* t)
(defvar *ANALYSIS-NODES* nil)
(defvar *ANALYSIS-NODES-structures* nil)

(defvar *print-synapse-event-times* nil)
(defvar *print-synapse-total-events* t)
(defvar *count-active-and-triggered-synapses* t "When non-NIL, the function COUNT-ACTIVE-SYNAPSES,
which normally prints out info at the end of each simulation, also prints the number of synapses
actually fired.")

(defvar *print-axon-spike-times* Nil)

(defvar *average-integrals* t)
(defvar  *x-integrate-min* nil)
(defvar *x-integrate-max* nil)


;; ****************************************
;;
;; Random Numbers
;;
;; ****************************************

(defvar *always-intialize-random-gen nil "Forces a call to GET-REFERENCE-RANDOM-STATE at various places.")

;; ****************************************
;;
;; Type Proclamations
;;
;; ****************************************


(proclaim '(double-float
	    *USER-MIN-STEP-DOUBLE*
	    *REAL-TIME-DOUBLE
	    *maximum-particle-error-numerator*
	    *vclamp-default-magnitude-double))


(proclaim '(single-float
	    MOST-POSITIVE-FIXNUM-FLOAT-VERSION
	    ; *light-stimulus-start-time*
	    *vclamp-default-magnitude* *display-time-increment 
	    *Temperature* *temp-celcius* 
	    *mrt* *user-start-time* 
            ZERO 
            *rs-mem
            *rd-mem *rd-int *capd-mem *caps-mem
            *r-electrode *synaptic-time-resolution

	    *absolute-voltage-error*
	    *relative-voltage-lte*
	    *absolute-particle-error*
	    *relative-particle-lte*
	    *absolute-conc-int-error*
	    *relative-conc-int-lte*

	    *pick-time-step-fudge*
	    *relative-particle-error* *twice-relative-particle-error*
	    *relative-conc-int-error* *twice-relative-conc-int-error*
	    ))


(proclaim '(type fixnum *min-step* *max-step* *time-step* *last-time-step* *integer-time*
	    *particle-error-max-time-step*
	    *conc-int-error-max-time-step*
	    *int-user-stop-time* *sim-time-n+1* *sim-time-n* *sim-time-n-1* *sim-time-n-2*
	    *vsource-array-length* *isource-array-length*)) 

(proclaim '(type (unsigned-byte 32)
	    *num-nodes* *num-unknowns*
	    *start-time* *stop-time* 
	    *total-num-iterations* *total-num-time-points*  *fp-exponent-large*
	    *fp-mantissa-large* *fp-exponent-small* *fp-mantissa-small* ))

(proclaim '(type (simple-array single-float 1) *diag* *lower-diag* *upper-diag* *v-at-half-step* *rhs*))
(proclaim '(type (simple-array double-float 1)
	    *diag-double* *lower-diag-double* *upper-diag-double* *v-at-half-step-double* *rhs-double*))
(proclaim '(type list *breakpoint-list*))








