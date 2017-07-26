;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")



;; *** DOCUMENTATION AND EXAMPLE FILE FOR THE SURF-HIPPO NEURON SIMULATOR ***

;; !! The SURF-HIPPO system and documentation is under development. Parameters and
;; functions are subject to change. !!

;; *** INTRODUCTION ***

;; The SURF-HIPPO Neuron Simulator uses various methods for the simulation of electrical
;; circuits whose elements and topologies are intended to model the electrical and (some)
;; chemical processes in neurons. The parallel version of SURF-HIPPO is an extension of
;; the SURF Circuit Simulator (intended for VLSI applications) written by Don Webber of
;; Thinking Machines Corporation and the University of California at Berkeley Department
;; of Electrical Engineering and Computer Science.

;; SURF-HIPPO is written in Common LISP and *LISP. The *LISP version is intended to run
;; on a Connection Machine, a massively parallel computer manufactured by Thinking
;; Machines Corporation. However, this version is entirely preliminary.

;; Depending on the *use-Hines* flag, the simulator uses either relaxation-based
;; (*use-Hines* = nil), or direct methods (*use-Hines* = T) to integrate the circuit. At
;; present, all simulations are done with the direct methods. *LISP simulations use the
;; relaxation-based methods exclusively.


;; *** SYSTEM VERSIONS ***

;; At present SURF-HIPPO may be compiled for running in either a generic Common LISP
;; environment, under CMU Common Lisp running the Garnet user interface system, or in the
;; Symbolics Genera Windows environment. The latter version includes a menu-driven
;; interface, visualization of (three-dimensional) neuron geometries, and automatic
;; plotting of simulation results. The code written for Garnet will eventually support
;; all of these features. The system definition for the Symbolics environment is found in
;; the file defsys.lisp, and the one for the Common LISP environment is found in the file
;; cldefsys.lisp.

;; *** FIRST THINGS FIRST ***

;; As mentioned below, changes may be necessary in order to get SURF-HIPPO running on any given system. It is
;; recommended that any such changes be kept in a separate file (if possible) since updates to the entire system
;; will be supplied in the near future.

;; *** RUNNING SURF-HIPPO ***

;;     Meta-X run-ilisp (dialect cmulisp)

;; NOTE: SURF-HIPPO should run successfully under other Common LISP environments without 
;; significant modification.

;; The SURF-HIPPO cldefsys.lisp file should then be loaded into the LISP environment
;; (without compiling first).

;; Move to the SURF package by entering:

;;     (setsurf)
	
;; SURF-HIPPO may then be run, for example, by entering the function:

;;     (surf 'sun-hippo)

;; I suggest that the first effort be in verifying that the system can compile, load, and
;; run in your environment correctly, and that some scheme worked out for displaying the
;; file output automatically or semi-automatically. Example output of the simulation
;; defined in "sun-hippo" with all the system parameters set to the values pre-defined in
;; the source code are in the files SUN-HIPPO.PLOT and SUN-HIPPO.INFO.

;; Note that the variable *circuit-name* is set to the name of function argument called
;; by "surf", and this name is used to create output filenames (see file PRINT.LISP).

;; *** SURF-HIPPO BASICS ***
	
;; The function "surf" is the executive simulator function (defined in the file
;; main.lisp), and "sun-hippo" is the name of the function that is specific to the
;; circuit (neuron) being simulated. With the menu-driven version of SURF-HIPPO, the user
;; can change a wide variety of simulation parameters for the circuit interactively.
;; Until a user interface is developed for other environments, however, all simulation
;; parameters will have to be set either 1) in the LISP environment by hand prior to
;; running the simulation, 2) in the circuit function definition (see definition of
;; "sun-hippo" below for example), or 3) by modifying and recompiling the function
;; "user-set-globals" (in file init.lisp), as appropriate.  Note that the function
;; "initialize-globals", called by "initialize" (both in file init.lisp) will set many
;; simulation parameters.

;; At present circuit functions specified for the argument to "surf" may not have any
;; arguments. For example, the function "sun-hippo" does two things -- it sets some
;; simulation parmeters, and then it calls the function "hippo" (in file
;; hippocampus/hippos.lisp), which actually describes the circuit (we will refer to this
;; as the circuit function from now on).  Functions which describe circuits may be
;; written in a variety of different ways, but for the present it is recommended that the
;; user copy the format used in "hippo".

;; We will now describe this format briefly. "hippo" begins by setting up some circuit
;; parameters. The function "create-cell-type" is then called which defines a class of
;; cells, here named "CA1". "create-cell" is called next to define an instance of the
;; cell type (note that this simulator is intended to also be used for multi-neuron
;; simulations, with potentially several cell-types and several cells of each type). The
;; cell geometry is then constructed, starting with "create-soma". The functions
;; "create-channels" (in this case the channels are going into the soma membrane) and
;; "create-source" complete the soma portion of the cell. A five segment short dendritic
;; cable is then constructed with repeated calls to "create-segment". Note that the
;; creation of cell elements which correspond to the geometry of the cell ("create-soma"
;; and "create-segment") require string name arguments. These functions return the
;; cell-element object. Functions which add membrane elements to soma or segments (e.g.
;; "create-channels", "create-source", "create-synapse") require the appropriate soma or
;; segment object as one of the arguments, with the name of the added element being built
;; up starting with the name of the soma or segment name.  Thus a current source that is
;; added to a segment whose name is "amacrine-12-1-3" could be created with:

;;    (create-source "Current clamp" (gethash "amacrine-12-1-3" segment-hash-table))

;; This current source would be automatically named "amacrine-12-1-3-istim". Note that
;; circuit objects are referenced in the appropriate hash tables, and that the objects
;; may be accessed by the "gethash" function (in the previous example a segment object is
;; returned from the call to "gethash").

;; NOTE: These conventions regarding cell element creation have not been completely
;; standardized in the SURF-HIPPO code, so that some exceptions may still exist.


(defun sun-hippo ()
  ;; First we shall set some simulation parameters.
  (setq
   ;; Duration is in milliseconds, and for now suppress any prompts.
   *use-Hines* t
   user-start-time 0.0 user-stop-time 25.0 *surf-interactive* nil

   ;;This is a list of circuit nodes to plot. The name of a node is the first argument to
   ;;"create-soma" or "create-segment". For example, the function "hippo" takes the
   ;;string argument 'cell-name. The call to "create-soma" has string argument for the
   ;;soma name in the form (format nil "~a-soma" cell-name).  Therefore, the soma node is
   ;;called "hippo-soma". Likewise, the cable segments are called "hippo-1", "hippo-2",
   ;;etc.. The lists *plot-nodes*, *plot-channel-currents*, etc. are used in the
   ;;functions called by "set-up-output-flags" (in file print.lisp).

   *plot-soma-nodes* '("hippo-soma")

   *plot-nodes* '("hippo-soma" "hippo-1" "hippo-2" "hippo-5")

   ;;This is a list of currents to plot. The call to "create-channels" creates channels
   ;;whose names are formed from both the name of the cell-element (in this case the cell
   ;;soma) that they are a part of and the channel type.

   *plot-channel-currents* '("hippo-soma-na1" "hippo-soma-na2" "hippo-soma-na3" "hippo-soma-dr"
			     "hippo-soma-a")

   *plot-isource-currents* '("hippo-soma-istim")
   ;;This flag enables the creation of channels.

   *active t

   ;;These flags enable specific channels.

   *include-na1 t *include-na2 t *include-na3 t *include-dr t *include-a t
   *include-ca nil *include-ahp nil *include-c nil

   ;;We want stimulus sources as a part of the circuit.

   ;;This is a list that specifies the waveform for the soma current source
   ;;"hippo-soma-istim" as a (depolarizing) 1.0 nA pulse from 5 milliseconds to 8.0
   ;;milliseconds (see file misc.lisp for description of this format). The name is formed
   ;;by "create-pwl-isource" (in file isource.lisp), which is part of "hippo".

   *include-sources t	
   *old-pulse-lists* (list (list (cons "hippo-soma-istim"  'ISOURCE)
				 (list 5.0 8.5 1.0)))
   )

  ;;Now call the actual circuit description function.

  (hippo "hippo"))


;; *** BUILDING CIRCUITS ***

;; A more sophisticated method of constructing (complicated) cell geometries is
;; demonstrated in the circuit function "real-hippo". This function uses the
;; "create-tree" function (in the file misc.lisp) which takes a list of tree node
;; parameters and generates the "create-segment" calls automatically.


;; *** SIMULATOR REPRESENTATION OF CIRCUIT STRUCTURE ***

;; Symbolically, circuits are composed of a set of object instances, of which there are
;; several different types: nodes, segments, somata, channels, synapses, isources,
;; particles, concentration integrators, etc. There are many different interrelationships
;; between objects in a hierarchical fashion, e.g. an instance of a channel object
;; references an instance of a node object (corresponding to the electrical circuit node
;; that it is attached to), an instance of a segment or soma object that it is a part of,
;; instances of particle and/or concentration particle objects that determine the
;; channel's conductance, and, indirectly, instances of particle nodes which correspond
;; to the particles' states.

;; The actual simulation of circuits is in terms of their nodes, which include the actual
;; electrical nodes (corresponding to somata and segments) as well as elements whose
;; states are both dependent on other nodes and in turn control either branch elements of
;; the circuit or other (non-voltage) nodes.  This latter class of elements include the
;; following:

;;   *Element*       *Meaning of Node "Voltage"*     *Depends On*         *Controls*
;;---------------------------------------------------------------------------------------
;; V-dep particles         Prob(Open) (0-1)         V(Segment/soma)         G-Channel
;; Ca-dep particles        Prob(Open) (0-1)         Conc. Integrator        G-Channel
;; Conc. Integrator   [Ca++], Shell 1 or 2 (mM)     I-Ca (soma/segment)  Ca-dep particles


;; The voltages (whether actual voltages or not) of the circuit nodes correspond to the
;; state variables of the circuit.  Note that branch elements (resistors, channels,
;; capacitances, synapses) do not represent the state variables per se; their values are
;; computed in terms of the appropriate node "voltages".

;; Presently there is some inconsistency in the way that data is referenced and stored
;; during a simulation.  Specifically, the voltage of any soma or segment (distal) node
;; that is saved and referenced in terms of the node object (the list referenced by the
;; accessor "node-voltage-data"), not the soma or segment object. On the other hand, the
;; "voltage" of a particle or concentration integrator (corresponding to particle state
;; or Ca++ concentration, respectively) is saved and referenced in terms of the element
;; object (the lists referenced by the accesors "particle-state-data",
;; "conc-part-state-data", "conc-int-concentration-1-data",
;; "conc-int-concentration-2-data"). Note that branch currents or branch element values
;; are referenced in terms of the appropriate branch element object (e.g.
;; "channel-current-data", "channel-conductance-data", "synapse-current-data",
;; "synapse-conductance-data", "isource-current-data").

;; Note that for the Hines method, "delta-v" structure slot values are actually V(t + dt/2).

;; *** Miscellaneous ***

;; The use of "core" structures (e.g. core-segments, core-nodes, etc.) is related to the
;; *Lisp implementation, in which each core structure is related to a single processor.
;; The intermittent use of these structures in the serial version is for compatibility
;; issues as SURF-HIPPO evolves.

;; The source code has not been completely cleaned up nor optimized. For example, many
;; apparently baroque function calls are deitrus from allowing the coexistence of several
;; simulator "versions" during the ongoing development.

;; Preliminary documentation on the Hines method is in segment.lisp.








