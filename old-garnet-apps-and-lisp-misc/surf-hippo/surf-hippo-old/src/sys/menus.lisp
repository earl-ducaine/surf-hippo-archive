;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-


;;;************* SOMA MENUS AND SETUP FUNCTIONS **************



;;;; MAIN-MENU Sets up all the parameters for the current run.
(defun main-menu ()
  (setq *modify-stimulus nil)
  (tv:choose-variable-values
    '((*modify-overall-parameters "Modify overall simulation parameters" :boolean)
      (*modify-drawing  "Modify histology drawing parameters" :boolean)
      (*modify-cell  "Modify cell membrane, synapse or stimulus parameters" :boolean)
      (*modify-cell-type  "Modify cell type parameters" :boolean)
      (*change-plotted-nodes  "Change plotted nodes?" :boolean)
      (*change-plotted-currents  "Change plotted currents?" :boolean)
      (*change-plotted-conductances  "Change plotted conductances?" :boolean)
;      (*specify-initial-node-voltages "Specify node initializations?" :boolean)
      (*modify-globals "Modify global variables?" :boolean))
    ':label "Main Simulation Menu"
      )
  (zl:cond-every
    (*modify-overall-parameters (overall-parameter-menu)(setq *modify-overall-parameters nil))
    (*modify-plot-parameters (plot-parameters-menu) (setq *modify-plot-parameters nil))
    (*modify-drawing (drawing-menu) (setq *modify-drawing nil))
    (*modify-cell  (cell-menu) (setq *modify-cell nil))
;    (*specify-initial-node-voltages      (setq *init-value-list* *old-init-value-list*))
    ))


;;; OVERALL-PARAMETER-MENU Set up the overall simulation parameters.
(defun overall-parameter-menu ()
  (tv:choose-variable-values
    '((*duration "Length of simulation [ms]" :number)
      (*modify-plot-parameters "Modify plot parameters" :boolean)
      (*clamp-type "Current or voltage clamp" :choose ("Current clamp" "Voltage clamp"))
      (*hard-copy-screen "Hardcopy screen after plotting?" :boolean)
      (*save-simulation "Save simulation results to file?" :boolean)
      (*short-print-circuit "Short print circuit after simulation?" :boolean))
    ':label "Setting up stimulus conditions for clamp"))

;;; PLOT-PARAMETERS-MENU Set up some plotting parameters.
(defun plot-parameters-menu ()
  (tv:choose-variable-values
    '((*plot-voltages-solid "Plot all the voltages in solid lines" :boolean)
      (*plot-results "Plot results?" :boolean)
      (*automatic-dendrite-voltage-plot-scaling "Automatic dendrite voltage plot scaling?" :boolean)
      (*automatic-soma-voltage-plot-scaling "Automatic soma voltage plot scaling?" :boolean)
;	(*plot-results-frame-2 "Plot results in Hippo frame 2?" :boolean)
;      (*distinct-plot "Plot each trace distinctly (takes more time)?" :boolean)
      (*overlay-simulations "Plot over previous data?" :boolean))
    ':label "Setting up plot parameters")
  (if *automatic-dendrite-voltage-plot-scaling
      (setq *dendrite-voltage-plot-min nil *dendrite-voltage-plot-max nil)
      (progn
	(zl:cond-every
	  ((not *dendrite-voltage-plot-min) (setq *dendrite-voltage-plot-min -90))
	  ((not *dendrite-voltage-plot-max) (setq *dendrite-voltage-plot-max -40)))
	(dendrite-plot-parameters-menu)))
    (if *automatic-soma-voltage-plot-scaling
      (setq *soma-voltage-plot-min nil *soma-voltage-plot-max nil)
      (progn
	(zl:cond-every
	  ((not *soma-voltage-plot-min) (setq *soma-voltage-plot-min -90))
	  ((not *soma-voltage-plot-max) (setq *soma-voltage-plot-max -40)))
	(soma-plot-parameters-menu))))

;;; DENDRITE-PLOT-PARAMETERS-MENU Set up some plotting parameters.
(defun dendrite-plot-parameters-menu ()
  (tv:choose-variable-values
    '((*dendrite-voltage-plot-min "*dendrite-voltage-plot-min [mV]" :number)
      (*dendrite-voltage-plot-max "*dendrite-voltage-plot-max [mV]" :number))
    ':label "Setting up dendrite voltage plot parameters"))

;;; SOMA-PLOT-PARAMETERS-MENU Set up some plotting parameters.
(defun soma-plot-parameters-menu ()
  (tv:choose-variable-values
    '((*soma-voltage-plot-min "*soma-voltage-plot-min [mV]" :number)
      (*soma-voltage-plot-max "*soma-voltage-plot-max [mV]" :number))
    ':label "Setting up soma voltage plot parameters"))

;;; CELL-MENU Sets up all the parameters for the current run.
(defun cell-menu ()
  (tv:choose-variable-values
    '((*modify-soma-currents "Modify the soma currents " :boolean)
      (*include-sources "Include current or voltage stimulus" :boolean)
      (*modify-stimulus "Modify current or voltage stimulus" :boolean)
      (*modify-synapses "Modify the synapses" :boolean)
      ))
  (zl:cond-every
    (*modify-soma-currents (surf-menu-for-soma-currents)(setq *modify-soma-currents nil))
    (*modify-synapses (menu-for-synapse-parameters ) (setq *modify-synapses nil))
  ))




;;;     MENU-FOR-SPOT-AND-ANNULUS-PARAMETERS
(defun MENU-FOR-SPOT-AND-ANNULUS-PARAMETERS ()
  (tv:choose-variable-values
    '((*light-start-position-x "Light stimulus position X" :number)
      (*light-start-position-y "Light stimulus position Y" :number)
      (*spot-outside-diameter "Spot or annulus outside diameter" :number)
      (*spot-inside-diameter "Annulus inside diameter" :number)
      (*light-input-offset-distance "Light input offset distance [microns]" :number)
      (*light-input-offset-angle-degrees "Light input offset angle [degrees]" :number)
      (*light-input-delay "Light input delay [milliseconds]" :number)
      )
    ':label "Setting up synapse parameters")
)


;;;     MENU-FOR-APPARENT-MOTION-PARAMETERS
(defun MENU-FOR-apparent-motion-PARAMETERS ()
  (setq *light-theta-degrees (truncate (* *light-theta (/ 1.0 pi) 0.5 360)))
  (setq *light-input-offset-angle-degrees (truncate (* *light-input-offset-angle (/ 1.0 pi) 0.5 360)))
  (tv:choose-variable-values
    '(
      (*bar-a-width "*bar-a-width [microns]" :number)
      (*bar-a-length "*bar-a-length [microns]" :number)
      (*bar-a-intensity "*bar-a-intensity" :number)
      (*bar-a-start-time "*bar-a-start-time [milliseconds]" :number)
      (*bar-a-stop-time "*bar-a-stop-time [milliseconds]" :number)
      (*bar-a-position-x "*bar-a-position-x [microns]" :number)
      (*bar-a-position-y "*bar-a-position-y [microns]" :number)

      (*bar-b-width "*bar-b-width [microns]" :number)
      (*bar-b-length "*bar-b-length [microns]" :number)
      (*bar-b-intensity "*bar-b-intensity" :number)
      (*bar-b-start-time "*bar-b-start-time [milliseconds]" :number)
      (*bar-b-stop-time "*bar-b-stop-time [milliseconds]" :number)
      (*bar-b-position-x "*bar-b-position-x [microns]" :number)
      (*bar-b-position-y "*bar-b-position-y [microns]" :number)

      (*light-input-offset-distance "Light input offset distance [microns]" :number)
      (*light-input-offset-angle-degrees "Light input offset angle [degrees]" :number)
      (*light-input-delay "Light input delay [milliseconds]" :number)
      )
    ':label "Setting Up Apparent Motion Parameters")
    (setq *light-theta (* *light-theta-degrees 2.0 pi-single (/ 1.0 360)))
  (setq *light-input-offset-angle (* *light-input-offset-angle-degrees  2.0 pi-single (/ 1.0 360))))

;;; MENU-FOR-moving-bar-grating-PARAMETERS
(defun menu-for-moving-bar-grating-parameters ()
  (setq *light-theta-degrees (truncate (* *light-theta (/ 1.0 pi) 0.5 360)))
  (setq *light-input-offset-angle-degrees (truncate (* *light-input-offset-angle (/ 1.0 pi) 0.5 360)))
  (tv:choose-variable-values
    '((*light-start-position-x "Light bar grating stimulus start position X" :number)
      (*light-start-position-y "Light bar grating stimulus start position Y" :number)
      (*bar-width "Bar width [microns]" :number)
      (*bar-length "Grating width [microns]" :number)
      (*light-theta-degrees "Orientation of grating bar long axis [degrees]" :number)
      (*light-speed "Speed of grating [microns per millisecond]" :number)
      (*light-input-offset-distance "Light input offset distance [microns]" :number)
      (*light-input-offset-angle-degrees "Light input offset angle [degrees]" :number)
      (*light-input-delay "Light input delay [milliseconds]" :number)
      (*light-direction  "Movement direction 90 degrees ahead (yes) / behind (no) of bar long axis" :boolean)
      (*motion-start-time "Time to start movement" :number)
      (*motion-stop-time "Time to stop movement" :number)
      (*grating-spatial-period "Spatial period of grating [microns]" :number)
      )
    ':label "Setting up synapse parameters")
  (setq *light-theta (* *light-theta-degrees 2.0 pi-single (/ 1.0 360)))
  (setq *light-input-offset-angle (* *light-input-offset-angle-degrees  2.0 pi-single (/ 1.0 360))))



;;; MENU-FOR-moving-bar-PARAMETERS
(defun menu-for-moving-bar-parameters ()
  (setq *light-theta-degrees (truncate (* *light-theta (/ 1.0 pi) 0.5 360)))
  (setq *light-input-offset-angle-degrees (truncate (* *light-input-offset-angle (/ 1.0 pi) 0.5 360)))
  (tv:choose-variable-values
    '((*light-start-position-x "Light bar stimulus start position X" :number)
      (*light-start-position-y "Light bar stimulus start position Y" :number)
      (*bar-width "Bar width [microns]" :number)
      (*bar-length "Bar length [microns]" :number)
      (*light-theta-degrees "Bar or grating theta [degrees] (orientation of long axis)" :number)
      (*light-speed "Speed of bar [microns per millisecond]" :number)
      (*light-input-offset-distance "Light input offset distance [microns]" :number)
      (*light-input-offset-angle-degrees "Light input offset angle [degrees]" :number)
      (*light-input-delay "Light input delay [milliseconds]" :number)
      (*light-direction  "Movement direction 90 degrees ahead (yes) / behind (no) of long axis" :boolean)
      (*motion-start-time "Time to start movement" :number)
      (*motion-stop-time "Time to stop movement" :number)
      )
    ':label "Setting up synapse parameters")
  (setq *light-theta (* *light-theta-degrees 2.0 pi-single (/ 1.0 360)))
  (setq *light-input-offset-angle (* *light-input-offset-angle-degrees  2.0 pi-single (/ 1.0 360))))


;;; MENU-FOR-SYNAPSE-PARAMETERS
(defun menu-for-synapse-parameters ()
  (setq *light-theta-degrees (truncate (* *light-theta (/ 1.0 pi) 0.5 360)))
  (setq *light-input-offset-angle-degrees (truncate (* *light-input-offset-angle (/ 1.0 pi) 0.5 360)))
  (tv:choose-variable-values
    '((*light-stimulus "Light stimulus type"
		       :choose (moving-spot annulus spot moving-bar moving-bar-grating
					    moving-sine-grating apparent-motion ;reversing-bar
					    ))
      (*light-stimulus-start-time "Light stimulus start-time" :number)
      (*light-stimulus-stop-time "Light stimulus stop-time" :number)
      (*light-start-position-x "Light stimulus start center X" :number)
      (*light-start-position-y "Light stimulus start center Y" :number)
      (*light-input-offset-distance "Light input offset distance [microns]" :number)
      (*light-input-offset-angle-degrees "Light input offset angle [degrees]" :number)
      (*light-input-delay "Light input delay [milliseconds]" :number)
      (*use-aperture "Use aperture?" :boolean)
      (*aperture-radius "Aperture radius [microns]" :number)
      (*aperture-center-x "Aperture center-x [microns]" :number)
      (*aperture-center-y "Aperture center-y [microns]" :number)
;      (*g-bar-excitatory-1 "*g-bar-excitatory-1" :number)
;      (*g-bar-inhibitory-1 "*g-bar-inhibitory-1" :number)
      (*g-excitatory-1-dens "*g-excitatory-1-dens [pS per sq uM]" :number)
      (*g-inhibitory-1-dens "*g-inhibitory-1-dens [pS per sq uM]" :number)
      (*e-rev-excitatory-1 "*e-rev-excitatory-1" :number)
      (*e-rev-inhibitory-1 "*e-rev-inhibitory-1" :number)
      (*synapse-g-leak-ratio  "Ratio between synapse cond and g-leak" :number )
      (*light-stimulus-strength "Light stimulus strength" :number)
      (*use-old-synapse-waveforms "Use last synapse waveforms?" :boolean)
      (*include-light-synapses "Include light inputs?" :boolean)

      )
    ':label "Setting up synapse parameters")
;    (setq *g-ex-mem (/ 1.0 *r-ex-mem)
;	  *g-in-mem (/ 1.0 *r-in-mem))
  (setq *light-theta (* *light-theta-degrees 2.0 pi-single (/ 1.0 360)))
  (setq *light-input-offset-angle (* *light-input-offset-angle-degrees  2.0 pi-single (/ 1.0 360)))
  (cond  ((eq *light-stimulus 'reversing-bar)
		       ())
		      ((eq *light-stimulus 'apparent-motion)
		       (menu-for-apparent-motion-parameters))
		      ((eq *light-stimulus 'moving-bar)
		       (menu-for-moving-bar-parameters))
		      ((eq *light-stimulus 'moving-bar-grating)
		       (menu-for-moving-bar-grating-parameters))
		      ((eq *light-stimulus 'moving-sine-grating)
		       (menu-for-moving-sine-grating-parameters))
		      ((eq *light-stimulus 'moving-spot)
		       (menu-for-moving-spot-parameters))
		      ((or (eq *light-stimulus 'annulus) (eq *light-stimulus 'spot))
		       (menu-for-spot-and-annulus-parameters))))

(defun menu-for-moving-sine-grating-parameters ())
(defun menu-for-moving-spot-parameters ())


;	(*r-ex-mem "Excitatory resistance [ohms/cm^2]" :number)
;	(*r-in-mem "Inhibitory resistance [ohms/cm^2]" :number)	;
;	(*distal-direction "Motion away from soma?" :boolean)
;	(*tau-alpha-ex "Excitatory alpha tau [ms]" :number)
;	(*tau-alpha-in "Inhibitory alpha tau [ms]" :number)
;	(*synapse-weight-offset "Synapse-weight-offset" :number)
;	(*synapse-weight-slope "Synapse-weight-slope [microns]" :number)
;	(*synapse-cable-length "synapse-cable-length" :number)
;	(*synapse-cable-diameter "synapse-cable-diameter" :number)
;	(*sp "synapse spacing" :number)
;	(*soma-radius "Soma radius [microns]" :number)
;	(*lambda-factor  "lambda-factor " :number)


;;; SURF-MENU-FOR-SOMA-CURRENTS Sets up all the soma currents for the current run.
(defun surf-menu-for-soma-currents ()
  (let ((update-flag nil)
	(list1 (list (list 'Na1  "Na1 (trigger mutha) current"
			   (list (list :include  *include-na1) :modify))
		     (list 'Na2  "Na2 (slow tail) current"
			   (list (list :include  *include-na2) :modify))
		     (list 'Na3  "Na3 (repetitive) current"
			   (list (list :include  *include-na3) :modify))
		     (list 'ca   "Ca current" (list (list :include  *include-ca) :modify))
;		     (list 'cas   "Slow Ca current" (list (list :include  *include-cas) :modify))
		     (list 'dr   "DR current" (list (list :include  *include-dr) :modify))
;		     (list 'c   "C current" (list (list :include  *include-c) :modify))
;		     (list 'ahp   "Ahp current" (list (list :include  *include-ahp) :modify))
;		     (list 'm   "M current" (list (list :include  *include-m) :modify))	
;		     (list 'q   "Q current" (list (list :include  *include-q) :modify))	
		     (list 'a   "A current" (list (list :include  *include-a) :modify))
		     )))
    (let ((result (tv:multiple-choose "Pyramidal Currents" list1
				      '((:include "Include" nil nil nil (:modify))
					(:modify "Modify" (:include) nil)))))
      (loop for item in result
	    do (progn (if (not (zl:memq :include item))
			  (zl:selectq (car item)
			    (Na1 (setq *include-na1 nil))
			    (Na2 (setq *include-na2 nil))
			    (Na3 (setq *include-na3 nil))
			    (Ca (setq *include-Ca nil))
;			    (Cas (setq *include-Cas nil))
			    (DR (setq *include-DR nil))
;			    (C (setq *include-C nil))
;			    (AHP (setq *include-AHP nil)) (M (setq *include-M nil))
;			    (Q (setq *include-Q nil))
			    (A (setq *include-A nil))
			    ))
		      (if (zl:memq :modify item)
			  (zl:selectq (car item)
			    (Na1 (and (setq update-flag t *na1-mod t)(menu-for-Na1-current)))
			    (Na2 (and (setq update-flag t *na2-mod t)(menu-for-Na2-current)))
			    (Na3 (and (setq update-flag t *na3-mod t)(menu-for-Na3-current)))
;			    (Ca (and (setq update-flag t *ca-mod t)(menu-for-Ca-current)))
;;			  (Cas (and (setq update-flag t) (menu-for-Cas-current)))
			    (DR (and (setq update-flag t *dr-mod t)(menu-for-DR-current)))
;			    (C (and (setq update-flag t *c-mod t)(menu-for-C-current)))
;			  (AHP (and (setq update-flag t *ahp-mod t)(menu-for-AHP-current)))
;;			  (M (and (setq update-flag t)(menu-for-M-current)))
;			    (Q (and (setq update-flag t *q-mod t)(menu-for-Q-current)))
			    (A (and (setq update-flag t *a-mod t)(menu-for-A-current)))
			    ))
		      (if (zl:memq :include item)
			  (zl:selectq (car item)
			    (Na1 (setq *include-na1 T))
			    (Na2 (setq *include-na2 T))(Na3 (setq *include-na3 T))
			    (Ca (setq *include-Ca T))
;			    (Cas (setq *include-Cas T))
			    (DR (setq *include-DR T))
;			    (C (setq *include-C T))
;			    (AHP (setq *include-AHP T)) (M (setq *include-M T))
;			    (Q (setq *include-Q T))
			    (A (setq *include-A T))
			    )))))
;    (update-gbars)
    ))




;;; MENU-FOR-ISOURCE-VALUES
(defun menu-for-isource-values  (name)
  (tv:choose-variable-values
    '((*i-stim-1 "Step 1 amplitude [na]" :number)
      (*t-stim-1 "       For how long [ms]" :number)
      (*i-stim-2 "Step 2 amplitude [na]" :number)
      (*t-stim-2 "       For how long [ms]" :number)
      (*i-stim-3 "Step 3 amplitude [na]"  :number)
      (*t-stim-3 "       For how long [ms]" :number)
      (*i-stim-4 "Step 4 amplitude [na]" :number)
      (*t-stim-4 "       For how long [ms]" :number)
      (*i-stim-5 "Step 5 amplitude [na] " :number)
      (*duration "For how long (this will change the duration of the simulation)[ms]" :number))
    ':label (format nil "Setting Up Current Source ~a" name))
  (setf user-stop-time *duration))

;;; MENU-FOR-VSOURCE-VALUES
(defun menu-for-vsource-values  (name)
  (tv:choose-variable-values
    '((*i-stim-1 "Step 1 amplitude [mV]" :number)
      (*t-stim-1 "       For how long [ms]" :number)
      (*i-stim-2 "Step 2 amplitude [mV]" :number)
      (*t-stim-2 "       For how long [ms]" :number)
      (*i-stim-3 "Step 3 amplitude [mV]"  :number)
      (*t-stim-3 "       For how long [ms]" :number)
      (*i-stim-4 "Step 4 amplitude [mV]" :number)
      (*t-stim-4 "       For how long [ms]" :number)
      (*i-stim-5 "Step 5 amplitude [mV] " :number)
      (*duration "For how long (this will change the duration of the simulation)[ms]" :number))
    ':label (format nil "Setting Up Voltage Source ~a" name))
  (setf user-stop-time *duration))


;;; GLOBALS-MENU This allows changing of some global flags, such as for debugging.
(defun globals-menu ()
      (tv:choose-variable-values
	'((*debug-partition*  :boolean)
	  (*debug-time-trace*  :boolean)
	  (*debug-at-time-steps*  :boolean)
	  (*debug-all-iterations*  :boolean)
	  (*print-matrix*  :boolean)
	  (*debug-dc*  :boolean)
	  (*use-tridiagonal* :boolean)
	  (*model-ca-variation* :boolean)
	  (*pseudo-transient-requested* :boolean)
	  (*dc-solution-computed* :boolean)
	  (cmin :number)
	  (vabs :number)
	  (vrel :number)
	  (iabs :number)
	  (irel :number)
	  (max-voltage-step :number)
	  (lteabs :number)
	  (lterel :number)
	  (up-step-ratio :number)
	  (down-step-ratio :number)
	  (user-min-step :number)
	  (user-max-step :number)
	  (*max-num-relax-iterations* :integer)
	  (*iters-before-sor* :integer)
	  (*under-relax-factor* :number)
	  (*over-relax-factor* :number)
	  )
	':label "Setting Up Simulator Parameters"))